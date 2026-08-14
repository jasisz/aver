use super::{ReturnControl, VM};
use crate::nan_value::{AllocSpace, NanValue};
use crate::vm::opcode::{RETURN, TAIL_CALL_KNOWN, TAIL_CALL_SELF};
use crate::vm::profile::ReturnPathProfileKind;
use crate::vm::types::CallFrame;

impl VM {
    fn result_uses_frame_local_heap(&self, frame: &CallFrame, result: NanValue) -> bool {
        result.heap_index().is_some_and(|index| {
            self.arena.is_frame_local_index(
                index,
                frame.arena_mark,
                frame.yard_mark,
                frame.handoff_mark,
            )
        })
    }

    /// Drop the frame's young region without rewriting anything.
    ///
    /// The result handle test alone is not enough. It says the handle we hand
    /// back does not point into the region about to go, but says nothing about
    /// what that value CONTAINS — and an owned in-place vector write can leave
    /// a vector below the marks holding a value allocated above them. The
    /// frame's `inplace_write_escaped` flag is what reports that; while it is
    /// set the frame must go the long way round, where the references are
    /// rewritten before anything is dropped.
    fn can_fast_return_with_young_truncate(&self, frame: &CallFrame, result: NanValue) -> bool {
        !frame.globals_dirty
            && !frame.yard_dirty
            && !frame.handoff_dirty
            && !frame.inplace_write_escaped
            && self.arena.yard_len() == frame.yard_mark as usize
            && self.arena.handoff_len() == frame.handoff_mark as usize
            && self.arena.young_len() > frame.arena_mark as usize
            && !self.result_uses_frame_local_heap(frame, result)
    }

    pub(super) fn collect_stable_roots(&mut self, frame_roots: &mut [NanValue]) {
        let root_count = frame_roots.len();
        let global_count = self.globals.len();
        let constant_count: usize = self
            .code
            .functions
            .iter()
            .map(|chunk| chunk.constants.len())
            .sum();
        let mut all_roots = Vec::with_capacity(root_count + global_count + constant_count);
        all_roots.extend_from_slice(frame_roots);
        all_roots.extend(self.globals.iter().copied());
        for chunk in &self.code.functions {
            all_roots.extend(chunk.constants.iter().copied());
        }
        self.arena.collect_stable_from_roots(&mut all_roots);

        frame_roots.copy_from_slice(&all_roots[..root_count]);
        for (dst, src) in self.globals.iter_mut().zip(
            all_roots[root_count..root_count + global_count]
                .iter()
                .copied(),
        ) {
            *dst = src;
        }
        let mut constant_offset = root_count + global_count;
        for chunk in &mut self.code.functions {
            let len = chunk.constants.len();
            chunk
                .constants
                .copy_from_slice(&all_roots[constant_offset..constant_offset + len]);
            constant_offset += len;
        }
    }

    /// Finish the frame's regions before the tail call reuses it.
    // One over the limit, and the one over is `inplace_write_escaped`, which
    // the evacuation below needs.
    #[allow(clippy::too_many_arguments)]
    pub(super) fn finalize_frame_locals_for_tail_call(
        &mut self,
        arena_mark: u32,
        yard_mark: u32,
        handoff_mark: u32,
        globals_dirty: bool,
        yard_dirty: bool,
        inplace_write_escaped: bool,
        frame_roots: &mut [NanValue],
    ) {
        let _ = yard_dirty;
        if globals_dirty {
            self.arena.promote_roots_to_stable(&mut self.globals);
        }

        let has_local_young = self.arena.young_len() > arena_mark as usize;
        let has_local_yard = self.arena.yard_len() > yard_mark as usize;
        let has_local_handoff = self.arena.handoff_len() > handoff_mark as usize;

        if has_local_yard || has_local_handoff {
            self.arena.evacuate_frame_to_yard(
                arena_mark,
                yard_mark,
                handoff_mark,
                frame_roots,
                inplace_write_escaped,
            );
            return;
        }

        if has_local_young {
            // Small young growth (≤4 entries) means the iteration appended a
            // few objects (e.g. one cons cell) without significant temporaries.
            // Skip promotion — the entries stay in young, protected by
            // arena_mark.  The next return boundary will evacuate them.
            let young_growth = self.arena.young_len() - arena_mark as usize;
            if young_growth > 4 {
                self.arena
                    .promote_young_roots_to_yard(arena_mark, frame_roots);
            }
        }
    }

    pub(super) fn finalize_frame_return_to_caller(
        &mut self,
        arena_mark: u32,
        yard_base: u32,
        handoff_mark: u32,
        globals_dirty: bool,
        inplace_write_escaped: bool,
        frame_roots: &mut [NanValue],
    ) -> (bool, bool) {
        if globals_dirty {
            self.arena.promote_roots_to_stable(&mut self.globals);
        }
        let has_local_young = self.arena.young_len() > arena_mark as usize;
        let has_local_yard = self.arena.yard_len() > yard_base as usize;
        let has_local_handoff = self.arena.handoff_len() > handoff_mark as usize;
        let handoff_growth = self
            .arena
            .handoff_len()
            .saturating_sub(handoff_mark as usize);
        let result_is_single_local_handoff = matches!(
            frame_roots,
            [result]
                if result
                    .heap_index()
                    .is_some_and(|index| self.arena.is_handoff_index_in_region(index, handoff_mark))
                    && handoff_growth == 1
        );

        // Ordinary helper returns often build their final value directly in
        // handoff. We can keep that suffix in place cheaply when the frame is
        // "pure handoff" or "pure young". Mixed young+handoff returns still
        // go through full evacuation so nested refs stay correct.
        if !has_local_yard && !has_local_young {
            self.arena.truncate_to(arena_mark);
            self.arena.truncate_yard_to(yard_base);
            return (false, has_local_handoff);
        }

        if !has_local_yard && !has_local_handoff {
            self.arena
                .promote_young_roots_to_handoff(arena_mark, frame_roots);
            self.arena.truncate_yard_to(yard_base);
            return (false, self.arena.handoff_len() > handoff_mark as usize);
        }

        if !has_local_yard && has_local_young && result_is_single_local_handoff {
            self.arena
                .promote_young_roots_to_handoff(arena_mark, frame_roots);
            self.arena.truncate_yard_to(yard_base);
            return (false, self.arena.handoff_len() > handoff_mark as usize);
        }

        if has_local_young || has_local_yard || has_local_handoff {
            return self.arena.evacuate_frame_to_handoff(
                arena_mark,
                yard_base,
                handoff_mark,
                frame_roots,
                inplace_write_escaped,
            );
        }

        self.arena.truncate_to(arena_mark);
        self.arena.truncate_yard_to(yard_base);
        self.arena.truncate_handoff_to(handoff_mark);
        (false, false)
    }

    pub(super) fn finalize_frame_return(
        &mut self,
        arena_mark: u32,
        yard_base: u32,
        handoff_mark: u32,
        globals_dirty: bool,
        frame_roots: &mut [NanValue],
    ) {
        if globals_dirty {
            self.arena.promote_roots_to_stable(&mut self.globals);
        }
        self.arena.promote_roots_to_stable(frame_roots);
        self.arena.truncate_to(arena_mark);
        self.arena.truncate_yard_to(yard_base);
        self.arena.truncate_handoff_to(handoff_mark);
    }

    pub(super) fn finalize_parent_thin_return_to_caller(
        &mut self,
        arena_mark: u32,
        yard_base: u32,
        handoff_mark: u32,
        globals_dirty: bool,
        inplace_write_escaped: bool,
        frame_roots: &mut [NanValue],
    ) -> (bool, bool) {
        if globals_dirty {
            self.arena.promote_roots_to_stable(&mut self.globals);
        }

        let has_local_young = self.arena.young_len() > arena_mark as usize;
        let has_local_yard = self.arena.yard_len() > yard_base as usize;
        let has_local_handoff = self.arena.handoff_len() > handoff_mark as usize;

        if has_local_yard || has_local_handoff {
            return self.finalize_frame_return_to_caller(
                arena_mark,
                yard_base,
                handoff_mark,
                globals_dirty,
                inplace_write_escaped,
                frame_roots,
            );
        }

        // Parent-thin frames borrow the caller young lane on purpose.
        // If they stayed out of yard/handoff, their local young scratch can
        // simply remain in the caller region and die at the caller boundary.
        let _ = has_local_young;
        self.arena.truncate_yard_to(yard_base);
        self.arena.truncate_handoff_to(handoff_mark);
        (false, false)
    }

    pub(super) fn next_value_alloc_space(&self, code: &[u8], ip: usize) -> AllocSpace {
        if matches!(code.get(ip).copied(), Some(op) if op == TAIL_CALL_SELF || op == TAIL_CALL_KNOWN)
        {
            AllocSpace::Yard
        } else if self.frames.last().is_some_and(|frame| frame.parent_thin) {
            AllocSpace::Young
        } else if matches!(code.get(ip).copied(), Some(op) if op == RETURN) && self.frames.len() > 1
        {
            AllocSpace::Handoff
        } else {
            AllocSpace::Young
        }
    }

    /// Return without touching the arena at all.
    ///
    /// `inplace_write_escaped` deliberately does not appear here. This path
    /// drops nothing — no truncate, no compaction — so a reference into this
    /// frame's regions cannot go stale at this boundary whatever the frame
    /// wrote. What it also does not do is REPAIR one, so the obligation simply
    /// outlives the frame; `complete_frame_return` hands the flag to the caller
    /// so the boundary that does drop something still knows about it.
    pub(super) fn can_fast_return(&self, frame: &CallFrame) -> bool {
        if frame.parent_thin {
            // Parent-thin frames are allowed to grow the borrowed young lane;
            // the key fast-path condition is "no separate survivor lane work".
            return !frame.globals_dirty
                && !frame.yard_dirty
                && !frame.handoff_dirty
                && self.arena.yard_len() == frame.yard_mark as usize
                && self.arena.handoff_len() == frame.handoff_mark as usize;
        }

        frame.thin
            && !frame.globals_dirty
            && !frame.yard_dirty
            && !frame.handoff_dirty
            && self.arena.young_len() == frame.arena_mark as usize
            && self.arena.yard_len() == frame.yard_mark as usize
            && self.arena.handoff_len() == frame.handoff_mark as usize
    }

    pub(super) fn complete_frame_return(
        &mut self,
        frame: CallFrame,
        mut result: NanValue,
        caller_depth: usize,
    ) -> ReturnControl {
        // An escaping in-place write is an obligation on whichever boundary
        // eventually drops the region the written value lives in, and this
        // frame's boundary is not always that one. Hand it up unconditionally
        // rather than trying to work out which paths discharge it: where a
        // boundary really did rewrite the references, it already reports
        // `yard_dirty` / `handoff_dirty` to the caller, which withholds the
        // same fast return — so the inherited flag costs nothing there and is
        // load-bearing exactly where nothing else was reported.
        if frame.inplace_write_escaped
            && let Some(caller) = self.frames.last_mut()
        {
            caller.inplace_write_escaped = true;
        }

        if self.can_fast_return(&frame) {
            if let Some(profile) = self.profile.as_mut() {
                profile.record_return_path(&frame, ReturnPathProfileKind::Fast);
            }
            if self.frames.len() == caller_depth {
                return ReturnControl::Done(result);
            }

            let caller = self.frames.last().unwrap();
            return ReturnControl::Resume {
                result,
                fn_id: caller.fn_id,
                ip: caller.ip as usize,
                bp: caller.bp as usize,
            };
        }

        if self.can_fast_return_with_young_truncate(&frame, result) {
            if let Some(profile) = self.profile.as_mut() {
                profile.record_return_path(&frame, ReturnPathProfileKind::YoungTruncateFast);
            }
            self.arena.truncate_to(frame.arena_mark);
            if self.frames.len() == caller_depth {
                return ReturnControl::Done(result);
            }

            let caller = self.frames.last().unwrap();
            return ReturnControl::Resume {
                result,
                fn_id: caller.fn_id,
                ip: caller.ip as usize,
                bp: caller.bp as usize,
            };
        }

        if let Some(profile) = self.profile.as_mut() {
            profile.record_return_path(&frame, ReturnPathProfileKind::Slow);
        }

        if self.frames.len() == caller_depth {
            // No flag to pass: this path promotes the roots to stable, which
            // takes every space wholesale rather than by region, so an
            // in-place write into an older slot is carried along with the rest.
            self.finalize_frame_return(
                frame.arena_mark,
                frame.yard_base,
                frame.handoff_mark,
                frame.globals_dirty,
                std::slice::from_mut(&mut result),
            );
            if caller_depth == 0 {
                self.collect_stable_roots(std::slice::from_mut(&mut result));
            }
            return ReturnControl::Done(result);
        }

        if frame.parent_thin {
            let (yard_dirty, handoff_dirty) = self.finalize_parent_thin_return_to_caller(
                frame.arena_mark,
                frame.yard_base,
                frame.handoff_mark,
                frame.globals_dirty,
                frame.inplace_write_escaped,
                std::slice::from_mut(&mut result),
            );
            let caller = self.frames.last_mut().unwrap();
            caller.yard_dirty |= yard_dirty;
            caller.handoff_dirty |= handoff_dirty;
            return ReturnControl::Resume {
                result,
                fn_id: caller.fn_id,
                ip: caller.ip as usize,
                bp: caller.bp as usize,
            };
        }

        let (yard_dirty, handoff_dirty) = self.finalize_frame_return_to_caller(
            frame.arena_mark,
            frame.yard_base,
            frame.handoff_mark,
            frame.globals_dirty,
            frame.inplace_write_escaped,
            std::slice::from_mut(&mut result),
        );
        let caller = self.frames.last_mut().unwrap();
        caller.yard_dirty |= yard_dirty;
        caller.handoff_dirty |= handoff_dirty;
        ReturnControl::Resume {
            result,
            fn_id: caller.fn_id,
            ip: caller.ip as usize,
            bp: caller.bp as usize,
        }
    }
}

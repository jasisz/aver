use super::{ReturnControl, VM};
use crate::nan_value::{AllocSpace, NanValue};
use crate::vm::opcode::{RETURN, TAIL_CALL_KNOWN, TAIL_CALL_SELF};
use crate::vm::types::CallFrame;

impl VM {
    pub(super) fn collect_stable_roots(&mut self, frame_roots: &mut [NanValue]) {
        let root_count = frame_roots.len();
        let mut all_roots = Vec::with_capacity(root_count + self.globals.len());
        all_roots.extend_from_slice(frame_roots);
        all_roots.extend(self.globals.iter().copied());
        self.arena.collect_stable_from_roots(&mut all_roots);

        frame_roots.copy_from_slice(&all_roots[..root_count]);
        for (dst, src) in self
            .globals
            .iter_mut()
            .zip(all_roots[root_count..].iter().copied())
        {
            *dst = src;
        }
    }

    pub(super) fn finalize_frame_locals_for_tail_call(
        &mut self,
        arena_mark: u32,
        yard_mark: u32,
        handoff_mark: u32,
        globals_dirty: bool,
        yard_dirty: bool,
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
            self.arena
                .evacuate_frame_to_yard(arena_mark, yard_mark, handoff_mark, frame_roots);
            return;
        }

        if has_local_young {
            self.arena
                .promote_young_roots_to_yard(arena_mark, frame_roots);
        }
    }

    pub(super) fn finalize_frame_return_to_caller(
        &mut self,
        arena_mark: u32,
        yard_base: u32,
        handoff_mark: u32,
        globals_dirty: bool,
        frame_roots: &mut [NanValue],
    ) -> (bool, bool) {
        if globals_dirty {
            self.arena.promote_roots_to_stable(&mut self.globals);
        }
        self.arena.promote_roots_to_stable(frame_roots);
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

    pub(super) fn next_value_alloc_space(&self, code: &[u8], ip: usize) -> AllocSpace {
        if matches!(code.get(ip).copied(), Some(op) if op == TAIL_CALL_SELF || op == TAIL_CALL_KNOWN)
        {
            AllocSpace::Yard
        } else if matches!(code.get(ip).copied(), Some(op) if op == RETURN) && self.frames.len() > 1
        {
            AllocSpace::Handoff
        } else {
            AllocSpace::Young
        }
    }

    pub(super) fn can_fast_return(&self, frame: &CallFrame) -> bool {
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
        if self.can_fast_return(&frame) {
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

        if self.frames.len() == caller_depth {
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

        let (yard_dirty, handoff_dirty) = self.finalize_frame_return_to_caller(
            frame.arena_mark,
            frame.yard_base,
            frame.handoff_mark,
            frame.globals_dirty,
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

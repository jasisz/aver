use std::collections::BTreeMap;

use super::opcode::opcode_name;
use super::types::{CallFrame, CodeStore};

#[derive(Debug, Clone, Default)]
pub struct VmReturnStats {
    pub total_entries: u64,
    pub thin_entries: u64,
    pub parent_thin_entries: u64,
    pub thin_fast_returns: u64,
    pub parent_thin_fast_returns: u64,
    pub young_truncate_fast_returns: u64,
    pub thin_slow_returns: u64,
    pub parent_thin_slow_returns: u64,
    pub regular_slow_returns: u64,
}

#[derive(Debug, Clone)]
pub struct VmFunctionProfile {
    pub name: String,
    pub thin: bool,
    pub parent_thin: bool,
    pub entries: u64,
    pub fast_returns: u64,
    pub young_truncate_fast_returns: u64,
    pub slow_returns: u64,
}

#[derive(Debug, Clone)]
pub struct VmOpcodeProfile {
    pub opcode: u8,
    pub name: &'static str,
    pub count: u64,
}

#[derive(Debug, Clone)]
pub struct VmBuiltinProfile {
    pub name: String,
    pub count: u64,
}

#[derive(Debug, Clone)]
pub struct VmProfileReport {
    pub total_opcodes: u64,
    pub opcodes: Vec<VmOpcodeProfile>,
    pub functions: Vec<VmFunctionProfile>,
    pub builtins: Vec<VmBuiltinProfile>,
    pub returns: VmReturnStats,
}

impl VmProfileReport {
    pub fn merge(&mut self, other: &Self) {
        assert_eq!(
            self.opcodes.len(),
            other.opcodes.len(),
            "opcode profile shapes must match"
        );
        assert_eq!(
            self.functions.len(),
            other.functions.len(),
            "function profile shapes must match"
        );

        self.total_opcodes += other.total_opcodes;
        for (dst, src) in self.opcodes.iter_mut().zip(other.opcodes.iter()) {
            assert_eq!(dst.opcode, src.opcode, "opcode profile ordering must match");
            dst.count += src.count;
        }
        for (dst, src) in self.functions.iter_mut().zip(other.functions.iter()) {
            assert_eq!(dst.name, src.name, "function profile ordering must match");
            dst.entries += src.entries;
            dst.fast_returns += src.fast_returns;
            dst.young_truncate_fast_returns += src.young_truncate_fast_returns;
            dst.slow_returns += src.slow_returns;
        }
        for src in &other.builtins {
            if let Some(dst) = self.builtins.iter_mut().find(|dst| dst.name == src.name) {
                dst.count += src.count;
            } else {
                self.builtins.push(src.clone());
            }
        }
        self.builtins
            .sort_by(|a, b| b.count.cmp(&a.count).then_with(|| a.name.cmp(&b.name)));

        self.returns.total_entries += other.returns.total_entries;
        self.returns.thin_entries += other.returns.thin_entries;
        self.returns.parent_thin_entries += other.returns.parent_thin_entries;
        self.returns.thin_fast_returns += other.returns.thin_fast_returns;
        self.returns.parent_thin_fast_returns += other.returns.parent_thin_fast_returns;
        self.returns.young_truncate_fast_returns += other.returns.young_truncate_fast_returns;
        self.returns.thin_slow_returns += other.returns.thin_slow_returns;
        self.returns.parent_thin_slow_returns += other.returns.parent_thin_slow_returns;
        self.returns.regular_slow_returns += other.returns.regular_slow_returns;
    }
}

#[derive(Debug, Clone, Copy)]
pub(crate) enum ReturnPathProfileKind {
    Fast,
    YoungTruncateFast,
    Slow,
}

#[derive(Debug, Clone)]
pub(crate) struct VmProfileState {
    opcode_counts: [u64; 256],
    prev_opcode: u8,
    bigram_counts: BTreeMap<(u8, u8), u64>,
    function_entries: Vec<u64>,
    function_fast_returns: Vec<u64>,
    function_young_truncate_fast_returns: Vec<u64>,
    function_slow_returns: Vec<u64>,
    builtin_calls: BTreeMap<String, u64>,
    return_stats: VmReturnStats,
}

impl VmProfileState {
    pub(crate) fn new(function_count: usize) -> Self {
        Self {
            opcode_counts: [0; 256],
            prev_opcode: 0xFF,
            bigram_counts: BTreeMap::new(),
            function_entries: vec![0; function_count],
            function_fast_returns: vec![0; function_count],
            function_young_truncate_fast_returns: vec![0; function_count],
            function_slow_returns: vec![0; function_count],
            builtin_calls: BTreeMap::new(),
            return_stats: VmReturnStats::default(),
        }
    }

    pub(crate) fn record_opcode(&mut self, opcode: u8) {
        self.opcode_counts[opcode as usize] += 1;
        if self.prev_opcode != 0xFF {
            *self
                .bigram_counts
                .entry((self.prev_opcode, opcode))
                .or_insert(0) += 1;
        }
        self.prev_opcode = opcode;
    }

    pub(crate) fn top_bigrams(&self, n: usize) -> Vec<((u8, u8), u64)> {
        let mut pairs: Vec<_> = self.bigram_counts.iter().map(|(&k, &v)| (k, v)).collect();
        pairs.sort_by_key(|b| std::cmp::Reverse(b.1));
        pairs.truncate(n);
        pairs
    }

    pub(crate) fn record_function_entry(&mut self, chunk: &super::types::FnChunk, fn_id: u32) {
        let fn_idx = fn_id as usize;
        self.function_entries[fn_idx] += 1;
        self.return_stats.total_entries += 1;
        if chunk.parent_thin {
            self.return_stats.parent_thin_entries += 1;
        } else if chunk.thin {
            self.return_stats.thin_entries += 1;
        }
    }

    pub(crate) fn record_builtin_call(&mut self, name: &str) {
        *self.builtin_calls.entry(name.to_string()).or_insert(0) += 1;
    }

    pub(crate) fn record_return_path(&mut self, frame: &CallFrame, kind: ReturnPathProfileKind) {
        let fn_idx = frame.fn_id as usize;
        match kind {
            ReturnPathProfileKind::Fast => {
                self.function_fast_returns[fn_idx] += 1;
                if frame.parent_thin {
                    self.return_stats.parent_thin_fast_returns += 1;
                } else if frame.thin {
                    self.return_stats.thin_fast_returns += 1;
                }
            }
            ReturnPathProfileKind::YoungTruncateFast => {
                self.function_young_truncate_fast_returns[fn_idx] += 1;
                self.return_stats.young_truncate_fast_returns += 1;
            }
            ReturnPathProfileKind::Slow => {
                self.function_slow_returns[fn_idx] += 1;
                if frame.parent_thin {
                    self.return_stats.parent_thin_slow_returns += 1;
                } else if frame.thin {
                    self.return_stats.thin_slow_returns += 1;
                } else {
                    self.return_stats.regular_slow_returns += 1;
                }
            }
        }
    }

    pub(crate) fn report(&self, code: &CodeStore) -> VmProfileReport {
        let total_opcodes = self.opcode_counts.iter().sum();
        let mut opcodes = self
            .opcode_counts
            .iter()
            .enumerate()
            .filter_map(|(opcode, count)| {
                (*count > 0).then_some(VmOpcodeProfile {
                    opcode: opcode as u8,
                    name: opcode_name(opcode as u8),
                    count: *count,
                })
            })
            .collect::<Vec<_>>();
        opcodes.sort_by(|a, b| b.count.cmp(&a.count).then_with(|| a.opcode.cmp(&b.opcode)));

        let mut functions = code
            .functions
            .iter()
            .enumerate()
            .filter_map(|(idx, chunk)| {
                let entries = self.function_entries[idx];
                let fast_returns = self.function_fast_returns[idx];
                let young_truncate_fast_returns = self.function_young_truncate_fast_returns[idx];
                let slow_returns = self.function_slow_returns[idx];
                (entries > 0
                    || fast_returns > 0
                    || young_truncate_fast_returns > 0
                    || slow_returns > 0)
                    .then_some(VmFunctionProfile {
                        name: chunk.name.clone(),
                        thin: chunk.thin,
                        parent_thin: chunk.parent_thin,
                        entries,
                        fast_returns,
                        young_truncate_fast_returns,
                        slow_returns,
                    })
            })
            .collect::<Vec<_>>();
        functions.sort_by(|a, b| b.entries.cmp(&a.entries).then_with(|| a.name.cmp(&b.name)));

        let mut builtins = self
            .builtin_calls
            .iter()
            .map(|(name, count)| VmBuiltinProfile {
                name: name.clone(),
                count: *count,
            })
            .collect::<Vec<_>>();
        builtins.sort_by(|a, b| b.count.cmp(&a.count).then_with(|| a.name.cmp(&b.name)));

        VmProfileReport {
            total_opcodes,
            opcodes,
            functions,
            builtins,
            returns: self.return_stats.clone(),
        }
    }
}

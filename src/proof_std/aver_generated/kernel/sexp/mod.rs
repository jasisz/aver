#[allow(unused_imports)]
use crate::proof_std::*;
use ::aver_rt::aver_list_match;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Sexp {
    Atom(AverStr),
    Text(AverStr),
    Node(aver_rt::AverList<Sexp>),
}

impl Sexp {
    fn aver_key_rank(&self) -> usize {
        match self {
            Sexp::Atom(..) => 0,
            Sexp::Node(..) => 1,
            Sexp::Text(..) => 2,
        }
    }
}

impl PartialOrd for Sexp {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Sexp {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        let rank = self.aver_key_rank().cmp(&other.aver_key_rank());
        if rank != std::cmp::Ordering::Equal {
            return rank;
        }
        match (self, other) {
            (Sexp::Atom(a0), Sexp::Atom(b0)) => std::cmp::Ordering::Equal.then_with(|| a0.cmp(b0)),
            (Sexp::Node(a0), Sexp::Node(b0)) => std::cmp::Ordering::Equal.then_with(|| a0.cmp(b0)),
            (Sexp::Text(a0), Sexp::Text(b0)) => std::cmp::Ordering::Equal.then_with(|| a0.cmp(b0)),
            _ => std::cmp::Ordering::Equal,
        }
    }
}

impl aver_rt::AverDisplay for Sexp {
    fn aver_display(&self) -> String {
        match self {
            Sexp::Atom(f0) => format!("Atom({})", f0.aver_display_inner()),
            Sexp::Text(f0) => format!("Text({})", f0.aver_display_inner()),
            Sexp::Node(f0) => format!("Node({})", f0.aver_display_inner()),
        }
    }
    fn aver_display_inner(&self) -> String {
        self.aver_display()
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum ReadMode {
    Ready,
    Atom(AverStr),
    Text(AverStr),
    Escape(AverStr),
    Comment,
}

impl ReadMode {
    fn aver_key_rank(&self) -> usize {
        match self {
            ReadMode::Atom(..) => 0,
            ReadMode::Comment => 1,
            ReadMode::Escape(..) => 2,
            ReadMode::Ready => 3,
            ReadMode::Text(..) => 4,
        }
    }
}

impl PartialOrd for ReadMode {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for ReadMode {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        let rank = self.aver_key_rank().cmp(&other.aver_key_rank());
        if rank != std::cmp::Ordering::Equal {
            return rank;
        }
        match (self, other) {
            (ReadMode::Atom(a0), ReadMode::Atom(b0)) => {
                std::cmp::Ordering::Equal.then_with(|| a0.cmp(b0))
            }
            (ReadMode::Escape(a0), ReadMode::Escape(b0)) => {
                std::cmp::Ordering::Equal.then_with(|| a0.cmp(b0))
            }
            (ReadMode::Text(a0), ReadMode::Text(b0)) => {
                std::cmp::Ordering::Equal.then_with(|| a0.cmp(b0))
            }
            _ => std::cmp::Ordering::Equal,
        }
    }
}

impl aver_rt::AverDisplay for ReadMode {
    fn aver_display(&self) -> String {
        match self {
            ReadMode::Ready => "Ready".to_string(),
            ReadMode::Atom(f0) => format!("Atom({})", f0.aver_display_inner()),
            ReadMode::Text(f0) => format!("Text({})", f0.aver_display_inner()),
            ReadMode::Escape(f0) => format!("Escape({})", f0.aver_display_inner()),
            ReadMode::Comment => "Comment".to_string(),
        }
    }
    fn aver_display_inner(&self) -> String {
        self.aver_display()
    }
}

/// One S-expression and nothing after it but blanks, its shared parts written out.
#[inline(always)]
pub fn readAll(text @ _: AverStr) -> Result<Sexp, AverStr> {
    crate::proof_std::cancel_checkpoint();
    let __arg1 @ _ = crate::proof_std::aver_generated::kernel::sexp::ReadMode::Ready;
    let __arg2 @ _ = <aver_rt::AverList<Sexp>>::empty();
    let __arg3 @ _ = <aver_rt::AverList<aver_rt::AverList<Sexp>>>::empty();
    crate::proof_std::aver_generated::kernel::sexp::scan__cursor(
        text, 0i64, &__arg1, __arg2, __arg3,
    )
}

/// Read characters once, keeping reversed items for the current list and each parent list.
#[inline(always)]
pub fn scan(
    cs @ _: &aver_rt::AverList<AverStr>,
    mode @ _: &ReadMode,
    mut current @ _: aver_rt::AverList<Sexp>,
    mut parents @ _: aver_rt::AverList<aver_rt::AverList<Sexp>>,
) -> Result<Sexp, AverStr> {
    let mut __hold0: aver_rt::AverList<AverStr>;
    let mut cs @ _: &aver_rt::AverList<AverStr> = cs;
    let mut __hold1: ReadMode;
    let mut mode @ _: &ReadMode = mode;
    loop {
        crate::proof_std::cancel_checkpoint();
        {
            let __list_subject = cs;
            if __list_subject.is_empty() {
                match mode {
                    crate::proof_std::aver_generated::kernel::sexp::ReadMode::Text(acc) => {
                        let acc = acc.clone();
                        return Err(::aver_rt::str_lit!("unterminated text"));
                    }
                    crate::proof_std::aver_generated::kernel::sexp::ReadMode::Escape(acc) => {
                        let acc = acc.clone();
                        return Err(::aver_rt::str_lit!("unterminated text"));
                    }
                    crate::proof_std::aver_generated::kernel::sexp::ReadMode::Atom(acc) => {
                        let acc = acc.clone();
                        return {
                            let __arg14 @ _ = aver_rt::AverList::prepend_owned(
                                crate::proof_std::aver_generated::kernel::sexp::Sexp::Atom(acc),
                                current,
                            );
                            crate::proof_std::aver_generated::kernel::sexp::finish(
                                &__arg14, &parents,
                            )
                        };
                    }
                    _ => {
                        return crate::proof_std::aver_generated::kernel::sexp::finish(
                            &current, &parents,
                        );
                    }
                }
            } else {
                if let Some((c, rest)) = aver_rt::list_uncons_cloned(__list_subject) {
                    match mode {
                        crate::proof_std::aver_generated::kernel::sexp::ReadMode::Ready => {
                            let __dispatch_subject =
                                aver_rt::AverInt::from_i64(aver_rt::str_code1(&c));
                            if __dispatch_subject == aver_rt::AverInt::from_i64(32) {
                                {
                                    let __tco0 = rest;
                                    __hold0 = __tco0;
                                    cs = &__hold0;
                                    continue;
                                }
                            } else {
                                if __dispatch_subject == aver_rt::AverInt::from_i64(10) {
                                    {
                                        let __tco0 = rest;
                                        __hold0 = __tco0;
                                        cs = &__hold0;
                                        continue;
                                    }
                                } else {
                                    if __dispatch_subject == aver_rt::AverInt::from_i64(9) {
                                        {
                                            let __tco0 = rest;
                                            __hold0 = __tco0;
                                            cs = &__hold0;
                                            continue;
                                        }
                                    } else {
                                        if __dispatch_subject == aver_rt::AverInt::from_i64(13) {
                                            {
                                                let __tco0 = rest;
                                                __hold0 = __tco0;
                                                cs = &__hold0;
                                                continue;
                                            }
                                        } else {
                                            if __dispatch_subject == aver_rt::AverInt::from_i64(59)
                                            {
                                                {
                                                    let __tco0 = rest;
                                                    let __tco1 = crate::proof_std::aver_generated::kernel::sexp::ReadMode::Comment;
                                                    __hold0 = __tco0;
                                                    cs = &__hold0;
                                                    __hold1 = __tco1;
                                                    mode = &__hold1;
                                                    continue;
                                                }
                                            } else {
                                                if __dispatch_subject
                                                    == aver_rt::AverInt::from_i64(40)
                                                {
                                                    {
                                                        let __tco0 = rest;
                                                        let __tco2 = aver_rt::AverList::empty();
                                                        let __tco3 =
                                                            aver_rt::AverList::prepend_owned(
                                                                current, parents,
                                                            );
                                                        __hold0 = __tco0;
                                                        cs = &__hold0;
                                                        current = __tco2;
                                                        parents = __tco3;
                                                        continue;
                                                    }
                                                } else {
                                                    if __dispatch_subject
                                                        == aver_rt::AverInt::from_i64(41)
                                                    {
                                                        {
                                                            let __list_subject = parents;
                                                            if __list_subject.is_empty() {
                                                                return Err(::aver_rt::str_lit!(
                                                                    "unexpected )"
                                                                ));
                                                            } else {
                                                                if let (Some(parent), Some(above)) = (
                                                                    __list_subject.first(),
                                                                    __list_subject.tail(),
                                                                ) {
                                                                    {
                                                                        let __tco0 = rest;
                                                                        let __tco2 = aver_rt::AverList::prepend(crate::proof_std::aver_generated::kernel::sexp::Sexp::Node(current.reverse()), parent);
                                                                        let __tco3 = above;
                                                                        __hold0 = __tco0;
                                                                        cs = &__hold0;
                                                                        current = __tco2;
                                                                        parents = __tco3;
                                                                        continue;
                                                                    }
                                                                } else {
                                                                    panic!(
                                                                        "Aver Rust codegen: non-exhaustive list match"
                                                                    )
                                                                }
                                                            }
                                                        }
                                                    } else {
                                                        if __dispatch_subject
                                                            == aver_rt::AverInt::from_i64(34)
                                                        {
                                                            {
                                                                let __tco0 = rest;
                                                                let __tco1 = crate::proof_std::aver_generated::kernel::sexp::ReadMode::Text(::aver_rt::str_lit!(""));
                                                                __hold0 = __tco0;
                                                                cs = &__hold0;
                                                                __hold1 = __tco1;
                                                                mode = &__hold1;
                                                                continue;
                                                            }
                                                        } else {
                                                            {
                                                                let __tco0 = rest;
                                                                let __tco1 = crate::proof_std::aver_generated::kernel::sexp::ReadMode::Atom(c);
                                                                __hold0 = __tco0;
                                                                cs = &__hold0;
                                                                __hold1 = __tco1;
                                                                mode = &__hold1;
                                                                continue;
                                                            }
                                                        }
                                                    }
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                        crate::proof_std::aver_generated::kernel::sexp::ReadMode::Atom(acc) => {
                            let acc = acc.clone();
                            {
                                let __dispatch_subject =
                                    aver_rt::AverInt::from_i64(aver_rt::str_code1(&c));
                                if __dispatch_subject == aver_rt::AverInt::from_i64(32) {
                                    {
                                        let __tco0 = rest;
                                        let __tco1 = crate::proof_std::aver_generated::kernel::sexp::ReadMode::Ready;
                                        let __tco2 = aver_rt::AverList::prepend_owned(crate::proof_std::aver_generated::kernel::sexp::Sexp::Atom(acc), current);
                                        __hold0 = __tco0;
                                        cs = &__hold0;
                                        __hold1 = __tco1;
                                        mode = &__hold1;
                                        current = __tco2;
                                        continue;
                                    }
                                } else {
                                    if __dispatch_subject == aver_rt::AverInt::from_i64(10) {
                                        {
                                            let __tco0 = rest;
                                            let __tco1 = crate::proof_std::aver_generated::kernel::sexp::ReadMode::Ready;
                                            let __tco2 = aver_rt::AverList::prepend_owned(crate::proof_std::aver_generated::kernel::sexp::Sexp::Atom(acc), current);
                                            __hold0 = __tco0;
                                            cs = &__hold0;
                                            __hold1 = __tco1;
                                            mode = &__hold1;
                                            current = __tco2;
                                            continue;
                                        }
                                    } else {
                                        if __dispatch_subject == aver_rt::AverInt::from_i64(9) {
                                            {
                                                let __tco0 = rest;
                                                let __tco1 = crate::proof_std::aver_generated::kernel::sexp::ReadMode::Ready;
                                                let __tco2 = aver_rt::AverList::prepend_owned(crate::proof_std::aver_generated::kernel::sexp::Sexp::Atom(acc), current);
                                                __hold0 = __tco0;
                                                cs = &__hold0;
                                                __hold1 = __tco1;
                                                mode = &__hold1;
                                                current = __tco2;
                                                continue;
                                            }
                                        } else {
                                            if __dispatch_subject == aver_rt::AverInt::from_i64(13)
                                            {
                                                {
                                                    let __tco0 = rest;
                                                    let __tco1 = crate::proof_std::aver_generated::kernel::sexp::ReadMode::Ready;
                                                    let __tco2 = aver_rt::AverList::prepend_owned(crate::proof_std::aver_generated::kernel::sexp::Sexp::Atom(acc), current);
                                                    __hold0 = __tco0;
                                                    cs = &__hold0;
                                                    __hold1 = __tco1;
                                                    mode = &__hold1;
                                                    current = __tco2;
                                                    continue;
                                                }
                                            } else {
                                                if __dispatch_subject
                                                    == aver_rt::AverInt::from_i64(40)
                                                {
                                                    {
                                                        let __tco1 = crate::proof_std::aver_generated::kernel::sexp::ReadMode::Ready;
                                                        let __tco2 = aver_rt::AverList::prepend_owned(crate::proof_std::aver_generated::kernel::sexp::Sexp::Atom(acc), current);
                                                        __hold1 = __tco1;
                                                        mode = &__hold1;
                                                        current = __tco2;
                                                        continue;
                                                    }
                                                } else {
                                                    if __dispatch_subject
                                                        == aver_rt::AverInt::from_i64(41)
                                                    {
                                                        {
                                                            let __tco1 = crate::proof_std::aver_generated::kernel::sexp::ReadMode::Ready;
                                                            let __tco2 = aver_rt::AverList::prepend_owned(crate::proof_std::aver_generated::kernel::sexp::Sexp::Atom(acc), current);
                                                            __hold1 = __tco1;
                                                            mode = &__hold1;
                                                            current = __tco2;
                                                            continue;
                                                        }
                                                    } else {
                                                        if __dispatch_subject
                                                            == aver_rt::AverInt::from_i64(34)
                                                        {
                                                            {
                                                                let __tco1 = crate::proof_std::aver_generated::kernel::sexp::ReadMode::Ready;
                                                                let __tco2 = aver_rt::AverList::prepend_owned(crate::proof_std::aver_generated::kernel::sexp::Sexp::Atom(acc), current);
                                                                __hold1 = __tco1;
                                                                mode = &__hold1;
                                                                current = __tco2;
                                                                continue;
                                                            }
                                                        } else {
                                                            {
                                                                let __tco0 = rest;
                                                                let __tco1 = crate::proof_std::aver_generated::kernel::sexp::ReadMode::Atom((acc + &c));
                                                                __hold0 = __tco0;
                                                                cs = &__hold0;
                                                                __hold1 = __tco1;
                                                                mode = &__hold1;
                                                                continue;
                                                            }
                                                        }
                                                    }
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                        crate::proof_std::aver_generated::kernel::sexp::ReadMode::Text(acc) => {
                            let acc = acc.clone();
                            {
                                let __dispatch_subject =
                                    aver_rt::AverInt::from_i64(aver_rt::str_code1(&c));
                                if __dispatch_subject == aver_rt::AverInt::from_i64(34) {
                                    {
                                        let __tco0 = rest;
                                        let __tco1 = crate::proof_std::aver_generated::kernel::sexp::ReadMode::Ready;
                                        let __tco2 = aver_rt::AverList::prepend_owned(crate::proof_std::aver_generated::kernel::sexp::Sexp::Text(acc), current);
                                        __hold0 = __tco0;
                                        cs = &__hold0;
                                        __hold1 = __tco1;
                                        mode = &__hold1;
                                        current = __tco2;
                                        continue;
                                    }
                                } else {
                                    if __dispatch_subject == aver_rt::AverInt::from_i64(92) {
                                        {
                                            let __tco0 = rest;
                                            let __tco1 = crate::proof_std::aver_generated::kernel::sexp::ReadMode::Escape(acc);
                                            __hold0 = __tco0;
                                            cs = &__hold0;
                                            __hold1 = __tco1;
                                            mode = &__hold1;
                                            continue;
                                        }
                                    } else {
                                        {
                                            let __tco0 = rest;
                                            let __tco1 = crate::proof_std::aver_generated::kernel::sexp::ReadMode::Text((acc + &c));
                                            __hold0 = __tco0;
                                            cs = &__hold0;
                                            __hold1 = __tco1;
                                            mode = &__hold1;
                                            continue;
                                        }
                                    }
                                }
                            }
                        }
                        crate::proof_std::aver_generated::kernel::sexp::ReadMode::Escape(acc) => {
                            let acc = acc.clone();
                            {
                                let __int_match_subject =
                                    aver_rt::AverInt::from_i64(aver_rt::str_code1(&c));
                                if __int_match_subject == aver_rt::AverInt::from_i64(110) {
                                    {
                                        let __tco0 = rest;
                                        let __tco1 = crate::proof_std::aver_generated::kernel::sexp::ReadMode::Text((acc + &::aver_rt::str_lit!("\n")));
                                        __hold0 = __tco0;
                                        cs = &__hold0;
                                        __hold1 = __tco1;
                                        mode = &__hold1;
                                        continue;
                                    }
                                } else {
                                    {
                                        let __tco0 = rest;
                                        let __tco1 = crate::proof_std::aver_generated::kernel::sexp::ReadMode::Text((acc + &c));
                                        __hold0 = __tco0;
                                        cs = &__hold0;
                                        __hold1 = __tco1;
                                        mode = &__hold1;
                                        continue;
                                    }
                                }
                            }
                        }
                        crate::proof_std::aver_generated::kernel::sexp::ReadMode::Comment => {
                            let __int_match_subject =
                                aver_rt::AverInt::from_i64(aver_rt::str_code1(&c));
                            if __int_match_subject == aver_rt::AverInt::from_i64(10) {
                                {
                                    let __tco0 = rest;
                                    let __tco1 = crate::proof_std::aver_generated::kernel::sexp::ReadMode::Ready;
                                    __hold0 = __tco0;
                                    cs = &__hold0;
                                    __hold1 = __tco1;
                                    mode = &__hold1;
                                    continue;
                                }
                            } else {
                                {
                                    let __tco0 = rest;
                                    __hold0 = __tco0;
                                    cs = &__hold0;
                                    continue;
                                }
                            }
                        }
                    }
                } else {
                    panic!("Aver Rust codegen: non-exhaustive list match")
                }
            }
        }
    }
}

/// Finish one top-level form, or name the unmatched/list-tail error.
#[inline(always)]
pub fn finish(
    current @ _: &aver_rt::AverList<Sexp>,
    parents @ _: &aver_rt::AverList<aver_rt::AverList<Sexp>>,
) -> Result<Sexp, AverStr> {
    crate::proof_std::cancel_checkpoint();
    {
        let __list_subject = parents;
        if let Some((parent, above)) = aver_rt::list_uncons_cloned(__list_subject) {
            Err(::aver_rt::str_lit!("unexpected end of data"))
        } else {
            if __list_subject.is_empty() {
                {
                    let __list_subject = current;
                    if __list_subject.is_empty() {
                        Err(::aver_rt::str_lit!("unexpected end of data"))
                    } else {
                        if let (Some(s), Some(__pat0)) =
                            (__list_subject.first(), __list_subject.tail())
                        {
                            {
                                let __list_subject = __pat0;
                                if __list_subject.is_empty() {
                                    crate::proof_std::aver_generated::kernel::sexp::expand(
                                        s.clone(),
                                    )
                                } else {
                                    Err(::aver_rt::str_lit!("text after the S-expression"))
                                }
                            }
                        } else {
                            panic!("Aver Rust codegen: non-exhaustive list match")
                        }
                    }
                }
            } else {
                panic!("Aver Rust codegen: non-exhaustive list match")
            }
        }
    }
}

/// (shared ((#NAME S)…) BODY) is BODY with every #NAME written out as its S; each S may use only the names before it. Anything else is itself.
pub fn expand(mut s @ _: Sexp) -> Result<Sexp, AverStr> {
    crate::proof_std::cancel_checkpoint();
    match &s {
        crate::proof_std::aver_generated::kernel::sexp::Sexp::Node(__pat0) => {
            let __pat0 = __pat0.clone();
            {
                let __list_subject = __pat0;
                if let (Some(__pat1), Some(__pat2)) =
                    (__list_subject.first(), __list_subject.tail())
                {
                    match __pat1 {
                        crate::proof_std::aver_generated::kernel::sexp::Sexp::Atom(__pat3) => {
                            let __pat3 = __pat3.clone();
                            match &*__pat3 {
                                "shared" => {
                                    let __list_subject = __pat2;
                                    if let (Some(__pat4), Some(__pat5)) =
                                        (__list_subject.first(), __list_subject.tail())
                                    {
                                        match __pat4 {
        crate::proof_std::aver_generated::kernel::sexp::Sexp::Node(parts) => {
            { let __list_subject = __pat5; if let (Some(body), Some(__pat6)) = (__list_subject.first(), __list_subject.tail()) { { let __list_subject = __pat6; if __list_subject.is_empty() { { let (b, n) = { let __arg14 @ _ = { let __arg13 @ _ = { let __arg12 @ _ = <aver_rt::AverMap<AverStr, (Sexp, aver_rt::AverInt)>>::new(); crate::proof_std::aver_generated::kernel::sexp::define(parts, __arg12) }?; crate::proof_std::aver_generated::kernel::sexp::within(body.clone(), &__arg13) }?; crate::proof_std::aver_generated::kernel::sexp::bounded(&__arg14) }?; Ok(b) } } else { Ok(s) } } } else { Ok(s) } }
        },
        _ => {
            Ok(s)
        }
    }
                                    } else {
                                        Ok(s)
                                    }
                                }
                                _ => Ok(s),
                            }
                        }
                        _ => Ok(s),
                    }
                } else {
                    Ok(s)
                }
            }
        }
        _ => Ok(s),
    }
}

/// Each (#NAME S) in order, its S written out with the names before it.
#[inline(always)]
pub fn define(
    parts @ _: &aver_rt::AverList<Sexp>,
    mut table @ _: aver_rt::AverMap<AverStr, (Sexp, aver_rt::AverInt)>,
) -> Result<aver_rt::AverMap<AverStr, (Sexp, aver_rt::AverInt)>, AverStr> {
    let mut __hold0: aver_rt::AverList<Sexp>;
    let mut parts @ _: &aver_rt::AverList<Sexp> = parts;
    loop {
        crate::proof_std::cancel_checkpoint();
        {
            let __list_subject = parts;
            if __list_subject.is_empty() {
                return Ok(table);
            } else {
                if let (Some(__pat0), Some(rest)) = (__list_subject.first(), __list_subject.tail())
                {
                    match __pat0 {
                        crate::proof_std::aver_generated::kernel::sexp::Sexp::Node(__pat1) => {
                            let __list_subject = __pat1;
                            if let (Some(__pat2), Some(__pat3)) =
                                (__list_subject.first(), __list_subject.tail())
                            {
                                match __pat2 {
                                    crate::proof_std::aver_generated::kernel::sexp::Sexp::Atom(
                                        n,
                                    ) => {
                                        let n = n.clone();
                                        {
                                            let __list_subject = __pat3;
                                            if let (Some(s), Some(__pat4)) =
                                                (__list_subject.first(), __list_subject.tail())
                                            {
                                                {
                                                    let __list_subject = __pat4;
                                                    if __list_subject.is_empty() {
                                                        {
                                                            let __tco0 = rest;
                                                            let __tco1 = table.clone().insert_owned(crate::proof_std::aver_generated::kernel::sexp::fresh(n, &table)?, { let __arg10 @ _ = crate::proof_std::aver_generated::kernel::sexp::within(s.clone(), &table)?; crate::proof_std::aver_generated::kernel::sexp::bounded(&__arg10) }?);
                                                            __hold0 = __tco0;
                                                            parts = &__hold0;
                                                            table = __tco1;
                                                            continue;
                                                        }
                                                    } else {
                                                        return Err(::aver_rt::str_lit!(
                                                            "malformed shared part"
                                                        ));
                                                    }
                                                }
                                            } else {
                                                return Err(::aver_rt::str_lit!(
                                                    "malformed shared part"
                                                ));
                                            }
                                        }
                                    }
                                    _ => {
                                        return Err(::aver_rt::str_lit!("malformed shared part"));
                                    }
                                }
                            } else {
                                return Err(::aver_rt::str_lit!("malformed shared part"));
                            }
                        }
                        _ => {
                            return Err(::aver_rt::str_lit!("malformed shared part"));
                        }
                    }
                } else {
                    panic!("Aver Rust codegen: non-exhaustive list match")
                }
            }
        }
    }
}

/// A new shared name: it starts with # and is not defined yet.
#[inline(always)]
pub fn fresh(
    n @ _: AverStr,
    table @ _: &aver_rt::AverMap<AverStr, (Sexp, aver_rt::AverInt)>,
) -> Result<AverStr, AverStr> {
    crate::proof_std::cancel_checkpoint();
    if n.starts_with("#") {
        if table.contains_key(&n) {
            Err(aver_rt::AverStr::from({
                let mut __b = {
                    let mut __b = aver_rt::Buffer::with_capacity(
                        (aver_rt::AverInt::from_i64(33)).to_usize().unwrap_or(0),
                    );
                    __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(n))));
                    __b
                };
                __b.push_str(&::aver_rt::str_lit!(" is defined twice"));
                __b
            }))
        } else {
            Ok(n)
        }
    } else {
        Err(aver_rt::AverStr::from({
            let mut __b = {
                let mut __b = aver_rt::Buffer::with_capacity(
                    (aver_rt::AverInt::from_i64(45)).to_usize().unwrap_or(0),
                );
                __b.push_str(&::aver_rt::str_lit!("a shared name starts with #: "));
                __b
            };
            __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(n))));
            __b
        }))
    }
}

/// A written-out part of at most 4194304 atoms, texts and lists: a short script may not stand for an enormous one.
pub fn bounded(r @ _: &(Sexp, aver_rt::AverInt)) -> Result<(Sexp, aver_rt::AverInt), AverStr> {
    crate::proof_std::cancel_checkpoint();
    {
        let (s, n) = r.clone();
        if (n > aver_rt::AverInt::from_i64(4194304)) {
            Err(::aver_rt::str_lit!(
                "a shared part stands for more than 4194304 items"
            ))
        } else {
            Ok(r.clone())
        }
    }
}

/// The S-expression with every #NAME written out, and how many atoms, texts and lists that is; a written-out part is the very value the table holds, not a copy.
pub fn within(
    mut s @ _: Sexp,
    table @ _: &aver_rt::AverMap<AverStr, (Sexp, aver_rt::AverInt)>,
) -> Result<(Sexp, aver_rt::AverInt), AverStr> {
    crate::proof_std::cancel_checkpoint();
    match &s {
        crate::proof_std::aver_generated::kernel::sexp::Sexp::Atom(a) => {
            let a = a.clone();
            if a.starts_with("#") {
                table.get(&a).cloned().ok_or_else(|| {
                    aver_rt::AverStr::from({
                        let mut __b = {
                            let mut __b = aver_rt::Buffer::with_capacity(
                                (aver_rt::AverInt::from_i64(46)).to_usize().unwrap_or(0),
                            );
                            __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(a))));
                            __b
                        };
                        __b.push_str(&::aver_rt::str_lit!(" is not defined before its use"));
                        __b
                    })
                })
            } else {
                Ok((s, aver_rt::AverInt::from_i64(1)))
            }
        }
        crate::proof_std::aver_generated::kernel::sexp::Sexp::Text(t) => {
            let t = t.clone();
            Ok((s, aver_rt::AverInt::from_i64(1)))
        }
        crate::proof_std::aver_generated::kernel::sexp::Sexp::Node(xs) => {
            let __arg5 @ _ = <aver_rt::AverList<Sexp>>::empty();
            crate::proof_std::aver_generated::kernel::sexp::withinAll(
                xs,
                table,
                __arg5,
                aver_rt::AverInt::from_i64(1),
            )
        }
    }
}

/// The items of a list written out, then the list and its size.
#[inline(always)]
pub fn withinAll(
    xs @ _: &aver_rt::AverList<Sexp>,
    table @ _: &aver_rt::AverMap<AverStr, (Sexp, aver_rt::AverInt)>,
    mut acc @ _: aver_rt::AverList<Sexp>,
    mut size @ _: aver_rt::AverInt,
) -> Result<(Sexp, aver_rt::AverInt), AverStr> {
    let mut __hold0: aver_rt::AverList<Sexp>;
    let mut xs @ _: &aver_rt::AverList<Sexp> = xs;
    loop {
        crate::proof_std::cancel_checkpoint();
        {
            let __list_subject = xs;
            if __list_subject.is_empty() {
                return Ok((
                    crate::proof_std::aver_generated::kernel::sexp::Sexp::Node(acc.reverse()),
                    size,
                ));
            } else {
                if let (Some(x), Some(rest)) = (__list_subject.first(), __list_subject.tail()) {
                    {
                        let (y, n) = crate::proof_std::aver_generated::kernel::sexp::within(
                            x.clone(),
                            table,
                        )?;
                        {
                            let __tco0 = rest;
                            let __tco2 = aver_rt::AverList::prepend_owned(y, acc);
                            let __tco3 = size.add(&n);
                            __hold0 = __tco0;
                            xs = &__hold0;
                            acc = __tco2;
                            size = __tco3;
                            continue;
                        }
                    }
                } else {
                    panic!("Aver Rust codegen: non-exhaustive list match")
                }
            }
        }
    }
}

/// Synthesized cursor variant of `scan`. Call sites that hand it `String.chars(s)` walk `s` directly — a byte offset stepped one codepoint at a time — instead of materialising the list of one-character strings.
#[inline(always)]
pub fn scan__cursor(
    mut __cur_s @ _: AverStr,
    mut __cur_i @ _: i64,
    mode @ _: &ReadMode,
    mut current @ _: aver_rt::AverList<Sexp>,
    mut parents @ _: aver_rt::AverList<aver_rt::AverList<Sexp>>,
) -> Result<Sexp, AverStr> {
    let mut __hold2: ReadMode;
    let mut mode @ _: &ReadMode = mode;
    loop {
        crate::proof_std::cancel_checkpoint();
        if aver_rt::str_cursor_end(&__cur_s, (__cur_i) as usize) {
            match mode {
                crate::proof_std::aver_generated::kernel::sexp::ReadMode::Text(acc) => {
                    let acc = acc.clone();
                    return Err(::aver_rt::str_lit!("unterminated text"));
                }
                crate::proof_std::aver_generated::kernel::sexp::ReadMode::Escape(acc) => {
                    let acc = acc.clone();
                    return Err(::aver_rt::str_lit!("unterminated text"));
                }
                crate::proof_std::aver_generated::kernel::sexp::ReadMode::Atom(acc) => {
                    let acc = acc.clone();
                    return {
                        let __arg15 @ _ = aver_rt::AverList::prepend_owned(
                            crate::proof_std::aver_generated::kernel::sexp::Sexp::Atom(acc),
                            current,
                        );
                        crate::proof_std::aver_generated::kernel::sexp::finish(&__arg15, &parents)
                    };
                }
                _ => {
                    return crate::proof_std::aver_generated::kernel::sexp::finish(
                        &current, &parents,
                    );
                }
            }
        } else {
            {
                let c =
                    aver_rt::AverStr::from(aver_rt::str_cursor_head(&__cur_s, (__cur_i) as usize));
                {
                    let __cur_i1 = (aver_rt::str_cursor_next(&__cur_s, (__cur_i) as usize) as i64);
                    match mode {
                        crate::proof_std::aver_generated::kernel::sexp::ReadMode::Ready => {
                            let __dispatch_subject =
                                aver_rt::AverInt::from_i64(aver_rt::str_code1(&c));
                            if __dispatch_subject == aver_rt::AverInt::from_i64(32) {
                                {
                                    let __tco1 = __cur_i1;
                                    __cur_i = __tco1;
                                    continue;
                                }
                            } else {
                                if __dispatch_subject == aver_rt::AverInt::from_i64(10) {
                                    {
                                        let __tco1 = __cur_i1;
                                        __cur_i = __tco1;
                                        continue;
                                    }
                                } else {
                                    if __dispatch_subject == aver_rt::AverInt::from_i64(9) {
                                        {
                                            let __tco1 = __cur_i1;
                                            __cur_i = __tco1;
                                            continue;
                                        }
                                    } else {
                                        if __dispatch_subject == aver_rt::AverInt::from_i64(13) {
                                            {
                                                let __tco1 = __cur_i1;
                                                __cur_i = __tco1;
                                                continue;
                                            }
                                        } else {
                                            if __dispatch_subject == aver_rt::AverInt::from_i64(59)
                                            {
                                                {
                                                    let __tco1 = __cur_i1;
                                                    let __tco2 = crate::proof_std::aver_generated::kernel::sexp::ReadMode::Comment;
                                                    __cur_i = __tco1;
                                                    __hold2 = __tco2;
                                                    mode = &__hold2;
                                                    continue;
                                                }
                                            } else {
                                                if __dispatch_subject
                                                    == aver_rt::AverInt::from_i64(40)
                                                {
                                                    {
                                                        let __tco1 = __cur_i1;
                                                        let __tco3 = aver_rt::AverList::empty();
                                                        let __tco4 =
                                                            aver_rt::AverList::prepend_owned(
                                                                current, parents,
                                                            );
                                                        __cur_i = __tco1;
                                                        current = __tco3;
                                                        parents = __tco4;
                                                        continue;
                                                    }
                                                } else {
                                                    if __dispatch_subject
                                                        == aver_rt::AverInt::from_i64(41)
                                                    {
                                                        {
                                                            let __list_subject = parents;
                                                            if __list_subject.is_empty() {
                                                                return Err(::aver_rt::str_lit!(
                                                                    "unexpected )"
                                                                ));
                                                            } else {
                                                                if let (Some(parent), Some(above)) = (
                                                                    __list_subject.first(),
                                                                    __list_subject.tail(),
                                                                ) {
                                                                    {
                                                                        let __tco1 = __cur_i1;
                                                                        let __tco3 = aver_rt::AverList::prepend(crate::proof_std::aver_generated::kernel::sexp::Sexp::Node(current.reverse()), parent);
                                                                        let __tco4 = above;
                                                                        __cur_i = __tco1;
                                                                        current = __tco3;
                                                                        parents = __tco4;
                                                                        continue;
                                                                    }
                                                                } else {
                                                                    panic!(
                                                                        "Aver Rust codegen: non-exhaustive list match"
                                                                    )
                                                                }
                                                            }
                                                        }
                                                    } else {
                                                        if __dispatch_subject
                                                            == aver_rt::AverInt::from_i64(34)
                                                        {
                                                            {
                                                                let __tco1 = __cur_i1;
                                                                let __tco2 = crate::proof_std::aver_generated::kernel::sexp::ReadMode::Text(::aver_rt::str_lit!(""));
                                                                __cur_i = __tco1;
                                                                __hold2 = __tco2;
                                                                mode = &__hold2;
                                                                continue;
                                                            }
                                                        } else {
                                                            {
                                                                let __tco1 = __cur_i1;
                                                                let __tco2 = crate::proof_std::aver_generated::kernel::sexp::ReadMode::Atom(c);
                                                                __cur_i = __tco1;
                                                                __hold2 = __tco2;
                                                                mode = &__hold2;
                                                                continue;
                                                            }
                                                        }
                                                    }
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                        crate::proof_std::aver_generated::kernel::sexp::ReadMode::Atom(acc) => {
                            let acc = acc.clone();
                            {
                                let __dispatch_subject =
                                    aver_rt::AverInt::from_i64(aver_rt::str_code1(&c));
                                if __dispatch_subject == aver_rt::AverInt::from_i64(32) {
                                    {
                                        let __tco1 = __cur_i1;
                                        let __tco2 = crate::proof_std::aver_generated::kernel::sexp::ReadMode::Ready;
                                        let __tco3 = aver_rt::AverList::prepend_owned(crate::proof_std::aver_generated::kernel::sexp::Sexp::Atom(acc), current);
                                        __cur_i = __tco1;
                                        __hold2 = __tco2;
                                        mode = &__hold2;
                                        current = __tco3;
                                        continue;
                                    }
                                } else {
                                    if __dispatch_subject == aver_rt::AverInt::from_i64(10) {
                                        {
                                            let __tco1 = __cur_i1;
                                            let __tco2 = crate::proof_std::aver_generated::kernel::sexp::ReadMode::Ready;
                                            let __tco3 = aver_rt::AverList::prepend_owned(crate::proof_std::aver_generated::kernel::sexp::Sexp::Atom(acc), current);
                                            __cur_i = __tco1;
                                            __hold2 = __tco2;
                                            mode = &__hold2;
                                            current = __tco3;
                                            continue;
                                        }
                                    } else {
                                        if __dispatch_subject == aver_rt::AverInt::from_i64(9) {
                                            {
                                                let __tco1 = __cur_i1;
                                                let __tco2 = crate::proof_std::aver_generated::kernel::sexp::ReadMode::Ready;
                                                let __tco3 = aver_rt::AverList::prepend_owned(crate::proof_std::aver_generated::kernel::sexp::Sexp::Atom(acc), current);
                                                __cur_i = __tco1;
                                                __hold2 = __tco2;
                                                mode = &__hold2;
                                                current = __tco3;
                                                continue;
                                            }
                                        } else {
                                            if __dispatch_subject == aver_rt::AverInt::from_i64(13)
                                            {
                                                {
                                                    let __tco1 = __cur_i1;
                                                    let __tco2 = crate::proof_std::aver_generated::kernel::sexp::ReadMode::Ready;
                                                    let __tco3 = aver_rt::AverList::prepend_owned(crate::proof_std::aver_generated::kernel::sexp::Sexp::Atom(acc), current);
                                                    __cur_i = __tco1;
                                                    __hold2 = __tco2;
                                                    mode = &__hold2;
                                                    current = __tco3;
                                                    continue;
                                                }
                                            } else {
                                                if __dispatch_subject
                                                    == aver_rt::AverInt::from_i64(40)
                                                {
                                                    {
                                                        let __tco2 = crate::proof_std::aver_generated::kernel::sexp::ReadMode::Ready;
                                                        let __tco3 = aver_rt::AverList::prepend_owned(crate::proof_std::aver_generated::kernel::sexp::Sexp::Atom(acc), current);
                                                        __hold2 = __tco2;
                                                        mode = &__hold2;
                                                        current = __tco3;
                                                        continue;
                                                    }
                                                } else {
                                                    if __dispatch_subject
                                                        == aver_rt::AverInt::from_i64(41)
                                                    {
                                                        {
                                                            let __tco2 = crate::proof_std::aver_generated::kernel::sexp::ReadMode::Ready;
                                                            let __tco3 = aver_rt::AverList::prepend_owned(crate::proof_std::aver_generated::kernel::sexp::Sexp::Atom(acc), current);
                                                            __hold2 = __tco2;
                                                            mode = &__hold2;
                                                            current = __tco3;
                                                            continue;
                                                        }
                                                    } else {
                                                        if __dispatch_subject
                                                            == aver_rt::AverInt::from_i64(34)
                                                        {
                                                            {
                                                                let __tco2 = crate::proof_std::aver_generated::kernel::sexp::ReadMode::Ready;
                                                                let __tco3 = aver_rt::AverList::prepend_owned(crate::proof_std::aver_generated::kernel::sexp::Sexp::Atom(acc), current);
                                                                __hold2 = __tco2;
                                                                mode = &__hold2;
                                                                current = __tco3;
                                                                continue;
                                                            }
                                                        } else {
                                                            {
                                                                let __tco1 = __cur_i1;
                                                                let __tco2 = crate::proof_std::aver_generated::kernel::sexp::ReadMode::Atom((acc + &c));
                                                                __cur_i = __tco1;
                                                                __hold2 = __tco2;
                                                                mode = &__hold2;
                                                                continue;
                                                            }
                                                        }
                                                    }
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                        crate::proof_std::aver_generated::kernel::sexp::ReadMode::Text(acc) => {
                            let acc = acc.clone();
                            {
                                let __dispatch_subject =
                                    aver_rt::AverInt::from_i64(aver_rt::str_code1(&c));
                                if __dispatch_subject == aver_rt::AverInt::from_i64(34) {
                                    {
                                        let __tco1 = __cur_i1;
                                        let __tco2 = crate::proof_std::aver_generated::kernel::sexp::ReadMode::Ready;
                                        let __tco3 = aver_rt::AverList::prepend_owned(crate::proof_std::aver_generated::kernel::sexp::Sexp::Text(acc), current);
                                        __cur_i = __tco1;
                                        __hold2 = __tco2;
                                        mode = &__hold2;
                                        current = __tco3;
                                        continue;
                                    }
                                } else {
                                    if __dispatch_subject == aver_rt::AverInt::from_i64(92) {
                                        {
                                            let __tco1 = __cur_i1;
                                            let __tco2 = crate::proof_std::aver_generated::kernel::sexp::ReadMode::Escape(acc);
                                            __cur_i = __tco1;
                                            __hold2 = __tco2;
                                            mode = &__hold2;
                                            continue;
                                        }
                                    } else {
                                        {
                                            let __tco1 = __cur_i1;
                                            let __tco2 = crate::proof_std::aver_generated::kernel::sexp::ReadMode::Text((acc + &c));
                                            __cur_i = __tco1;
                                            __hold2 = __tco2;
                                            mode = &__hold2;
                                            continue;
                                        }
                                    }
                                }
                            }
                        }
                        crate::proof_std::aver_generated::kernel::sexp::ReadMode::Escape(acc) => {
                            let acc = acc.clone();
                            {
                                let __int_match_subject =
                                    aver_rt::AverInt::from_i64(aver_rt::str_code1(&c));
                                if __int_match_subject == aver_rt::AverInt::from_i64(110) {
                                    {
                                        let __tco1 = __cur_i1;
                                        let __tco2 = crate::proof_std::aver_generated::kernel::sexp::ReadMode::Text((acc + &::aver_rt::str_lit!("\n")));
                                        __cur_i = __tco1;
                                        __hold2 = __tco2;
                                        mode = &__hold2;
                                        continue;
                                    }
                                } else {
                                    {
                                        let __tco1 = __cur_i1;
                                        let __tco2 = crate::proof_std::aver_generated::kernel::sexp::ReadMode::Text((acc + &c));
                                        __cur_i = __tco1;
                                        __hold2 = __tco2;
                                        mode = &__hold2;
                                        continue;
                                    }
                                }
                            }
                        }
                        crate::proof_std::aver_generated::kernel::sexp::ReadMode::Comment => {
                            let __int_match_subject =
                                aver_rt::AverInt::from_i64(aver_rt::str_code1(&c));
                            if __int_match_subject == aver_rt::AverInt::from_i64(10) {
                                {
                                    let __tco1 = __cur_i1;
                                    let __tco2 = crate::proof_std::aver_generated::kernel::sexp::ReadMode::Ready;
                                    __cur_i = __tco1;
                                    __hold2 = __tco2;
                                    mode = &__hold2;
                                    continue;
                                }
                            } else {
                                {
                                    let __tco1 = __cur_i1;
                                    __cur_i = __tco1;
                                    continue;
                                }
                            }
                        }
                    }
                }
            }
        }
    }
}

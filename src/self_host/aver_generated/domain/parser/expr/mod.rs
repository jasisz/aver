#[allow(unused_imports)]
use crate::*;

#[allow(non_camel_case_types)]
enum __MutualTco1 {
    ParseAddExprTail(aver_rt::AverInt, crate::aver_generated::domain::ast::Expr),
    ParseAddExprRight(
        aver_rt::AverInt,
        crate::aver_generated::domain::ast::Expr,
        aver_rt::AverInt,
    ),
}

fn __mutual_tco_trampoline_1(
    mut __state: __MutualTco1,
    tokens @ _: &aver_rt::AverList<crate::aver_generated::domain::token::Token>,
) -> Result<(crate::aver_generated::domain::ast::Expr, aver_rt::AverInt), AverStr> {
    loop {
        __state = match __state {
            __MutualTco1::ParseAddExprTail(mut pos @ _, mut left @ _) => {
                crate::cancel_checkpoint();
                let nextPos @ _ = pos.add(&aver_rt::AverInt::from_i64(1));
                let t @ _ = crate::aver_generated::domain::parser_match::continuationAt(
                    &*tokens,
                    pos.clone(),
                );
                match t {
                    crate::aver_generated::domain::token::Token::TkPlus => {
                        __MutualTco1::ParseAddExprRight(
                            nextPos,
                            left,
                            aver_rt::AverInt::from_i64(0),
                        )
                    }
                    crate::aver_generated::domain::token::Token::TkMinus => {
                        __MutualTco1::ParseAddExprRight(
                            nextPos,
                            left,
                            aver_rt::AverInt::from_i64(1),
                        )
                    }
                    _ => return Ok((left, pos)),
                }
            }
            __MutualTco1::ParseAddExprRight(mut pos @ _, mut left @ _, mut op @ _) => {
                crate::cancel_checkpoint();
                let r @ _ =
                    crate::aver_generated::domain::parser::expr::parseMulExpr(&*tokens, pos)?;
                {
                    let (right, pos2) = r;
                    if (op == aver_rt::AverInt::from_i64(0)) {
                        {
                            let __arg7 @ _ = crate::aver_generated::domain::ast::Expr::ExprAdd(
                                crate::aver_generated::domain::ast::Expr__ExprAdd(left, right),
                            );
                            __MutualTco1::ParseAddExprTail(pos2, __arg7)
                        }
                    } else {
                        {
                            let __arg8 @ _ = crate::aver_generated::domain::ast::Expr::ExprSub(
                                crate::aver_generated::domain::ast::Expr__ExprSub(left, right),
                            );
                            __MutualTco1::ParseAddExprTail(pos2, __arg8)
                        }
                    }
                }
            }
        };
    }
}

/// Continue parsing additive operators.
pub fn parseAddExprTail(
    tokens @ _: &aver_rt::AverList<crate::aver_generated::domain::token::Token>,
    mut pos @ _: aver_rt::AverInt,
    mut left @ _: crate::aver_generated::domain::ast::Expr,
) -> Result<(crate::aver_generated::domain::ast::Expr, aver_rt::AverInt), AverStr> {
    crate::cancel_checkpoint();
    let nextPos @ _ = pos.add(&aver_rt::AverInt::from_i64(1));
    let t @ _ = crate::aver_generated::domain::parser_match::continuationAt(&*tokens, pos.clone());
    match t {
        crate::aver_generated::domain::token::Token::TkPlus => {
            return __mutual_tco_trampoline_1(
                __MutualTco1::ParseAddExprRight(nextPos, left, aver_rt::AverInt::from_i64(0)),
                tokens,
            );
        }
        crate::aver_generated::domain::token::Token::TkMinus => {
            return __mutual_tco_trampoline_1(
                __MutualTco1::ParseAddExprRight(nextPos, left, aver_rt::AverInt::from_i64(1)),
                tokens,
            );
        }
        _ => return Ok((left, pos)),
    }
}

/// Parse right side of additive op.
pub fn parseAddExprRight(
    tokens @ _: &aver_rt::AverList<crate::aver_generated::domain::token::Token>,
    mut pos @ _: aver_rt::AverInt,
    mut left @ _: crate::aver_generated::domain::ast::Expr,
    mut op @ _: aver_rt::AverInt,
) -> Result<(crate::aver_generated::domain::ast::Expr, aver_rt::AverInt), AverStr> {
    crate::cancel_checkpoint();
    let r @ _ = crate::aver_generated::domain::parser::expr::parseMulExpr(&*tokens, pos)?;
    {
        let (right, pos2) = r;
        if (op == aver_rt::AverInt::from_i64(0)) {
            {
                let __arg7 @ _ = crate::aver_generated::domain::ast::Expr::ExprAdd(
                    crate::aver_generated::domain::ast::Expr__ExprAdd(left, right),
                );
                return __mutual_tco_trampoline_1(
                    __MutualTco1::ParseAddExprTail(pos2, __arg7),
                    tokens,
                );
            }
        } else {
            {
                let __arg8 @ _ = crate::aver_generated::domain::ast::Expr::ExprSub(
                    crate::aver_generated::domain::ast::Expr__ExprSub(left, right),
                );
                return __mutual_tco_trampoline_1(
                    __MutualTco1::ParseAddExprTail(pos2, __arg8),
                    tokens,
                );
            }
        }
    }
}

#[allow(non_camel_case_types)]
enum __MutualTco2 {
    ParseMulExprTail(aver_rt::AverInt, crate::aver_generated::domain::ast::Expr),
    ParseMulExprRight(
        aver_rt::AverInt,
        crate::aver_generated::domain::ast::Expr,
        aver_rt::AverInt,
    ),
}

fn __mutual_tco_trampoline_2(
    mut __state: __MutualTco2,
    tokens @ _: &aver_rt::AverList<crate::aver_generated::domain::token::Token>,
) -> Result<(crate::aver_generated::domain::ast::Expr, aver_rt::AverInt), AverStr> {
    loop {
        __state = match __state {
            __MutualTco2::ParseMulExprTail(mut pos @ _, mut left @ _) => {
                crate::cancel_checkpoint();
                let nextPos @ _ = pos.add(&aver_rt::AverInt::from_i64(1));
                match crate::aver_generated::domain::parser_match::continuationAt(
                    &*tokens,
                    pos.clone(),
                ) {
                    crate::aver_generated::domain::token::Token::TkStar => {
                        __MutualTco2::ParseMulExprRight(
                            nextPos,
                            left,
                            aver_rt::AverInt::from_i64(0),
                        )
                    }
                    crate::aver_generated::domain::token::Token::TkSlash => {
                        __MutualTco2::ParseMulExprRight(
                            nextPos,
                            left,
                            aver_rt::AverInt::from_i64(1),
                        )
                    }
                    _ => return Ok((left, pos)),
                }
            }
            __MutualTco2::ParseMulExprRight(mut pos @ _, mut left @ _, mut op @ _) => {
                crate::cancel_checkpoint();
                let r @ _ = crate::aver_generated::domain::parser::expr::parseAtom(&*tokens, pos)?;
                {
                    let (right, pos2) = r;
                    if (op == aver_rt::AverInt::from_i64(0)) {
                        {
                            let __arg7 @ _ = crate::aver_generated::domain::ast::Expr::ExprMul(
                                crate::aver_generated::domain::ast::Expr__ExprMul(left, right),
                            );
                            __MutualTco2::ParseMulExprTail(pos2, __arg7)
                        }
                    } else {
                        {
                            let __arg8 @ _ = crate::aver_generated::domain::ast::Expr::ExprDiv(
                                crate::aver_generated::domain::ast::Expr__ExprDiv(left, right),
                            );
                            __MutualTco2::ParseMulExprTail(pos2, __arg8)
                        }
                    }
                }
            }
        };
    }
}

/// Continue parsing multiplicative operators.
pub fn parseMulExprTail(
    tokens @ _: &aver_rt::AverList<crate::aver_generated::domain::token::Token>,
    mut pos @ _: aver_rt::AverInt,
    mut left @ _: crate::aver_generated::domain::ast::Expr,
) -> Result<(crate::aver_generated::domain::ast::Expr, aver_rt::AverInt), AverStr> {
    crate::cancel_checkpoint();
    let nextPos @ _ = pos.add(&aver_rt::AverInt::from_i64(1));
    match crate::aver_generated::domain::parser_match::continuationAt(&*tokens, pos.clone()) {
        crate::aver_generated::domain::token::Token::TkStar => {
            return __mutual_tco_trampoline_2(
                __MutualTco2::ParseMulExprRight(nextPos, left, aver_rt::AverInt::from_i64(0)),
                tokens,
            );
        }
        crate::aver_generated::domain::token::Token::TkSlash => {
            return __mutual_tco_trampoline_2(
                __MutualTco2::ParseMulExprRight(nextPos, left, aver_rt::AverInt::from_i64(1)),
                tokens,
            );
        }
        _ => return Ok((left, pos)),
    }
}

/// Parse right side of mul/div op.
pub fn parseMulExprRight(
    tokens @ _: &aver_rt::AverList<crate::aver_generated::domain::token::Token>,
    mut pos @ _: aver_rt::AverInt,
    mut left @ _: crate::aver_generated::domain::ast::Expr,
    mut op @ _: aver_rt::AverInt,
) -> Result<(crate::aver_generated::domain::ast::Expr, aver_rt::AverInt), AverStr> {
    crate::cancel_checkpoint();
    let r @ _ = crate::aver_generated::domain::parser::expr::parseAtom(&*tokens, pos)?;
    {
        let (right, pos2) = r;
        if (op == aver_rt::AverInt::from_i64(0)) {
            {
                let __arg7 @ _ = crate::aver_generated::domain::ast::Expr::ExprMul(
                    crate::aver_generated::domain::ast::Expr__ExprMul(left, right),
                );
                return __mutual_tco_trampoline_2(
                    __MutualTco2::ParseMulExprTail(pos2, __arg7),
                    tokens,
                );
            }
        } else {
            {
                let __arg8 @ _ = crate::aver_generated::domain::ast::Expr::ExprDiv(
                    crate::aver_generated::domain::ast::Expr__ExprDiv(left, right),
                );
                return __mutual_tco_trampoline_2(
                    __MutualTco2::ParseMulExprTail(pos2, __arg8),
                    tokens,
                );
            }
        }
    }
}

#[allow(non_camel_case_types)]
enum __MutualTco3 {
    ParseMapEntries(
        aver_rt::AverInt,
        aver_rt::AverList<crate::aver_generated::domain::ast::Expr>,
    ),
    ParseMapAfterKey(
        aver_rt::AverInt,
        aver_rt::AverList<crate::aver_generated::domain::ast::Expr>,
        crate::aver_generated::domain::ast::Expr,
    ),
    ParseMapEntryTail(
        aver_rt::AverInt,
        aver_rt::AverList<crate::aver_generated::domain::ast::Expr>,
    ),
}

fn __mutual_tco_trampoline_3(
    mut __state: __MutualTco3,
    tokens @ _: &aver_rt::AverList<crate::aver_generated::domain::token::Token>,
) -> Result<(crate::aver_generated::domain::ast::Expr, aver_rt::AverInt), AverStr> {
    loop {
        __state = match __state {
            __MutualTco3::ParseMapEntries(mut pos @ _, mut acc @ _) => {
                crate::cancel_checkpoint();
                let kr @ _ = crate::aver_generated::domain::parser::expr::parseExpr(&*tokens, pos)?;
                {
                    let (keyExpr, pos2) = kr;
                    __MutualTco3::ParseMapAfterKey(pos2, acc, keyExpr)
                }
            }
            __MutualTco3::ParseMapAfterKey(mut pos @ _, mut acc @ _, mut keyExpr @ _) => {
                crate::cancel_checkpoint();
                let pos2 @ _ = {
                    let __arg8 @ _ = crate::aver_generated::domain::token::Token::TkFatArrow;
                    crate::aver_generated::domain::parser_match::expect(&*tokens, pos, &__arg8)
                }?;
                let vr @ _ =
                    crate::aver_generated::domain::parser::expr::parseExpr(&*tokens, pos2)?;
                {
                    let (valExpr, pos3) = vr;
                    {
                        let __arg9 @ _ = aver_rt::AverList::prepend_owned(
                            crate::aver_generated::domain::ast::Expr::ExprTuple(
                                crate::aver_generated::domain::ast::Expr__ExprTuple(
                                    aver_rt::AverList::from_vec(vec![keyExpr, valExpr]),
                                ),
                            ),
                            acc,
                        );
                        __MutualTco3::ParseMapEntryTail(pos3, __arg9)
                    }
                }
            }
            __MutualTco3::ParseMapEntryTail(mut pos @ _, mut acc @ _) => {
                crate::cancel_checkpoint();
                let pos2 @ _ = crate::aver_generated::domain::parser::expr::skipNl(&*tokens, pos);
                match crate::aver_generated::domain::parser_match::tokenAt(&*tokens, pos2.clone()) {
                    crate::aver_generated::domain::token::Token::TkComma => {
                        let __arg4 @ _ = crate::aver_generated::domain::parser::expr::skipNl(
                            &*tokens,
                            pos2.add(&aver_rt::AverInt::from_i64(1)),
                        );
                        __MutualTco3::ParseMapEntries(__arg4, acc)
                    }
                    crate::aver_generated::domain::token::Token::TkRBrace => {
                        return Ok((
                            crate::aver_generated::domain::ast::Expr::ExprCall(
                                crate::aver_generated::domain::ast::Expr__ExprCall(
                                    ::aver_rt::str_lit!("Map.fromList"),
                                    aver_rt::AverList::from_vec(vec![
                                        crate::aver_generated::domain::ast::Expr::ExprList(
                                            crate::aver_generated::domain::ast::Expr__ExprList(
                                                acc.reverse(),
                                            ),
                                        ),
                                    ]),
                                ),
                            ),
                            pos2.add(&aver_rt::AverInt::from_i64(1)),
                        ));
                    }
                    _ => return Err(::aver_rt::str_lit!("Expected ',' or '}' in map literal")),
                }
            }
        };
    }
}

/// Parse key => value pairs.
pub fn parseMapEntries(
    tokens @ _: &aver_rt::AverList<crate::aver_generated::domain::token::Token>,
    mut pos @ _: aver_rt::AverInt,
    mut acc @ _: aver_rt::AverList<crate::aver_generated::domain::ast::Expr>,
) -> Result<(crate::aver_generated::domain::ast::Expr, aver_rt::AverInt), AverStr> {
    crate::cancel_checkpoint();
    let kr @ _ = crate::aver_generated::domain::parser::expr::parseExpr(&*tokens, pos)?;
    {
        let (keyExpr, pos2) = kr;
        return __mutual_tco_trampoline_3(
            __MutualTco3::ParseMapAfterKey(pos2, acc, keyExpr),
            tokens,
        );
    }
}

/// After key, expect => then value.
pub fn parseMapAfterKey(
    tokens @ _: &aver_rt::AverList<crate::aver_generated::domain::token::Token>,
    mut pos @ _: aver_rt::AverInt,
    mut acc @ _: aver_rt::AverList<crate::aver_generated::domain::ast::Expr>,
    mut keyExpr @ _: crate::aver_generated::domain::ast::Expr,
) -> Result<(crate::aver_generated::domain::ast::Expr, aver_rt::AverInt), AverStr> {
    crate::cancel_checkpoint();
    let pos2 @ _ = {
        let __arg8 @ _ = crate::aver_generated::domain::token::Token::TkFatArrow;
        crate::aver_generated::domain::parser_match::expect(&*tokens, pos, &__arg8)
    }?;
    let vr @ _ = crate::aver_generated::domain::parser::expr::parseExpr(&*tokens, pos2)?;
    {
        let (valExpr, pos3) = vr;
        {
            let __arg9 @ _ = aver_rt::AverList::prepend_owned(
                crate::aver_generated::domain::ast::Expr::ExprTuple(
                    crate::aver_generated::domain::ast::Expr__ExprTuple(
                        aver_rt::AverList::from_vec(vec![keyExpr, valExpr]),
                    ),
                ),
                acc,
            );
            return __mutual_tco_trampoline_3(
                __MutualTco3::ParseMapEntryTail(pos3, __arg9),
                tokens,
            );
        }
    }
}

/// After entry: , for more or } to end.
pub fn parseMapEntryTail(
    tokens @ _: &aver_rt::AverList<crate::aver_generated::domain::token::Token>,
    mut pos @ _: aver_rt::AverInt,
    mut acc @ _: aver_rt::AverList<crate::aver_generated::domain::ast::Expr>,
) -> Result<(crate::aver_generated::domain::ast::Expr, aver_rt::AverInt), AverStr> {
    crate::cancel_checkpoint();
    let pos2 @ _ = crate::aver_generated::domain::parser::expr::skipNl(&*tokens, pos);
    match crate::aver_generated::domain::parser_match::tokenAt(&*tokens, pos2.clone()) {
        crate::aver_generated::domain::token::Token::TkComma => {
            let __arg4 @ _ = crate::aver_generated::domain::parser::expr::skipNl(
                &*tokens,
                pos2.add(&aver_rt::AverInt::from_i64(1)),
            );
            return __mutual_tco_trampoline_3(__MutualTco3::ParseMapEntries(__arg4, acc), tokens);
        }
        crate::aver_generated::domain::token::Token::TkRBrace => {
            return Ok((
                crate::aver_generated::domain::ast::Expr::ExprCall(
                    crate::aver_generated::domain::ast::Expr__ExprCall(
                        ::aver_rt::str_lit!("Map.fromList"),
                        aver_rt::AverList::from_vec(vec![
                            crate::aver_generated::domain::ast::Expr::ExprList(
                                crate::aver_generated::domain::ast::Expr__ExprList(acc.reverse()),
                            ),
                        ]),
                    ),
                ),
                pos2.add(&aver_rt::AverInt::from_i64(1)),
            ));
        }
        _ => return Err(::aver_rt::str_lit!("Expected ',' or '}' in map literal")),
    }
}

#[allow(non_camel_case_types)]
enum __MutualTco4 {
    ParseInterpParts(
        aver_rt::AverInt,
        aver_rt::AverList<crate::aver_generated::domain::ast::Expr>,
    ),
    ParseInterpAfterExpr(
        aver_rt::AverInt,
        aver_rt::AverList<crate::aver_generated::domain::ast::Expr>,
    ),
    ParseInterpContinue(
        aver_rt::AverInt,
        aver_rt::AverList<crate::aver_generated::domain::ast::Expr>,
    ),
    ParseInterpAfterStr(
        aver_rt::AverInt,
        aver_rt::AverList<crate::aver_generated::domain::ast::Expr>,
    ),
}

fn __mutual_tco_trampoline_4(
    mut __state: __MutualTco4,
    tokens @ _: &aver_rt::AverList<crate::aver_generated::domain::token::Token>,
) -> Result<(crate::aver_generated::domain::ast::Expr, aver_rt::AverInt), AverStr> {
    loop {
        __state = match __state {
            __MutualTco4::ParseInterpParts(mut pos @ _, mut acc @ _) => {
                crate::cancel_checkpoint();
                let r @ _ = crate::aver_generated::domain::parser::expr::parseExpr(&*tokens, pos)?;
                {
                    let (expr, pos2) = r;
                    {
                        let __arg6 @ _ = aver_rt::AverList::prepend_owned(expr, acc);
                        __MutualTco4::ParseInterpAfterExpr(pos2, __arg6)
                    }
                }
            }
            __MutualTco4::ParseInterpAfterExpr(mut pos @ _, mut acc @ _) => {
                crate::cancel_checkpoint();
                match crate::aver_generated::domain::parser_match::tokenAt(&*tokens, pos.clone()) {
                    crate::aver_generated::domain::token::Token::TkInterpEnd => {
                        __MutualTco4::ParseInterpContinue(
                            pos.add(&aver_rt::AverInt::from_i64(1)),
                            acc,
                        )
                    }
                    _ => return Err(::aver_rt::str_lit!("Expected } in string interpolation")),
                }
            }
            __MutualTco4::ParseInterpContinue(mut pos @ _, mut acc @ _) => {
                crate::cancel_checkpoint();
                let nextPos @ _ = pos.add(&aver_rt::AverInt::from_i64(1));
                match crate::aver_generated::domain::parser_match::tokenAt(&*tokens, pos.clone()) {
                    crate::aver_generated::domain::token::Token::TkStr(s) => {
                        let __arg5 @ _ = aver_rt::AverList::prepend_owned(
                            crate::aver_generated::domain::ast::Expr::ExprStr(
                                crate::aver_generated::domain::ast::Expr__ExprStr(s),
                            ),
                            acc,
                        );
                        __MutualTco4::ParseInterpAfterStr(nextPos, __arg5)
                    }
                    crate::aver_generated::domain::token::Token::TkInterpStart => {
                        __MutualTco4::ParseInterpParts(nextPos, acc)
                    }
                    _ => {
                        return Ok((
                            crate::aver_generated::domain::ast::Expr::ExprConcat(
                                crate::aver_generated::domain::ast::Expr__ExprConcat(acc.reverse()),
                            ),
                            pos,
                        ));
                    }
                }
            }
            __MutualTco4::ParseInterpAfterStr(mut pos @ _, mut acc @ _) => {
                crate::cancel_checkpoint();
                match crate::aver_generated::domain::parser_match::tokenAt(&*tokens, pos.clone()) {
                    crate::aver_generated::domain::token::Token::TkInterpStart => {
                        __MutualTco4::ParseInterpParts(pos.add(&aver_rt::AverInt::from_i64(1)), acc)
                    }
                    _ => {
                        return Ok((
                            crate::aver_generated::domain::ast::Expr::ExprConcat(
                                crate::aver_generated::domain::ast::Expr__ExprConcat(acc.reverse()),
                            ),
                            pos,
                        ));
                    }
                }
            }
        };
    }
}

/// Parse interpolation parts: expressions between TkInterpStart/TkInterpEnd and TkStr segments.
pub fn parseInterpParts(
    tokens @ _: &aver_rt::AverList<crate::aver_generated::domain::token::Token>,
    mut pos @ _: aver_rt::AverInt,
    mut acc @ _: aver_rt::AverList<crate::aver_generated::domain::ast::Expr>,
) -> Result<(crate::aver_generated::domain::ast::Expr, aver_rt::AverInt), AverStr> {
    crate::cancel_checkpoint();
    let r @ _ = crate::aver_generated::domain::parser::expr::parseExpr(&*tokens, pos)?;
    {
        let (expr, pos2) = r;
        {
            let __arg6 @ _ = aver_rt::AverList::prepend_owned(expr, acc);
            return __mutual_tco_trampoline_4(
                __MutualTco4::ParseInterpAfterExpr(pos2, __arg6),
                tokens,
            );
        }
    }
}

/// After interpolation expr: expect TkInterpEnd, then maybe more string.
pub fn parseInterpAfterExpr(
    tokens @ _: &aver_rt::AverList<crate::aver_generated::domain::token::Token>,
    mut pos @ _: aver_rt::AverInt,
    mut acc @ _: aver_rt::AverList<crate::aver_generated::domain::ast::Expr>,
) -> Result<(crate::aver_generated::domain::ast::Expr, aver_rt::AverInt), AverStr> {
    crate::cancel_checkpoint();
    match crate::aver_generated::domain::parser_match::tokenAt(&*tokens, pos.clone()) {
        crate::aver_generated::domain::token::Token::TkInterpEnd => {
            return __mutual_tco_trampoline_4(
                __MutualTco4::ParseInterpContinue(pos.add(&aver_rt::AverInt::from_i64(1)), acc),
                tokens,
            );
        }
        _ => return Err(::aver_rt::str_lit!("Expected } in string interpolation")),
    }
}

/// After }: next TkStr continues, or end of string.
pub fn parseInterpContinue(
    tokens @ _: &aver_rt::AverList<crate::aver_generated::domain::token::Token>,
    mut pos @ _: aver_rt::AverInt,
    mut acc @ _: aver_rt::AverList<crate::aver_generated::domain::ast::Expr>,
) -> Result<(crate::aver_generated::domain::ast::Expr, aver_rt::AverInt), AverStr> {
    crate::cancel_checkpoint();
    let nextPos @ _ = pos.add(&aver_rt::AverInt::from_i64(1));
    match crate::aver_generated::domain::parser_match::tokenAt(&*tokens, pos.clone()) {
        crate::aver_generated::domain::token::Token::TkStr(s) => {
            let __arg5 @ _ = aver_rt::AverList::prepend_owned(
                crate::aver_generated::domain::ast::Expr::ExprStr(
                    crate::aver_generated::domain::ast::Expr__ExprStr(s),
                ),
                acc,
            );
            return __mutual_tco_trampoline_4(
                __MutualTco4::ParseInterpAfterStr(nextPos, __arg5),
                tokens,
            );
        }
        crate::aver_generated::domain::token::Token::TkInterpStart => {
            return __mutual_tco_trampoline_4(__MutualTco4::ParseInterpParts(nextPos, acc), tokens);
        }
        _ => {
            return Ok((
                crate::aver_generated::domain::ast::Expr::ExprConcat(
                    crate::aver_generated::domain::ast::Expr__ExprConcat(acc.reverse()),
                ),
                pos,
            ));
        }
    }
}

/// After string segment: more interpolation or end.
pub fn parseInterpAfterStr(
    tokens @ _: &aver_rt::AverList<crate::aver_generated::domain::token::Token>,
    mut pos @ _: aver_rt::AverInt,
    mut acc @ _: aver_rt::AverList<crate::aver_generated::domain::ast::Expr>,
) -> Result<(crate::aver_generated::domain::ast::Expr, aver_rt::AverInt), AverStr> {
    crate::cancel_checkpoint();
    match crate::aver_generated::domain::parser_match::tokenAt(&*tokens, pos.clone()) {
        crate::aver_generated::domain::token::Token::TkInterpStart => {
            return __mutual_tco_trampoline_4(
                __MutualTco4::ParseInterpParts(pos.add(&aver_rt::AverInt::from_i64(1)), acc),
                tokens,
            );
        }
        _ => {
            return Ok((
                crate::aver_generated::domain::ast::Expr::ExprConcat(
                    crate::aver_generated::domain::ast::Expr__ExprConcat(acc.reverse()),
                ),
                pos,
            ));
        }
    }
}

#[allow(non_camel_case_types)]
enum __MutualTco5 {
    ParseListItems(
        aver_rt::AverInt,
        aver_rt::AverList<crate::aver_generated::domain::ast::Expr>,
    ),
    ParseListItemsTail(
        aver_rt::AverInt,
        aver_rt::AverList<crate::aver_generated::domain::ast::Expr>,
    ),
    ParseListAfterComma(
        aver_rt::AverInt,
        aver_rt::AverList<crate::aver_generated::domain::ast::Expr>,
    ),
}

fn __mutual_tco_trampoline_5(
    mut __state: __MutualTco5,
    tokens @ _: &aver_rt::AverList<crate::aver_generated::domain::token::Token>,
) -> Result<(crate::aver_generated::domain::ast::Expr, aver_rt::AverInt), AverStr> {
    loop {
        __state = match __state {
            __MutualTco5::ParseListItems(mut pos @ _, mut acc @ _) => {
                crate::cancel_checkpoint();
                let r @ _ = crate::aver_generated::domain::parser::expr::parseExpr(&*tokens, pos)?;
                {
                    let (expr, pos2) = r;
                    {
                        let __arg6 @ _ = aver_rt::AverList::prepend_owned(expr, acc);
                        __MutualTco5::ParseListItemsTail(pos2, __arg6)
                    }
                }
            }
            __MutualTco5::ParseListItemsTail(mut pos0 @ _, mut items @ _) => {
                crate::cancel_checkpoint();
                let pos @ _ = crate::aver_generated::domain::parser::expr::skipNl(&*tokens, pos0);
                match crate::aver_generated::domain::parser_match::tokenAt(&*tokens, pos.clone()) {
                    crate::aver_generated::domain::token::Token::TkComma => {
                        let __arg4 @ _ = crate::aver_generated::domain::parser::expr::skipNl(
                            &*tokens,
                            pos.add(&aver_rt::AverInt::from_i64(1)),
                        );
                        __MutualTco5::ParseListAfterComma(__arg4, items)
                    }
                    crate::aver_generated::domain::token::Token::TkRBracket => {
                        return Ok((
                            crate::aver_generated::domain::ast::Expr::ExprList(
                                crate::aver_generated::domain::ast::Expr__ExprList(items.reverse()),
                            ),
                            pos.add(&aver_rt::AverInt::from_i64(1)),
                        ));
                    }
                    _ => return Err(::aver_rt::str_lit!("Expected ',' or ']' in list literal")),
                }
            }
            __MutualTco5::ParseListAfterComma(mut pos @ _, mut items @ _) => {
                crate::cancel_checkpoint();
                match crate::aver_generated::domain::parser_match::tokenAt(&*tokens, pos.clone()) {
                    crate::aver_generated::domain::token::Token::TkRBracket => {
                        return Ok((
                            crate::aver_generated::domain::ast::Expr::ExprList(
                                crate::aver_generated::domain::ast::Expr__ExprList(items),
                            ),
                            pos.add(&aver_rt::AverInt::from_i64(1)),
                        ));
                    }
                    _ => __MutualTco5::ParseListItems(pos, items),
                }
            }
        };
    }
}

/// Parse comma-separated list items.
pub fn parseListItems(
    tokens @ _: &aver_rt::AverList<crate::aver_generated::domain::token::Token>,
    mut pos @ _: aver_rt::AverInt,
    mut acc @ _: aver_rt::AverList<crate::aver_generated::domain::ast::Expr>,
) -> Result<(crate::aver_generated::domain::ast::Expr, aver_rt::AverInt), AverStr> {
    crate::cancel_checkpoint();
    let r @ _ = crate::aver_generated::domain::parser::expr::parseExpr(&*tokens, pos)?;
    {
        let (expr, pos2) = r;
        {
            let __arg6 @ _ = aver_rt::AverList::prepend_owned(expr, acc);
            return __mutual_tco_trampoline_5(
                __MutualTco5::ParseListItemsTail(pos2, __arg6),
                tokens,
            );
        }
    }
}

/// After a list item: ',' for more or ']' to end.
pub fn parseListItemsTail(
    tokens @ _: &aver_rt::AverList<crate::aver_generated::domain::token::Token>,
    mut pos0 @ _: aver_rt::AverInt,
    mut items @ _: aver_rt::AverList<crate::aver_generated::domain::ast::Expr>,
) -> Result<(crate::aver_generated::domain::ast::Expr, aver_rt::AverInt), AverStr> {
    crate::cancel_checkpoint();
    let pos @ _ = crate::aver_generated::domain::parser::expr::skipNl(&*tokens, pos0);
    match crate::aver_generated::domain::parser_match::tokenAt(&*tokens, pos.clone()) {
        crate::aver_generated::domain::token::Token::TkComma => {
            let __arg4 @ _ = crate::aver_generated::domain::parser::expr::skipNl(
                &*tokens,
                pos.add(&aver_rt::AverInt::from_i64(1)),
            );
            return __mutual_tco_trampoline_5(
                __MutualTco5::ParseListAfterComma(__arg4, items),
                tokens,
            );
        }
        crate::aver_generated::domain::token::Token::TkRBracket => {
            return Ok((
                crate::aver_generated::domain::ast::Expr::ExprList(
                    crate::aver_generated::domain::ast::Expr__ExprList(items.reverse()),
                ),
                pos.add(&aver_rt::AverInt::from_i64(1)),
            ));
        }
        _ => return Err(::aver_rt::str_lit!("Expected ',' or ']' in list literal")),
    }
}

/// Allow a trailing comma before ']' in list literals.
pub fn parseListAfterComma(
    tokens @ _: &aver_rt::AverList<crate::aver_generated::domain::token::Token>,
    mut pos @ _: aver_rt::AverInt,
    mut items @ _: aver_rt::AverList<crate::aver_generated::domain::ast::Expr>,
) -> Result<(crate::aver_generated::domain::ast::Expr, aver_rt::AverInt), AverStr> {
    crate::cancel_checkpoint();
    match crate::aver_generated::domain::parser_match::tokenAt(&*tokens, pos.clone()) {
        crate::aver_generated::domain::token::Token::TkRBracket => {
            return Ok((
                crate::aver_generated::domain::ast::Expr::ExprList(
                    crate::aver_generated::domain::ast::Expr__ExprList(items),
                ),
                pos.add(&aver_rt::AverInt::from_i64(1)),
            ));
        }
        _ => return __mutual_tco_trampoline_5(__MutualTco5::ParseListItems(pos, items), tokens),
    }
}

#[allow(non_camel_case_types)]
enum __MutualTco6 {
    ParseTupleRest(
        aver_rt::AverInt,
        aver_rt::AverList<crate::aver_generated::domain::ast::Expr>,
    ),
    ParseTupleRestTail(
        aver_rt::AverInt,
        aver_rt::AverList<crate::aver_generated::domain::ast::Expr>,
    ),
    ParseTupleAfterComma(
        aver_rt::AverInt,
        aver_rt::AverList<crate::aver_generated::domain::ast::Expr>,
    ),
}

fn __mutual_tco_trampoline_6(
    mut __state: __MutualTco6,
    tokens @ _: &aver_rt::AverList<crate::aver_generated::domain::token::Token>,
) -> Result<(crate::aver_generated::domain::ast::Expr, aver_rt::AverInt), AverStr> {
    loop {
        __state = match __state {
            __MutualTco6::ParseTupleRest(mut pos @ _, mut acc @ _) => {
                crate::cancel_checkpoint();
                let r @ _ = crate::aver_generated::domain::parser::expr::parseExpr(&*tokens, pos)?;
                {
                    let (expr, pos2) = r;
                    {
                        let __arg6 @ _ = aver_rt::AverList::prepend_owned(expr, acc);
                        __MutualTco6::ParseTupleRestTail(pos2, __arg6)
                    }
                }
            }
            __MutualTco6::ParseTupleRestTail(mut pos0 @ _, mut items @ _) => {
                crate::cancel_checkpoint();
                let pos @ _ = crate::aver_generated::domain::parser::expr::skipNl(&*tokens, pos0);
                match crate::aver_generated::domain::parser_match::tokenAt(&*tokens, pos.clone()) {
                    crate::aver_generated::domain::token::Token::TkComma => {
                        let __arg4 @ _ = crate::aver_generated::domain::parser::expr::skipNl(
                            &*tokens,
                            pos.add(&aver_rt::AverInt::from_i64(1)),
                        );
                        __MutualTco6::ParseTupleAfterComma(__arg4, items)
                    }
                    crate::aver_generated::domain::token::Token::TkRParen => {
                        return crate::aver_generated::domain::parser::expr::finishTupleOrProduct(
                            &*tokens,
                            pos.add(&aver_rt::AverInt::from_i64(1)),
                            &items.reverse(),
                        );
                    }
                    _ => return Err(::aver_rt::str_lit!("Expected ')' or ',' in tuple")),
                }
            }
            __MutualTco6::ParseTupleAfterComma(mut pos @ _, mut items @ _) => {
                crate::cancel_checkpoint();
                match crate::aver_generated::domain::parser_match::tokenAt(&*tokens, pos.clone()) {
                    crate::aver_generated::domain::token::Token::TkRParen => {
                        return crate::aver_generated::domain::parser::expr::finishTupleOrProduct(
                            &*tokens,
                            pos.add(&aver_rt::AverInt::from_i64(1)),
                            &items,
                        );
                    }
                    _ => __MutualTco6::ParseTupleRest(pos, items),
                }
            }
        };
    }
}

/// Parse remaining tuple elements after first comma.
pub fn parseTupleRest(
    tokens @ _: &aver_rt::AverList<crate::aver_generated::domain::token::Token>,
    mut pos @ _: aver_rt::AverInt,
    mut acc @ _: aver_rt::AverList<crate::aver_generated::domain::ast::Expr>,
) -> Result<(crate::aver_generated::domain::ast::Expr, aver_rt::AverInt), AverStr> {
    crate::cancel_checkpoint();
    let r @ _ = crate::aver_generated::domain::parser::expr::parseExpr(&*tokens, pos)?;
    {
        let (expr, pos2) = r;
        {
            let __arg6 @ _ = aver_rt::AverList::prepend_owned(expr, acc);
            return __mutual_tco_trampoline_6(
                __MutualTco6::ParseTupleRestTail(pos2, __arg6),
                tokens,
            );
        }
    }
}

/// After tuple element: ',' for more or ')' to end.
pub fn parseTupleRestTail(
    tokens @ _: &aver_rt::AverList<crate::aver_generated::domain::token::Token>,
    mut pos0 @ _: aver_rt::AverInt,
    mut items @ _: aver_rt::AverList<crate::aver_generated::domain::ast::Expr>,
) -> Result<(crate::aver_generated::domain::ast::Expr, aver_rt::AverInt), AverStr> {
    crate::cancel_checkpoint();
    let pos @ _ = crate::aver_generated::domain::parser::expr::skipNl(&*tokens, pos0);
    match crate::aver_generated::domain::parser_match::tokenAt(&*tokens, pos.clone()) {
        crate::aver_generated::domain::token::Token::TkComma => {
            let __arg4 @ _ = crate::aver_generated::domain::parser::expr::skipNl(
                &*tokens,
                pos.add(&aver_rt::AverInt::from_i64(1)),
            );
            return __mutual_tco_trampoline_6(
                __MutualTco6::ParseTupleAfterComma(__arg4, items),
                tokens,
            );
        }
        crate::aver_generated::domain::token::Token::TkRParen => {
            return crate::aver_generated::domain::parser::expr::finishTupleOrProduct(
                &*tokens,
                pos.add(&aver_rt::AverInt::from_i64(1)),
                &items.reverse(),
            );
        }
        _ => return Err(::aver_rt::str_lit!("Expected ')' or ',' in tuple")),
    }
}

/// Allow a trailing comma before ')' in tuples.
pub fn parseTupleAfterComma(
    tokens @ _: &aver_rt::AverList<crate::aver_generated::domain::token::Token>,
    mut pos @ _: aver_rt::AverInt,
    mut items @ _: aver_rt::AverList<crate::aver_generated::domain::ast::Expr>,
) -> Result<(crate::aver_generated::domain::ast::Expr, aver_rt::AverInt), AverStr> {
    crate::cancel_checkpoint();
    match crate::aver_generated::domain::parser_match::tokenAt(&*tokens, pos.clone()) {
        crate::aver_generated::domain::token::Token::TkRParen => {
            return crate::aver_generated::domain::parser::expr::finishTupleOrProduct(
                &*tokens,
                pos.add(&aver_rt::AverInt::from_i64(1)),
                &items,
            );
        }
        _ => return __mutual_tco_trampoline_6(__MutualTco6::ParseTupleRest(pos, items), tokens),
    }
}

#[allow(non_camel_case_types)]
enum __MutualTco7 {
    ParseFieldAccess(aver_rt::AverInt, crate::aver_generated::domain::ast::Expr),
    ParseFieldAccessTail(aver_rt::AverInt, crate::aver_generated::domain::ast::Expr),
}

fn __mutual_tco_trampoline_7(
    mut __state: __MutualTco7,
    tokens @ _: &aver_rt::AverList<crate::aver_generated::domain::token::Token>,
) -> Result<(crate::aver_generated::domain::ast::Expr, aver_rt::AverInt), AverStr> {
    loop {
        __state = match __state {
            __MutualTco7::ParseFieldAccess(mut pos @ _, mut obj @ _) => {
                crate::cancel_checkpoint();
                match crate::aver_generated::domain::parser_match::tokenAt(&*tokens, pos.clone()) {
                    crate::aver_generated::domain::token::Token::TkIdent(field) => {
                        __MutualTco7::ParseFieldAccessTail(
                            pos.add(&aver_rt::AverInt::from_i64(1)),
                            crate::aver_generated::domain::ast::Expr::ExprFieldAccess(
                                crate::aver_generated::domain::ast::Expr__ExprFieldAccess(
                                    obj, field,
                                ),
                            ),
                        )
                    }
                    _ => return Err(::aver_rt::str_lit!("Expected field name after '.'")),
                }
            }
            __MutualTco7::ParseFieldAccessTail(mut pos @ _, mut expr @ _) => {
                crate::cancel_checkpoint();
                match crate::aver_generated::domain::parser_match::tokenAt(&*tokens, pos.clone()) {
                    crate::aver_generated::domain::token::Token::TkDot => {
                        __MutualTco7::ParseFieldAccess(
                            pos.add(&aver_rt::AverInt::from_i64(1)),
                            expr,
                        )
                    }
                    _ => return Ok((expr, pos)),
                }
            }
        };
    }
}

/// Parse .field after an expression.
pub fn parseFieldAccess(
    tokens @ _: &aver_rt::AverList<crate::aver_generated::domain::token::Token>,
    mut pos @ _: aver_rt::AverInt,
    mut obj @ _: crate::aver_generated::domain::ast::Expr,
) -> Result<(crate::aver_generated::domain::ast::Expr, aver_rt::AverInt), AverStr> {
    crate::cancel_checkpoint();
    match crate::aver_generated::domain::parser_match::tokenAt(&*tokens, pos.clone()) {
        crate::aver_generated::domain::token::Token::TkIdent(field) => {
            return __mutual_tco_trampoline_7(
                __MutualTco7::ParseFieldAccessTail(
                    pos.add(&aver_rt::AverInt::from_i64(1)),
                    crate::aver_generated::domain::ast::Expr::ExprFieldAccess(
                        crate::aver_generated::domain::ast::Expr__ExprFieldAccess(obj, field),
                    ),
                ),
                tokens,
            );
        }
        _ => return Err(::aver_rt::str_lit!("Expected field name after '.'")),
    }
}

/// Check for chained field access.
pub fn parseFieldAccessTail(
    tokens @ _: &aver_rt::AverList<crate::aver_generated::domain::token::Token>,
    mut pos @ _: aver_rt::AverInt,
    mut expr @ _: crate::aver_generated::domain::ast::Expr,
) -> Result<(crate::aver_generated::domain::ast::Expr, aver_rt::AverInt), AverStr> {
    crate::cancel_checkpoint();
    match crate::aver_generated::domain::parser_match::tokenAt(&*tokens, pos.clone()) {
        crate::aver_generated::domain::token::Token::TkDot => {
            return __mutual_tco_trampoline_7(
                __MutualTco7::ParseFieldAccess(pos.add(&aver_rt::AverInt::from_i64(1)), expr),
                tokens,
            );
        }
        _ => return Ok((expr, pos)),
    }
}

#[allow(non_camel_case_types)]
enum __MutualTco8 {
    ParseRecordFields(
        aver_rt::AverInt,
        AverStr,
        aver_rt::AverList<(AverStr, crate::aver_generated::domain::ast::Expr)>,
    ),
    ParseRecordField(
        aver_rt::AverInt,
        AverStr,
        aver_rt::AverList<(AverStr, crate::aver_generated::domain::ast::Expr)>,
        AverStr,
    ),
    ParseRecordFieldsTail(
        aver_rt::AverInt,
        AverStr,
        aver_rt::AverList<(AverStr, crate::aver_generated::domain::ast::Expr)>,
    ),
    ParseRecordAfterComma(
        aver_rt::AverInt,
        AverStr,
        aver_rt::AverList<(AverStr, crate::aver_generated::domain::ast::Expr)>,
    ),
}

fn __mutual_tco_trampoline_8(
    mut __state: __MutualTco8,
    tokens @ _: &aver_rt::AverList<crate::aver_generated::domain::token::Token>,
) -> Result<(crate::aver_generated::domain::ast::Expr, aver_rt::AverInt), AverStr> {
    loop {
        __state = match __state {
            __MutualTco8::ParseRecordFields(mut pos @ _, mut name @ _, mut acc @ _) => {
                crate::cancel_checkpoint();
                match crate::aver_generated::domain::parser_match::tokenAt(&*tokens, pos.clone()) {
                    crate::aver_generated::domain::token::Token::TkIdent(field) => {
                        __MutualTco8::ParseRecordField(
                            pos.add(&aver_rt::AverInt::from_i64(1)),
                            name,
                            acc,
                            field,
                        )
                    }
                    _ => {
                        return Err(::aver_rt::str_lit!(
                            "Expected field name in record constructor"
                        ));
                    }
                }
            }
            __MutualTco8::ParseRecordField(
                mut pos @ _,
                mut name @ _,
                mut acc @ _,
                mut field @ _,
            ) => {
                crate::cancel_checkpoint();
                let pos2 @ _ = {
                    let __arg9 @ _ = crate::aver_generated::domain::token::Token::TkEq;
                    crate::aver_generated::domain::parser_match::expect(&*tokens, pos, &__arg9)
                }?;
                let r @ _ = crate::aver_generated::domain::parser::expr::parseExpr(&*tokens, pos2)?;
                {
                    let (expr, pos3) = r;
                    {
                        let __arg10 @ _ = aver_rt::AverList::prepend_owned((field, expr), acc);
                        __MutualTco8::ParseRecordFieldsTail(pos3, name, __arg10)
                    }
                }
            }
            __MutualTco8::ParseRecordFieldsTail(mut pos @ _, mut name @ _, mut fields @ _) => {
                crate::cancel_checkpoint();
                let pos2 @ _ = crate::aver_generated::domain::parser::expr::skipNl(&*tokens, pos);
                match crate::aver_generated::domain::parser_match::tokenAt(&*tokens, pos2.clone()) {
                    crate::aver_generated::domain::token::Token::TkComma => {
                        let __arg5 @ _ = crate::aver_generated::domain::parser::expr::skipNl(
                            &*tokens,
                            pos2.add(&aver_rt::AverInt::from_i64(1)),
                        );
                        __MutualTco8::ParseRecordAfterComma(__arg5, name, fields)
                    }
                    crate::aver_generated::domain::token::Token::TkRParen => {
                        return Ok((
                            crate::aver_generated::domain::ast::Expr::ExprRecord(
                                crate::aver_generated::domain::ast::Expr__ExprRecord(
                                    name,
                                    fields.reverse(),
                                ),
                            ),
                            pos2.add(&aver_rt::AverInt::from_i64(1)),
                        ));
                    }
                    _ => {
                        return Err(::aver_rt::str_lit!(
                            "Expected ',' or ')' in record constructor"
                        ));
                    }
                }
            }
            __MutualTco8::ParseRecordAfterComma(mut pos @ _, mut name @ _, mut fields @ _) => {
                crate::cancel_checkpoint();
                match crate::aver_generated::domain::parser_match::tokenAt(&*tokens, pos.clone()) {
                    crate::aver_generated::domain::token::Token::TkRParen => {
                        return Ok((
                            crate::aver_generated::domain::ast::Expr::ExprRecord(
                                crate::aver_generated::domain::ast::Expr__ExprRecord(name, fields),
                            ),
                            pos.add(&aver_rt::AverInt::from_i64(1)),
                        ));
                    }
                    _ => __MutualTco8::ParseRecordFields(pos, name, fields),
                }
            }
        };
    }
}

/// Parse record fields: field = expr, ...
pub fn parseRecordFields(
    tokens @ _: &aver_rt::AverList<crate::aver_generated::domain::token::Token>,
    mut pos @ _: aver_rt::AverInt,
    mut name @ _: AverStr,
    mut acc @ _: aver_rt::AverList<(AverStr, crate::aver_generated::domain::ast::Expr)>,
) -> Result<(crate::aver_generated::domain::ast::Expr, aver_rt::AverInt), AverStr> {
    crate::cancel_checkpoint();
    match crate::aver_generated::domain::parser_match::tokenAt(&*tokens, pos.clone()) {
        crate::aver_generated::domain::token::Token::TkIdent(field) => {
            return __mutual_tco_trampoline_8(
                __MutualTco8::ParseRecordField(
                    pos.add(&aver_rt::AverInt::from_i64(1)),
                    name,
                    acc,
                    field,
                ),
                tokens,
            );
        }
        _ => {
            return Err(::aver_rt::str_lit!(
                "Expected field name in record constructor"
            ));
        }
    }
}

/// Parse = expr after field name.
pub fn parseRecordField(
    tokens @ _: &aver_rt::AverList<crate::aver_generated::domain::token::Token>,
    mut pos @ _: aver_rt::AverInt,
    mut name @ _: AverStr,
    mut acc @ _: aver_rt::AverList<(AverStr, crate::aver_generated::domain::ast::Expr)>,
    mut field @ _: AverStr,
) -> Result<(crate::aver_generated::domain::ast::Expr, aver_rt::AverInt), AverStr> {
    crate::cancel_checkpoint();
    let pos2 @ _ = {
        let __arg9 @ _ = crate::aver_generated::domain::token::Token::TkEq;
        crate::aver_generated::domain::parser_match::expect(&*tokens, pos, &__arg9)
    }?;
    let r @ _ = crate::aver_generated::domain::parser::expr::parseExpr(&*tokens, pos2)?;
    {
        let (expr, pos3) = r;
        {
            let __arg10 @ _ = aver_rt::AverList::prepend_owned((field, expr), acc);
            return __mutual_tco_trampoline_8(
                __MutualTco8::ParseRecordFieldsTail(pos3, name, __arg10),
                tokens,
            );
        }
    }
}

/// After a field: ',' for more or ')' to end.
pub fn parseRecordFieldsTail(
    tokens @ _: &aver_rt::AverList<crate::aver_generated::domain::token::Token>,
    mut pos @ _: aver_rt::AverInt,
    mut name @ _: AverStr,
    mut fields @ _: aver_rt::AverList<(AverStr, crate::aver_generated::domain::ast::Expr)>,
) -> Result<(crate::aver_generated::domain::ast::Expr, aver_rt::AverInt), AverStr> {
    crate::cancel_checkpoint();
    let pos2 @ _ = crate::aver_generated::domain::parser::expr::skipNl(&*tokens, pos);
    match crate::aver_generated::domain::parser_match::tokenAt(&*tokens, pos2.clone()) {
        crate::aver_generated::domain::token::Token::TkComma => {
            let __arg5 @ _ = crate::aver_generated::domain::parser::expr::skipNl(
                &*tokens,
                pos2.add(&aver_rt::AverInt::from_i64(1)),
            );
            return __mutual_tco_trampoline_8(
                __MutualTco8::ParseRecordAfterComma(__arg5, name, fields),
                tokens,
            );
        }
        crate::aver_generated::domain::token::Token::TkRParen => {
            return Ok((
                crate::aver_generated::domain::ast::Expr::ExprRecord(
                    crate::aver_generated::domain::ast::Expr__ExprRecord(name, fields.reverse()),
                ),
                pos2.add(&aver_rt::AverInt::from_i64(1)),
            ));
        }
        _ => {
            return Err(::aver_rt::str_lit!(
                "Expected ',' or ')' in record constructor"
            ));
        }
    }
}

/// Allow a trailing comma before ')' in record constructors.
pub fn parseRecordAfterComma(
    tokens @ _: &aver_rt::AverList<crate::aver_generated::domain::token::Token>,
    mut pos @ _: aver_rt::AverInt,
    mut name @ _: AverStr,
    mut fields @ _: aver_rt::AverList<(AverStr, crate::aver_generated::domain::ast::Expr)>,
) -> Result<(crate::aver_generated::domain::ast::Expr, aver_rt::AverInt), AverStr> {
    crate::cancel_checkpoint();
    match crate::aver_generated::domain::parser_match::tokenAt(&*tokens, pos.clone()) {
        crate::aver_generated::domain::token::Token::TkRParen => {
            return Ok((
                crate::aver_generated::domain::ast::Expr::ExprRecord(
                    crate::aver_generated::domain::ast::Expr__ExprRecord(name, fields),
                ),
                pos.add(&aver_rt::AverInt::from_i64(1)),
            ));
        }
        _ => {
            return __mutual_tco_trampoline_8(
                __MutualTco8::ParseRecordFields(pos, name, fields),
                tokens,
            );
        }
    }
}

#[allow(non_camel_case_types)]
enum __MutualTco9 {
    ParseCallArgsList(
        aver_rt::AverInt,
        AverStr,
        aver_rt::AverList<crate::aver_generated::domain::ast::Expr>,
    ),
    ParseCallArgsListTail(
        aver_rt::AverInt,
        AverStr,
        aver_rt::AverList<crate::aver_generated::domain::ast::Expr>,
    ),
    ParseCallArgsAfterComma(
        aver_rt::AverInt,
        AverStr,
        aver_rt::AverList<crate::aver_generated::domain::ast::Expr>,
    ),
    ParseCallArgsCheckNamed(
        aver_rt::AverInt,
        AverStr,
        aver_rt::AverList<crate::aver_generated::domain::ast::Expr>,
        AverStr,
    ),
}

fn __mutual_tco_trampoline_9(
    mut __state: __MutualTco9,
    tokens @ _: &aver_rt::AverList<crate::aver_generated::domain::token::Token>,
) -> Result<(crate::aver_generated::domain::ast::Expr, aver_rt::AverInt), AverStr> {
    loop {
        __state = match __state {
            __MutualTco9::ParseCallArgsList(mut pos @ _, mut name @ _, mut acc @ _) => {
                crate::cancel_checkpoint();
                let r @ _ = crate::aver_generated::domain::parser::expr::parseExpr(&*tokens, pos)?;
                {
                    let (expr, pos2) = r;
                    {
                        let __arg7 @ _ = aver_rt::AverList::prepend_owned(expr, acc);
                        __MutualTco9::ParseCallArgsListTail(pos2, name, __arg7)
                    }
                }
            }
            __MutualTco9::ParseCallArgsListTail(mut pos0 @ _, mut name @ _, mut args @ _) => {
                crate::cancel_checkpoint();
                let pos @ _ = crate::aver_generated::domain::parser::expr::skipNl(&*tokens, pos0);
                match crate::aver_generated::domain::parser_match::tokenAt(&*tokens, pos.clone()) {
                    crate::aver_generated::domain::token::Token::TkComma => {
                        let __arg5 @ _ = crate::aver_generated::domain::parser::expr::skipNl(
                            &*tokens,
                            pos.add(&aver_rt::AverInt::from_i64(1)),
                        );
                        __MutualTco9::ParseCallArgsAfterComma(__arg5, name, args)
                    }
                    crate::aver_generated::domain::token::Token::TkRParen => {
                        return Ok((
                            crate::aver_generated::domain::ast::Expr::ExprCall(
                                crate::aver_generated::domain::ast::Expr__ExprCall(
                                    name,
                                    args.reverse(),
                                ),
                            ),
                            pos.add(&aver_rt::AverInt::from_i64(1)),
                        ));
                    }
                    _ => return Err(::aver_rt::str_lit!("Expected ',' or ')' in argument list")),
                }
            }
            __MutualTco9::ParseCallArgsAfterComma(mut pos @ _, mut name @ _, mut args @ _) => {
                crate::cancel_checkpoint();
                match crate::aver_generated::domain::parser_match::tokenAt(&*tokens, pos.clone()) {
                    crate::aver_generated::domain::token::Token::TkRParen => {
                        return Ok((
                            crate::aver_generated::domain::ast::Expr::ExprCall(
                                crate::aver_generated::domain::ast::Expr__ExprCall(
                                    name,
                                    args.reverse(),
                                ),
                            ),
                            pos.add(&aver_rt::AverInt::from_i64(1)),
                        ));
                    }
                    crate::aver_generated::domain::token::Token::TkIdent(field) => {
                        __MutualTco9::ParseCallArgsCheckNamed(pos, name, args, field)
                    }
                    _ => __MutualTco9::ParseCallArgsList(pos, name, args),
                }
            }
            __MutualTco9::ParseCallArgsCheckNamed(
                mut pos @ _,
                mut name @ _,
                mut args @ _,
                mut field @ _,
            ) => {
                crate::cancel_checkpoint();
                {
                    let __subject = crate::aver_generated::domain::parser_match::tokenAt(
                        &*tokens,
                        pos.add(&aver_rt::AverInt::from_i64(1)),
                    );
                    match __subject {
                        crate::aver_generated::domain::token::Token::TkEq => {
                            let __arg5 @ _ = <aver_rt::AverList<(
                                AverStr,
                                crate::aver_generated::domain::ast::Expr,
                            )>>::empty();
                            return crate::aver_generated::domain::parser::expr::parseNamedArgs(
                                &*tokens, pos, name, &args, __arg5,
                            );
                        }
                        _ => __MutualTco9::ParseCallArgsList(pos, name, args),
                    }
                }
            }
        };
    }
}

/// Parse comma-separated argument list.
pub fn parseCallArgsList(
    tokens @ _: &aver_rt::AverList<crate::aver_generated::domain::token::Token>,
    mut pos @ _: aver_rt::AverInt,
    mut name @ _: AverStr,
    mut acc @ _: aver_rt::AverList<crate::aver_generated::domain::ast::Expr>,
) -> Result<(crate::aver_generated::domain::ast::Expr, aver_rt::AverInt), AverStr> {
    crate::cancel_checkpoint();
    let r @ _ = crate::aver_generated::domain::parser::expr::parseExpr(&*tokens, pos)?;
    {
        let (expr, pos2) = r;
        {
            let __arg7 @ _ = aver_rt::AverList::prepend_owned(expr, acc);
            return __mutual_tco_trampoline_9(
                __MutualTco9::ParseCallArgsListTail(pos2, name, __arg7),
                tokens,
            );
        }
    }
}

/// After an argument: ',' for more or ')' to end. Detects named args (field = expr).
pub fn parseCallArgsListTail(
    tokens @ _: &aver_rt::AverList<crate::aver_generated::domain::token::Token>,
    mut pos0 @ _: aver_rt::AverInt,
    mut name @ _: AverStr,
    mut args @ _: aver_rt::AverList<crate::aver_generated::domain::ast::Expr>,
) -> Result<(crate::aver_generated::domain::ast::Expr, aver_rt::AverInt), AverStr> {
    crate::cancel_checkpoint();
    let pos @ _ = crate::aver_generated::domain::parser::expr::skipNl(&*tokens, pos0);
    match crate::aver_generated::domain::parser_match::tokenAt(&*tokens, pos.clone()) {
        crate::aver_generated::domain::token::Token::TkComma => {
            let __arg5 @ _ = crate::aver_generated::domain::parser::expr::skipNl(
                &*tokens,
                pos.add(&aver_rt::AverInt::from_i64(1)),
            );
            return __mutual_tco_trampoline_9(
                __MutualTco9::ParseCallArgsAfterComma(__arg5, name, args),
                tokens,
            );
        }
        crate::aver_generated::domain::token::Token::TkRParen => {
            return Ok((
                crate::aver_generated::domain::ast::Expr::ExprCall(
                    crate::aver_generated::domain::ast::Expr__ExprCall(name, args.reverse()),
                ),
                pos.add(&aver_rt::AverInt::from_i64(1)),
            ));
        }
        _ => return Err(::aver_rt::str_lit!("Expected ',' or ')' in argument list")),
    }
}

/// After comma in call args: check if next is ident = (named arg) or regular expr.
pub fn parseCallArgsAfterComma(
    tokens @ _: &aver_rt::AverList<crate::aver_generated::domain::token::Token>,
    mut pos @ _: aver_rt::AverInt,
    mut name @ _: AverStr,
    mut args @ _: aver_rt::AverList<crate::aver_generated::domain::ast::Expr>,
) -> Result<(crate::aver_generated::domain::ast::Expr, aver_rt::AverInt), AverStr> {
    crate::cancel_checkpoint();
    match crate::aver_generated::domain::parser_match::tokenAt(&*tokens, pos.clone()) {
        crate::aver_generated::domain::token::Token::TkRParen => {
            return Ok((
                crate::aver_generated::domain::ast::Expr::ExprCall(
                    crate::aver_generated::domain::ast::Expr__ExprCall(name, args.reverse()),
                ),
                pos.add(&aver_rt::AverInt::from_i64(1)),
            ));
        }
        crate::aver_generated::domain::token::Token::TkIdent(field) => {
            return __mutual_tco_trampoline_9(
                __MutualTco9::ParseCallArgsCheckNamed(pos, name, args, field),
                tokens,
            );
        }
        _ => {
            return __mutual_tco_trampoline_9(
                __MutualTco9::ParseCallArgsList(pos, name, args),
                tokens,
            );
        }
    }
}

/// Lookahead: if next token after ident is =, switch to named args mode.
pub fn parseCallArgsCheckNamed(
    tokens @ _: &aver_rt::AverList<crate::aver_generated::domain::token::Token>,
    mut pos @ _: aver_rt::AverInt,
    mut name @ _: AverStr,
    mut args @ _: aver_rt::AverList<crate::aver_generated::domain::ast::Expr>,
    mut field @ _: AverStr,
) -> Result<(crate::aver_generated::domain::ast::Expr, aver_rt::AverInt), AverStr> {
    crate::cancel_checkpoint();
    {
        let __subject = crate::aver_generated::domain::parser_match::tokenAt(
            &*tokens,
            pos.add(&aver_rt::AverInt::from_i64(1)),
        );
        match __subject {
            crate::aver_generated::domain::token::Token::TkEq => {
                let __arg5 @ _ = <aver_rt::AverList<(
                    AverStr,
                    crate::aver_generated::domain::ast::Expr,
                )>>::empty();
                return crate::aver_generated::domain::parser::expr::parseNamedArgs(
                    &*tokens, pos, name, &args, __arg5,
                );
            }
            _ => {
                return __mutual_tco_trampoline_9(
                    __MutualTco9::ParseCallArgsList(pos, name, args),
                    tokens,
                );
            }
        }
    }
}

#[allow(non_camel_case_types)]
enum __MutualTco10 {
    ParseNamedArgs(
        aver_rt::AverInt,
        AverStr,
        aver_rt::AverList<(AverStr, crate::aver_generated::domain::ast::Expr)>,
    ),
    ParseNamedArgField(
        aver_rt::AverInt,
        AverStr,
        aver_rt::AverList<(AverStr, crate::aver_generated::domain::ast::Expr)>,
        AverStr,
    ),
    ParseNamedArgsTail(
        aver_rt::AverInt,
        AverStr,
        aver_rt::AverList<(AverStr, crate::aver_generated::domain::ast::Expr)>,
    ),
}

fn __mutual_tco_trampoline_10(
    mut __state: __MutualTco10,
    tokens @ _: &aver_rt::AverList<crate::aver_generated::domain::token::Token>,
    positionalArgs @ _: &aver_rt::AverList<crate::aver_generated::domain::ast::Expr>,
) -> Result<(crate::aver_generated::domain::ast::Expr, aver_rt::AverInt), AverStr> {
    loop {
        __state = match __state {
            __MutualTco10::ParseNamedArgs(mut pos @ _, mut name @ _, mut namedAcc @ _) => {
                crate::cancel_checkpoint();
                match crate::aver_generated::domain::parser_match::tokenAt(&*tokens, pos.clone()) {
                    crate::aver_generated::domain::token::Token::TkIdent(field) => {
                        __MutualTco10::ParseNamedArgField(
                            pos.add(&aver_rt::AverInt::from_i64(1)),
                            name,
                            namedAcc,
                            field,
                        )
                    }
                    crate::aver_generated::domain::token::Token::TkRParen => {
                        return Ok((
                            crate::aver_generated::domain::ast::Expr::ExprCall(
                                crate::aver_generated::domain::ast::Expr__ExprCall(
                                    name,
                                    aver_rt::AverList::concat(
                                        &positionalArgs.reverse(),
                                        &aver_rt::AverList::from_vec(vec![
                                        crate::aver_generated::domain::ast::Expr::ExprRecord(
                                            crate::aver_generated::domain::ast::Expr__ExprRecord(
                                                ::aver_rt::str_lit!("_named"),
                                                namedAcc.reverse(),
                                            ),
                                        ),
                                    ]),
                                    ),
                                ),
                            ),
                            pos.add(&aver_rt::AverInt::from_i64(1)),
                        ));
                    }
                    _ => {
                        return Err(::aver_rt::str_lit!(
                            "Expected field name or ')' in named arguments"
                        ));
                    }
                }
            }
            __MutualTco10::ParseNamedArgField(
                mut pos @ _,
                mut name @ _,
                mut namedAcc @ _,
                mut field @ _,
            ) => {
                crate::cancel_checkpoint();
                let pos2 @ _ = {
                    let __arg10 @ _ = crate::aver_generated::domain::token::Token::TkEq;
                    crate::aver_generated::domain::parser_match::expect(&*tokens, pos, &__arg10)
                }?;
                let r @ _ = crate::aver_generated::domain::parser::expr::parseExpr(&*tokens, pos2)?;
                {
                    let (expr, pos3) = r;
                    {
                        let __arg11 @ _ = aver_rt::AverList::prepend_owned((field, expr), namedAcc);
                        __MutualTco10::ParseNamedArgsTail(pos3, name, __arg11)
                    }
                }
            }
            __MutualTco10::ParseNamedArgsTail(mut pos0 @ _, mut name @ _, mut namedAcc @ _) => {
                crate::cancel_checkpoint();
                let pos @ _ = crate::aver_generated::domain::parser::expr::skipNl(&*tokens, pos0);
                match crate::aver_generated::domain::parser_match::tokenAt(&*tokens, pos.clone()) {
                    crate::aver_generated::domain::token::Token::TkComma => {
                        let __arg6 @ _ = crate::aver_generated::domain::parser::expr::skipNl(
                            &*tokens,
                            pos.add(&aver_rt::AverInt::from_i64(1)),
                        );
                        __MutualTco10::ParseNamedArgs(__arg6, name, namedAcc)
                    }
                    crate::aver_generated::domain::token::Token::TkRParen => {
                        return Ok((
                            crate::aver_generated::domain::ast::Expr::ExprCall(
                                crate::aver_generated::domain::ast::Expr__ExprCall(
                                    name,
                                    aver_rt::AverList::concat(
                                        &positionalArgs.reverse(),
                                        &aver_rt::AverList::from_vec(vec![
                                        crate::aver_generated::domain::ast::Expr::ExprRecord(
                                            crate::aver_generated::domain::ast::Expr__ExprRecord(
                                                ::aver_rt::str_lit!("_named"),
                                                namedAcc.reverse(),
                                            ),
                                        ),
                                    ]),
                                    ),
                                ),
                            ),
                            pos.add(&aver_rt::AverInt::from_i64(1)),
                        ));
                    }
                    _ => {
                        return Err(::aver_rt::str_lit!(
                            "Expected ',' or ')' in named arguments"
                        ));
                    }
                }
            }
        };
    }
}

/// Parse field = expr pairs for named/record-update args.
pub fn parseNamedArgs(
    tokens @ _: &aver_rt::AverList<crate::aver_generated::domain::token::Token>,
    mut pos @ _: aver_rt::AverInt,
    mut name @ _: AverStr,
    positionalArgs @ _: &aver_rt::AverList<crate::aver_generated::domain::ast::Expr>,
    mut namedAcc @ _: aver_rt::AverList<(AverStr, crate::aver_generated::domain::ast::Expr)>,
) -> Result<(crate::aver_generated::domain::ast::Expr, aver_rt::AverInt), AverStr> {
    crate::cancel_checkpoint();
    match crate::aver_generated::domain::parser_match::tokenAt(&*tokens, pos.clone()) {
        crate::aver_generated::domain::token::Token::TkIdent(field) => {
            return __mutual_tco_trampoline_10(
                __MutualTco10::ParseNamedArgField(
                    pos.add(&aver_rt::AverInt::from_i64(1)),
                    name,
                    namedAcc,
                    field,
                ),
                tokens,
                positionalArgs,
            );
        }
        crate::aver_generated::domain::token::Token::TkRParen => {
            return Ok((
                crate::aver_generated::domain::ast::Expr::ExprCall(
                    crate::aver_generated::domain::ast::Expr__ExprCall(
                        name,
                        aver_rt::AverList::concat(
                            &positionalArgs.reverse(),
                            &aver_rt::AverList::from_vec(vec![
                                crate::aver_generated::domain::ast::Expr::ExprRecord(
                                    crate::aver_generated::domain::ast::Expr__ExprRecord(
                                        ::aver_rt::str_lit!("_named"),
                                        namedAcc.reverse(),
                                    ),
                                ),
                            ]),
                        ),
                    ),
                ),
                pos.add(&aver_rt::AverInt::from_i64(1)),
            ));
        }
        _ => {
            return Err(::aver_rt::str_lit!(
                "Expected field name or ')' in named arguments"
            ));
        }
    }
}

/// Parse = expr after field name in named args.
pub fn parseNamedArgField(
    tokens @ _: &aver_rt::AverList<crate::aver_generated::domain::token::Token>,
    mut pos @ _: aver_rt::AverInt,
    mut name @ _: AverStr,
    positionalArgs @ _: &aver_rt::AverList<crate::aver_generated::domain::ast::Expr>,
    mut namedAcc @ _: aver_rt::AverList<(AverStr, crate::aver_generated::domain::ast::Expr)>,
    mut field @ _: AverStr,
) -> Result<(crate::aver_generated::domain::ast::Expr, aver_rt::AverInt), AverStr> {
    crate::cancel_checkpoint();
    let pos2 @ _ = {
        let __arg10 @ _ = crate::aver_generated::domain::token::Token::TkEq;
        crate::aver_generated::domain::parser_match::expect(&*tokens, pos, &__arg10)
    }?;
    let r @ _ = crate::aver_generated::domain::parser::expr::parseExpr(&*tokens, pos2)?;
    {
        let (expr, pos3) = r;
        {
            let __arg11 @ _ = aver_rt::AverList::prepend_owned((field, expr), namedAcc);
            return __mutual_tco_trampoline_10(
                __MutualTco10::ParseNamedArgsTail(pos3, name, __arg11),
                tokens,
                positionalArgs,
            );
        }
    }
}

/// After named arg: ',' for more or ')' to end.
pub fn parseNamedArgsTail(
    tokens @ _: &aver_rt::AverList<crate::aver_generated::domain::token::Token>,
    mut pos0 @ _: aver_rt::AverInt,
    mut name @ _: AverStr,
    positionalArgs @ _: &aver_rt::AverList<crate::aver_generated::domain::ast::Expr>,
    mut namedAcc @ _: aver_rt::AverList<(AverStr, crate::aver_generated::domain::ast::Expr)>,
) -> Result<(crate::aver_generated::domain::ast::Expr, aver_rt::AverInt), AverStr> {
    crate::cancel_checkpoint();
    let pos @ _ = crate::aver_generated::domain::parser::expr::skipNl(&*tokens, pos0);
    match crate::aver_generated::domain::parser_match::tokenAt(&*tokens, pos.clone()) {
        crate::aver_generated::domain::token::Token::TkComma => {
            let __arg6 @ _ = crate::aver_generated::domain::parser::expr::skipNl(
                &*tokens,
                pos.add(&aver_rt::AverInt::from_i64(1)),
            );
            return __mutual_tco_trampoline_10(
                __MutualTco10::ParseNamedArgs(__arg6, name, namedAcc),
                tokens,
                positionalArgs,
            );
        }
        crate::aver_generated::domain::token::Token::TkRParen => {
            return Ok((
                crate::aver_generated::domain::ast::Expr::ExprCall(
                    crate::aver_generated::domain::ast::Expr__ExprCall(
                        name,
                        aver_rt::AverList::concat(
                            &positionalArgs.reverse(),
                            &aver_rt::AverList::from_vec(vec![
                                crate::aver_generated::domain::ast::Expr::ExprRecord(
                                    crate::aver_generated::domain::ast::Expr__ExprRecord(
                                        ::aver_rt::str_lit!("_named"),
                                        namedAcc.reverse(),
                                    ),
                                ),
                            ]),
                        ),
                    ),
                ),
                pos.add(&aver_rt::AverInt::from_i64(1)),
            ));
        }
        _ => {
            return Err(::aver_rt::str_lit!(
                "Expected ',' or ')' in named arguments"
            ));
        }
    }
}

#[allow(non_camel_case_types)]
enum __MutualTco11 {
    ParseMatchArms(
        aver_rt::AverInt,
        aver_rt::AverList<crate::aver_generated::domain::ast::MatchArm>,
    ),
    ParseOneArm(
        aver_rt::AverInt,
        aver_rt::AverList<crate::aver_generated::domain::ast::MatchArm>,
    ),
    ParseOneArmBody(
        aver_rt::AverInt,
        aver_rt::AverList<crate::aver_generated::domain::ast::MatchArm>,
        crate::aver_generated::domain::ast::Pattern,
    ),
}

fn __mutual_tco_trampoline_11(
    mut __state: __MutualTco11,
    tokens @ _: &aver_rt::AverList<crate::aver_generated::domain::token::Token>,
    subject @ _: &crate::aver_generated::domain::ast::Expr,
) -> Result<(crate::aver_generated::domain::ast::Expr, aver_rt::AverInt), AverStr> {
    loop {
        __state = match __state {
            __MutualTco11::ParseMatchArms(mut pos @ _, mut acc @ _) => {
                crate::cancel_checkpoint();
                let pos2 @ _ =
                    crate::aver_generated::domain::parser_match::skipNewlines(&*tokens, pos);
                match crate::aver_generated::domain::parser_match::tokenAt(&*tokens, pos2.clone()) {
                    crate::aver_generated::domain::token::Token::TkDedent => {
                        let __list_subject = acc.clone();
                        if __list_subject.is_empty() {
                            return Err(::aver_rt::str_lit!("Expected at least one match arm"));
                        } else {
                            return Ok((
                                crate::aver_generated::domain::ast::Expr::ExprMatch(
                                    crate::aver_generated::domain::ast::Expr__ExprMatch(
                                        (*subject).clone(),
                                        acc.reverse(),
                                    ),
                                ),
                                pos2.add(&aver_rt::AverInt::from_i64(1)),
                            ));
                        }
                    }
                    crate::aver_generated::domain::token::Token::TkEof => {
                        let __list_subject = acc.clone();
                        if __list_subject.is_empty() {
                            return Err(::aver_rt::str_lit!("Expected at least one match arm"));
                        } else {
                            return Ok((
                                crate::aver_generated::domain::ast::Expr::ExprMatch(
                                    crate::aver_generated::domain::ast::Expr__ExprMatch(
                                        (*subject).clone(),
                                        acc.reverse(),
                                    ),
                                ),
                                pos2,
                            ));
                        }
                    }
                    _ => __MutualTco11::ParseOneArm(pos2, acc),
                }
            }
            __MutualTco11::ParseOneArm(mut pos @ _, mut acc @ _) => {
                crate::cancel_checkpoint();
                let pr @ _ =
                    crate::aver_generated::domain::parser_match::parsePattern(&*tokens, pos)?;
                {
                    let (pat, pos2) = pr;
                    __MutualTco11::ParseOneArmBody(pos2, acc, pat)
                }
            }
            __MutualTco11::ParseOneArmBody(mut pos @ _, mut acc @ _, mut pat @ _) => {
                crate::cancel_checkpoint();
                let pos2 @ _ = {
                    let __arg9 @ _ = crate::aver_generated::domain::token::Token::TkArrow;
                    crate::aver_generated::domain::parser_match::expect(&*tokens, pos, &__arg9)
                }?;
                let er @ _ =
                    crate::aver_generated::domain::parser::expr::parseExpr(&*tokens, pos2)?;
                {
                    let (body, pos3) = er;
                    {
                        let __arg10 @ _ = crate::aver_generated::domain::parser_match::skipNewlines(
                            &*tokens, pos3,
                        );
                        {
                            let __arg11 @ _ = aver_rt::AverList::prepend_owned(
                                crate::aver_generated::domain::ast::MatchArm {
                                    pattern: pat,
                                    body: body,
                                    bindingSlots: HashMap::new(),
                                },
                                acc,
                            );
                            __MutualTco11::ParseMatchArms(__arg10, __arg11)
                        }
                    }
                }
            }
        };
    }
}

/// Parse match arms until DEDENT.
pub fn parseMatchArms(
    tokens @ _: &aver_rt::AverList<crate::aver_generated::domain::token::Token>,
    mut pos @ _: aver_rt::AverInt,
    subject @ _: &crate::aver_generated::domain::ast::Expr,
    mut acc @ _: aver_rt::AverList<crate::aver_generated::domain::ast::MatchArm>,
) -> Result<(crate::aver_generated::domain::ast::Expr, aver_rt::AverInt), AverStr> {
    crate::cancel_checkpoint();
    let pos2 @ _ = crate::aver_generated::domain::parser_match::skipNewlines(&*tokens, pos);
    match crate::aver_generated::domain::parser_match::tokenAt(&*tokens, pos2.clone()) {
        crate::aver_generated::domain::token::Token::TkDedent => {
            let __list_subject = acc.clone();
            if __list_subject.is_empty() {
                return Err(::aver_rt::str_lit!("Expected at least one match arm"));
            } else {
                return Ok((
                    crate::aver_generated::domain::ast::Expr::ExprMatch(
                        crate::aver_generated::domain::ast::Expr__ExprMatch(
                            (*subject).clone(),
                            acc.reverse(),
                        ),
                    ),
                    pos2.add(&aver_rt::AverInt::from_i64(1)),
                ));
            }
        }
        crate::aver_generated::domain::token::Token::TkEof => {
            let __list_subject = acc.clone();
            if __list_subject.is_empty() {
                return Err(::aver_rt::str_lit!("Expected at least one match arm"));
            } else {
                return Ok((
                    crate::aver_generated::domain::ast::Expr::ExprMatch(
                        crate::aver_generated::domain::ast::Expr__ExprMatch(
                            (*subject).clone(),
                            acc.reverse(),
                        ),
                    ),
                    pos2,
                ));
            }
        }
        _ => {
            return __mutual_tco_trampoline_11(
                __MutualTco11::ParseOneArm(pos2, acc),
                tokens,
                subject,
            );
        }
    }
}

/// Parse one match arm: pattern '->' expr NEWLINE
pub fn parseOneArm(
    tokens @ _: &aver_rt::AverList<crate::aver_generated::domain::token::Token>,
    mut pos @ _: aver_rt::AverInt,
    subject @ _: &crate::aver_generated::domain::ast::Expr,
    mut acc @ _: aver_rt::AverList<crate::aver_generated::domain::ast::MatchArm>,
) -> Result<(crate::aver_generated::domain::ast::Expr, aver_rt::AverInt), AverStr> {
    crate::cancel_checkpoint();
    let pr @ _ = crate::aver_generated::domain::parser_match::parsePattern(&*tokens, pos)?;
    {
        let (pat, pos2) = pr;
        return __mutual_tco_trampoline_11(
            __MutualTco11::ParseOneArmBody(pos2, acc, pat),
            tokens,
            subject,
        );
    }
}

/// Parse arrow and body of a match arm.
pub fn parseOneArmBody(
    tokens @ _: &aver_rt::AverList<crate::aver_generated::domain::token::Token>,
    mut pos @ _: aver_rt::AverInt,
    subject @ _: &crate::aver_generated::domain::ast::Expr,
    mut acc @ _: aver_rt::AverList<crate::aver_generated::domain::ast::MatchArm>,
    mut pat @ _: crate::aver_generated::domain::ast::Pattern,
) -> Result<(crate::aver_generated::domain::ast::Expr, aver_rt::AverInt), AverStr> {
    crate::cancel_checkpoint();
    let pos2 @ _ = {
        let __arg9 @ _ = crate::aver_generated::domain::token::Token::TkArrow;
        crate::aver_generated::domain::parser_match::expect(&*tokens, pos, &__arg9)
    }?;
    let er @ _ = crate::aver_generated::domain::parser::expr::parseExpr(&*tokens, pos2)?;
    {
        let (body, pos3) = er;
        {
            let __arg10 @ _ =
                crate::aver_generated::domain::parser_match::skipNewlines(&*tokens, pos3);
            {
                let __arg11 @ _ = aver_rt::AverList::prepend_owned(
                    crate::aver_generated::domain::ast::MatchArm {
                        pattern: pat,
                        body: body,
                        bindingSlots: HashMap::new(),
                    },
                    acc,
                );
                return __mutual_tco_trampoline_11(
                    __MutualTco11::ParseMatchArms(__arg10, __arg11),
                    tokens,
                    subject,
                );
            }
        }
    }
}

#[allow(non_camel_case_types)]
enum __MutualTco12 {
    ParseMatchArmsFlat(
        aver_rt::AverInt,
        aver_rt::AverList<crate::aver_generated::domain::ast::MatchArm>,
    ),
    ParseOneArmFlat(
        aver_rt::AverInt,
        aver_rt::AverList<crate::aver_generated::domain::ast::MatchArm>,
    ),
    ParseOneArmFlatBody(
        aver_rt::AverInt,
        aver_rt::AverList<crate::aver_generated::domain::ast::MatchArm>,
        crate::aver_generated::domain::ast::Pattern,
    ),
}

fn __mutual_tco_trampoline_12(
    mut __state: __MutualTco12,
    tokens @ _: &aver_rt::AverList<crate::aver_generated::domain::token::Token>,
    subject @ _: &crate::aver_generated::domain::ast::Expr,
) -> Result<(crate::aver_generated::domain::ast::Expr, aver_rt::AverInt), AverStr> {
    loop {
        __state = match __state {
            __MutualTco12::ParseMatchArmsFlat(mut pos @ _, mut acc @ _) => {
                crate::cancel_checkpoint();
                if crate::aver_generated::domain::parser_match::isArmStart(&*tokens, pos.clone()) {
                    __MutualTco12::ParseOneArmFlat(pos, acc)
                } else {
                    {
                        let __list_subject = acc.clone();
                        if __list_subject.is_empty() {
                            return Err(::aver_rt::str_lit!("Expected at least one match arm"));
                        } else {
                            return Ok((
                                crate::aver_generated::domain::ast::Expr::ExprMatch(
                                    crate::aver_generated::domain::ast::Expr__ExprMatch(
                                        (*subject).clone(),
                                        acc.reverse(),
                                    ),
                                ),
                                pos,
                            ));
                        }
                    }
                }
            }
            __MutualTco12::ParseOneArmFlat(mut pos @ _, mut acc @ _) => {
                crate::cancel_checkpoint();
                let pr @ _ =
                    crate::aver_generated::domain::parser_match::parsePattern(&*tokens, pos)?;
                {
                    let (pat, pos2) = pr;
                    __MutualTco12::ParseOneArmFlatBody(pos2, acc, pat)
                }
            }
            __MutualTco12::ParseOneArmFlatBody(mut pos @ _, mut acc @ _, mut pat @ _) => {
                crate::cancel_checkpoint();
                let pos2 @ _ = {
                    let __arg9 @ _ = crate::aver_generated::domain::token::Token::TkArrow;
                    crate::aver_generated::domain::parser_match::expect(&*tokens, pos, &__arg9)
                }?;
                let er @ _ =
                    crate::aver_generated::domain::parser::expr::parseExpr(&*tokens, pos2)?;
                {
                    let (body, pos3) = er;
                    {
                        let __arg10 @ _ = crate::aver_generated::domain::parser_match::skipNewlines(
                            &*tokens, pos3,
                        );
                        {
                            let __arg11 @ _ = aver_rt::AverList::prepend_owned(
                                crate::aver_generated::domain::ast::MatchArm {
                                    pattern: pat,
                                    body: body,
                                    bindingSlots: HashMap::new(),
                                },
                                acc,
                            );
                            __MutualTco12::ParseMatchArmsFlat(__arg10, __arg11)
                        }
                    }
                }
            }
        };
    }
}

/// Parse match arms without INDENT/DEDENT (fallback for flat token streams).
pub fn parseMatchArmsFlat(
    tokens @ _: &aver_rt::AverList<crate::aver_generated::domain::token::Token>,
    mut pos @ _: aver_rt::AverInt,
    subject @ _: &crate::aver_generated::domain::ast::Expr,
    mut acc @ _: aver_rt::AverList<crate::aver_generated::domain::ast::MatchArm>,
) -> Result<(crate::aver_generated::domain::ast::Expr, aver_rt::AverInt), AverStr> {
    crate::cancel_checkpoint();
    if crate::aver_generated::domain::parser_match::isArmStart(&*tokens, pos.clone()) {
        return __mutual_tco_trampoline_12(
            __MutualTco12::ParseOneArmFlat(pos, acc),
            tokens,
            subject,
        );
    } else {
        {
            let __list_subject = acc.clone();
            if __list_subject.is_empty() {
                return Err(::aver_rt::str_lit!("Expected at least one match arm"));
            } else {
                return Ok((
                    crate::aver_generated::domain::ast::Expr::ExprMatch(
                        crate::aver_generated::domain::ast::Expr__ExprMatch(
                            (*subject).clone(),
                            acc.reverse(),
                        ),
                    ),
                    pos,
                ));
            }
        }
    }
}

/// Parse one arm and continue in flat mode.
pub fn parseOneArmFlat(
    tokens @ _: &aver_rt::AverList<crate::aver_generated::domain::token::Token>,
    mut pos @ _: aver_rt::AverInt,
    subject @ _: &crate::aver_generated::domain::ast::Expr,
    mut acc @ _: aver_rt::AverList<crate::aver_generated::domain::ast::MatchArm>,
) -> Result<(crate::aver_generated::domain::ast::Expr, aver_rt::AverInt), AverStr> {
    crate::cancel_checkpoint();
    let pr @ _ = crate::aver_generated::domain::parser_match::parsePattern(&*tokens, pos)?;
    {
        let (pat, pos2) = pr;
        return __mutual_tco_trampoline_12(
            __MutualTco12::ParseOneArmFlatBody(pos2, acc, pat),
            tokens,
            subject,
        );
    }
}

/// Parse arrow and body, then continue flat.
pub fn parseOneArmFlatBody(
    tokens @ _: &aver_rt::AverList<crate::aver_generated::domain::token::Token>,
    mut pos @ _: aver_rt::AverInt,
    subject @ _: &crate::aver_generated::domain::ast::Expr,
    mut acc @ _: aver_rt::AverList<crate::aver_generated::domain::ast::MatchArm>,
    mut pat @ _: crate::aver_generated::domain::ast::Pattern,
) -> Result<(crate::aver_generated::domain::ast::Expr, aver_rt::AverInt), AverStr> {
    crate::cancel_checkpoint();
    let pos2 @ _ = {
        let __arg9 @ _ = crate::aver_generated::domain::token::Token::TkArrow;
        crate::aver_generated::domain::parser_match::expect(&*tokens, pos, &__arg9)
    }?;
    let er @ _ = crate::aver_generated::domain::parser::expr::parseExpr(&*tokens, pos2)?;
    {
        let (body, pos3) = er;
        {
            let __arg10 @ _ =
                crate::aver_generated::domain::parser_match::skipNewlines(&*tokens, pos3);
            {
                let __arg11 @ _ = aver_rt::AverList::prepend_owned(
                    crate::aver_generated::domain::ast::MatchArm {
                        pattern: pat,
                        body: body,
                        bindingSlots: HashMap::new(),
                    },
                    acc,
                );
                return __mutual_tco_trampoline_12(
                    __MutualTco12::ParseMatchArmsFlat(__arg10, __arg11),
                    tokens,
                    subject,
                );
            }
        }
    }
}

/// Parse an expression: comparison level, then optional ? postfix.
pub fn parseExpr(
    tokens @ _: &aver_rt::AverList<crate::aver_generated::domain::token::Token>,
    pos @ _: aver_rt::AverInt,
) -> Result<(crate::aver_generated::domain::ast::Expr, aver_rt::AverInt), AverStr> {
    crate::cancel_checkpoint();
    let r @ _ = crate::aver_generated::domain::parser::expr::parseCmpExpr(tokens, pos)?;
    {
        let (expr, pos2) = r;
        Ok(crate::aver_generated::domain::parser::expr::parsePostfixQuestion(tokens, pos2, expr))
    }
}

/// Check for postfix ? (error propagation).
pub fn parsePostfixQuestion(
    tokens @ _: &aver_rt::AverList<crate::aver_generated::domain::token::Token>,
    pos @ _: aver_rt::AverInt,
    mut expr @ _: crate::aver_generated::domain::ast::Expr,
) -> (crate::aver_generated::domain::ast::Expr, aver_rt::AverInt) {
    crate::cancel_checkpoint();
    match crate::aver_generated::domain::parser_match::continuationAt(tokens, pos.clone()) {
        crate::aver_generated::domain::token::Token::TkQuestion => (
            crate::aver_generated::domain::ast::Expr::ExprPropagate(
                crate::aver_generated::domain::ast::Expr__ExprPropagate(expr),
            ),
            pos.add(&aver_rt::AverInt::from_i64(1)),
        ),
        _ => (expr, pos),
    }
}

/// Parse comparison: addExpr (('==' | '<' | '>') addExpr)?
pub fn parseCmpExpr(
    tokens @ _: &aver_rt::AverList<crate::aver_generated::domain::token::Token>,
    pos @ _: aver_rt::AverInt,
) -> Result<(crate::aver_generated::domain::ast::Expr, aver_rt::AverInt), AverStr> {
    crate::cancel_checkpoint();
    let r @ _ = crate::aver_generated::domain::parser::expr::parseAddExpr(tokens, pos)?;
    {
        let (left, pos2) = r;
        crate::aver_generated::domain::parser::expr::parseCmpExprTail(tokens, pos2, left)
    }
}

/// Check for comparison operator.
pub fn parseCmpExprTail(
    tokens @ _: &aver_rt::AverList<crate::aver_generated::domain::token::Token>,
    pos @ _: aver_rt::AverInt,
    mut left @ _: crate::aver_generated::domain::ast::Expr,
) -> Result<(crate::aver_generated::domain::ast::Expr, aver_rt::AverInt), AverStr> {
    crate::cancel_checkpoint();
    let nextPos @ _ = pos.add(&aver_rt::AverInt::from_i64(1));
    match crate::aver_generated::domain::parser_match::continuationAt(tokens, pos.clone()) {
        crate::aver_generated::domain::token::Token::TkEqEq => {
            crate::aver_generated::domain::parser::expr::parseCmpExprRight(
                tokens,
                nextPos,
                left,
                aver_rt::AverInt::from_i64(0),
            )
        }
        crate::aver_generated::domain::token::Token::TkNeq => {
            crate::aver_generated::domain::parser::expr::parseCmpExprRight(
                tokens,
                nextPos,
                left,
                aver_rt::AverInt::from_i64(1),
            )
        }
        crate::aver_generated::domain::token::Token::TkLt => {
            crate::aver_generated::domain::parser::expr::parseCmpExprRight(
                tokens,
                nextPos,
                left,
                aver_rt::AverInt::from_i64(2),
            )
        }
        crate::aver_generated::domain::token::Token::TkGt => {
            crate::aver_generated::domain::parser::expr::parseCmpExprRight(
                tokens,
                nextPos,
                left,
                aver_rt::AverInt::from_i64(3),
            )
        }
        crate::aver_generated::domain::token::Token::TkLte => {
            crate::aver_generated::domain::parser::expr::parseCmpExprRight(
                tokens,
                nextPos,
                left,
                aver_rt::AverInt::from_i64(4),
            )
        }
        crate::aver_generated::domain::token::Token::TkGte => {
            crate::aver_generated::domain::parser::expr::parseCmpExprRight(
                tokens,
                nextPos,
                left,
                aver_rt::AverInt::from_i64(5),
            )
        }
        _ => Ok((left, pos)),
    }
}

/// Parse right side of comparison.
pub fn parseCmpExprRight(
    tokens @ _: &aver_rt::AverList<crate::aver_generated::domain::token::Token>,
    pos @ _: aver_rt::AverInt,
    mut left @ _: crate::aver_generated::domain::ast::Expr,
    op @ _: aver_rt::AverInt,
) -> Result<(crate::aver_generated::domain::ast::Expr, aver_rt::AverInt), AverStr> {
    crate::cancel_checkpoint();
    let r @ _ = crate::aver_generated::domain::parser::expr::parseAddExpr(tokens, pos)?;
    {
        let (right, pos2) = r;
        crate::aver_generated::domain::parser::expr::buildCmpExpr(left, right, pos2, op)
    }
}

/// Build comparison AST node from op code.
#[inline(always)]
pub fn buildCmpExpr(
    mut left @ _: crate::aver_generated::domain::ast::Expr,
    mut right @ _: crate::aver_generated::domain::ast::Expr,
    pos @ _: aver_rt::AverInt,
    op @ _: aver_rt::AverInt,
) -> Result<(crate::aver_generated::domain::ast::Expr, aver_rt::AverInt), AverStr> {
    crate::cancel_checkpoint();
    {
        let __dispatch_subject = op;
        if __dispatch_subject == aver_rt::AverInt::from_i64(0) {
            Ok((
                crate::aver_generated::domain::ast::Expr::ExprEq(
                    crate::aver_generated::domain::ast::Expr__ExprEq(left, right),
                ),
                pos,
            ))
        } else {
            if __dispatch_subject == aver_rt::AverInt::from_i64(1) {
                Ok((
                    crate::aver_generated::domain::ast::Expr::ExprNeq(
                        crate::aver_generated::domain::ast::Expr__ExprNeq(left, right),
                    ),
                    pos,
                ))
            } else {
                if __dispatch_subject == aver_rt::AverInt::from_i64(2) {
                    Ok((
                        crate::aver_generated::domain::ast::Expr::ExprLt(
                            crate::aver_generated::domain::ast::Expr__ExprLt(left, right),
                        ),
                        pos,
                    ))
                } else {
                    if __dispatch_subject == aver_rt::AverInt::from_i64(3) {
                        Ok((
                            crate::aver_generated::domain::ast::Expr::ExprGt(
                                crate::aver_generated::domain::ast::Expr__ExprGt(left, right),
                            ),
                            pos,
                        ))
                    } else {
                        if __dispatch_subject == aver_rt::AverInt::from_i64(4) {
                            Ok((
                                crate::aver_generated::domain::ast::Expr::ExprLte(
                                    crate::aver_generated::domain::ast::Expr__ExprLte(left, right),
                                ),
                                pos,
                            ))
                        } else {
                            if __dispatch_subject == aver_rt::AverInt::from_i64(5) {
                                Ok((
                                    crate::aver_generated::domain::ast::Expr::ExprGte(
                                        crate::aver_generated::domain::ast::Expr__ExprGte(
                                            left, right,
                                        ),
                                    ),
                                    pos,
                                ))
                            } else {
                                Err(::aver_rt::str_lit!("unknown cmp operator"))
                            }
                        }
                    }
                }
            }
        }
    }
}

/// Parse additive: mulExpr (('+' | '-') mulExpr)*
pub fn parseAddExpr(
    tokens @ _: &aver_rt::AverList<crate::aver_generated::domain::token::Token>,
    pos @ _: aver_rt::AverInt,
) -> Result<(crate::aver_generated::domain::ast::Expr, aver_rt::AverInt), AverStr> {
    crate::cancel_checkpoint();
    let r @ _ = crate::aver_generated::domain::parser::expr::parseMulExpr(tokens, pos)?;
    {
        let (expr, pos2) = r;
        crate::aver_generated::domain::parser::expr::parseAddExprTail(tokens, pos2, expr)
    }
}

/// Parse multiplicative: atom ('*' atom)*
pub fn parseMulExpr(
    tokens @ _: &aver_rt::AverList<crate::aver_generated::domain::token::Token>,
    pos @ _: aver_rt::AverInt,
) -> Result<(crate::aver_generated::domain::ast::Expr, aver_rt::AverInt), AverStr> {
    crate::cancel_checkpoint();
    let r @ _ = crate::aver_generated::domain::parser::expr::parseAtom(tokens, pos)?;
    {
        let (expr, pos2) = r;
        crate::aver_generated::domain::parser::expr::parseMulExprTail(tokens, pos2, expr)
    }
}

/// Parse atomic expression: literal, variable, call, match, or parenthesized.
pub fn parseAtom(
    tokens @ _: &aver_rt::AverList<crate::aver_generated::domain::token::Token>,
    pos @ _: aver_rt::AverInt,
) -> Result<(crate::aver_generated::domain::ast::Expr, aver_rt::AverInt), AverStr> {
    crate::cancel_checkpoint();
    let nextPos @ _ = pos.add(&aver_rt::AverInt::from_i64(1));
    let t @ _ = crate::aver_generated::domain::parser_match::tokenAt(tokens, pos);
    match &t {
        crate::aver_generated::domain::token::Token::TkMinus => {
            crate::aver_generated::domain::parser::expr::parseNegAtom(tokens, nextPos)
        }
        crate::aver_generated::domain::token::Token::TkInt(n) => {
            let n = n.clone();
            Ok((
                crate::aver_generated::domain::ast::Expr::ExprInt(
                    crate::aver_generated::domain::ast::Expr__ExprInt(n),
                ),
                nextPos,
            ))
        }
        crate::aver_generated::domain::token::Token::TkFloat(f) => {
            let f = f.clone();
            Ok((
                crate::aver_generated::domain::ast::Expr::ExprFloat(
                    crate::aver_generated::domain::ast::Expr__ExprFloat(f),
                ),
                nextPos,
            ))
        }
        crate::aver_generated::domain::token::Token::TkStr(s) => {
            let s = s.clone();
            crate::aver_generated::domain::parser::expr::parseStringOrInterp(tokens, nextPos, s)
        }
        crate::aver_generated::domain::token::Token::TkTrue => Ok((
            crate::aver_generated::domain::ast::Expr::ExprBool(
                crate::aver_generated::domain::ast::Expr__ExprBool(true),
            ),
            nextPos,
        )),
        crate::aver_generated::domain::token::Token::TkFalse => Ok((
            crate::aver_generated::domain::ast::Expr::ExprBool(
                crate::aver_generated::domain::ast::Expr__ExprBool(false),
            ),
            nextPos,
        )),
        crate::aver_generated::domain::token::Token::TkLBracket => {
            crate::aver_generated::domain::parser::expr::parseListExpr(tokens, nextPos)
        }
        crate::aver_generated::domain::token::Token::TkLBrace => {
            crate::aver_generated::domain::parser::expr::parseMapLiteral(tokens, nextPos)
        }
        crate::aver_generated::domain::token::Token::TkLParen => {
            crate::aver_generated::domain::parser::expr::parseParenExpr(tokens, nextPos)
        }
        crate::aver_generated::domain::token::Token::TkMatch => {
            crate::aver_generated::domain::parser::expr::parseMatchExpr(tokens, nextPos)
        }
        crate::aver_generated::domain::token::Token::TkIdent(name) => {
            let name = name.clone();
            crate::aver_generated::domain::parser::expr::parseIdentOrCall(tokens, nextPos, name)
        }
        _ => Err(aver_rt::AverStr::from({
            let mut __b = {
                let mut __b = aver_rt::Buffer::with_capacity(
                    (aver_rt::AverInt::from_i64(41)).to_usize().unwrap_or(0),
                );
                __b.push_str(&::aver_rt::str_lit!("Expected expression, got "));
                __b
            };
            __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                &(crate::aver_generated::domain::token::tokenRepr(&t)),
            )));
            __b
        })),
    }
}

/// Parse map literal into Map.fromList call.
pub fn parseMapLiteral(
    tokens @ _: &aver_rt::AverList<crate::aver_generated::domain::token::Token>,
    pos @ _: aver_rt::AverInt,
) -> Result<(crate::aver_generated::domain::ast::Expr, aver_rt::AverInt), AverStr> {
    crate::cancel_checkpoint();
    let pos2 @ _ = crate::aver_generated::domain::parser::expr::skipNl(tokens, pos);
    match crate::aver_generated::domain::parser_match::tokenAt(tokens, pos2.clone()) {
        crate::aver_generated::domain::token::Token::TkRBrace => Ok((
            crate::aver_generated::domain::ast::Expr::ExprCall(
                crate::aver_generated::domain::ast::Expr__ExprCall(
                    ::aver_rt::str_lit!("Map.fromList"),
                    aver_rt::AverList::from_vec(vec![
                        crate::aver_generated::domain::ast::Expr::ExprList(
                            crate::aver_generated::domain::ast::Expr__ExprList(
                                aver_rt::AverList::empty(),
                            ),
                        ),
                    ]),
                ),
            ),
            pos2.add(&aver_rt::AverInt::from_i64(1)),
        )),
        _ => {
            let __arg3 @ _ = <aver_rt::AverList<crate::aver_generated::domain::ast::Expr>>::empty();
            crate::aver_generated::domain::parser::expr::parseMapEntries(tokens, pos2, __arg3)
        }
    }
}

/// Parse unary minus into the first-class Expr.ExprNeg node.
pub fn parseNegAtom(
    tokens @ _: &aver_rt::AverList<crate::aver_generated::domain::token::Token>,
    pos @ _: aver_rt::AverInt,
) -> Result<(crate::aver_generated::domain::ast::Expr, aver_rt::AverInt), AverStr> {
    crate::cancel_checkpoint();
    let r @ _ = crate::aver_generated::domain::parser::expr::parseAtom(tokens, pos)?;
    {
        let (expr, pos2) = r;
        Ok((
            crate::aver_generated::domain::ast::Expr::ExprNeg(
                crate::aver_generated::domain::ast::Expr__ExprNeg(expr),
            ),
            pos2,
        ))
    }
}

/// After TkStr: check for interpolation (TkInterpStart) or plain string.
pub fn parseStringOrInterp(
    tokens @ _: &aver_rt::AverList<crate::aver_generated::domain::token::Token>,
    pos @ _: aver_rt::AverInt,
    prefix @ _: AverStr,
) -> Result<(crate::aver_generated::domain::ast::Expr, aver_rt::AverInt), AverStr> {
    crate::cancel_checkpoint();
    match crate::aver_generated::domain::parser_match::tokenAt(tokens, pos.clone()) {
        crate::aver_generated::domain::token::Token::TkInterpStart => {
            crate::aver_generated::domain::parser::expr::parseInterpParts(
                tokens,
                pos.add(&aver_rt::AverInt::from_i64(1)),
                aver_rt::AverList::from_vec(vec![
                    crate::aver_generated::domain::ast::Expr::ExprStr(
                        crate::aver_generated::domain::ast::Expr__ExprStr(prefix),
                    ),
                ]),
            )
        }
        _ => Ok((
            crate::aver_generated::domain::ast::Expr::ExprStr(
                crate::aver_generated::domain::ast::Expr__ExprStr(prefix),
            ),
            pos,
        )),
    }
}

/// Parse list literal: [expr, expr, ...] or [].
pub fn parseListExpr(
    tokens @ _: &aver_rt::AverList<crate::aver_generated::domain::token::Token>,
    pos0 @ _: aver_rt::AverInt,
) -> Result<(crate::aver_generated::domain::ast::Expr, aver_rt::AverInt), AverStr> {
    crate::cancel_checkpoint();
    let pos @ _ = crate::aver_generated::domain::parser::expr::skipNl(tokens, pos0);
    match crate::aver_generated::domain::parser_match::tokenAt(tokens, pos.clone()) {
        crate::aver_generated::domain::token::Token::TkRBracket => Ok((
            crate::aver_generated::domain::ast::Expr::ExprList(
                crate::aver_generated::domain::ast::Expr__ExprList(aver_rt::AverList::empty()),
            ),
            pos.add(&aver_rt::AverInt::from_i64(1)),
        )),
        _ => {
            let __arg3 @ _ = <aver_rt::AverList<crate::aver_generated::domain::ast::Expr>>::empty();
            crate::aver_generated::domain::parser::expr::parseListItems(tokens, pos, __arg3)
        }
    }
}

/// Parse parenthesized expression or tuple.
pub fn parseParenExpr(
    tokens @ _: &aver_rt::AverList<crate::aver_generated::domain::token::Token>,
    pos @ _: aver_rt::AverInt,
) -> Result<(crate::aver_generated::domain::ast::Expr, aver_rt::AverInt), AverStr> {
    crate::cancel_checkpoint();
    let r @ _ = crate::aver_generated::domain::parser::expr::parseExpr(tokens, pos)?;
    {
        let (expr, pos2) = r;
        match crate::aver_generated::domain::parser_match::tokenAt(tokens, pos2.clone()) {
            crate::aver_generated::domain::token::Token::TkComma => {
                crate::aver_generated::domain::parser::expr::parseTupleRest(
                    tokens,
                    pos2.add(&aver_rt::AverInt::from_i64(1)),
                    aver_rt::AverList::from_vec(vec![expr]),
                )
            }
            crate::aver_generated::domain::token::Token::TkRParen => {
                Ok((expr, pos2.add(&aver_rt::AverInt::from_i64(1))))
            }
            _ => Err(::aver_rt::str_lit!("Expected ')' or ',' after expression")),
        }
    }
}

/// After tuple closing ')': check for ?! or ! postfix to form independent product.
pub fn finishTupleOrProduct(
    tokens @ _: &aver_rt::AverList<crate::aver_generated::domain::token::Token>,
    pos @ _: aver_rt::AverInt,
    items @ _: &aver_rt::AverList<crate::aver_generated::domain::ast::Expr>,
) -> Result<(crate::aver_generated::domain::ast::Expr, aver_rt::AverInt), AverStr> {
    crate::cancel_checkpoint();
    match crate::aver_generated::domain::parser_match::tokenAt(tokens, pos.clone()) {
        crate::aver_generated::domain::token::Token::TkQuestion => {
            let __subject = crate::aver_generated::domain::parser_match::tokenAt(
                tokens,
                pos.add(&aver_rt::AverInt::from_i64(1)),
            );
            match __subject {
                crate::aver_generated::domain::token::Token::TkBang => Ok((
                    crate::aver_generated::domain::ast::Expr::ExprIndependentProduct(
                        crate::aver_generated::domain::ast::Expr__ExprIndependentProduct(
                            items.clone(),
                            true,
                        ),
                    ),
                    pos.add(&aver_rt::AverInt::from_i64(2)),
                )),
                _ => Ok((
                    crate::aver_generated::domain::ast::Expr::ExprTuple(
                        crate::aver_generated::domain::ast::Expr__ExprTuple(items.clone()),
                    ),
                    pos,
                )),
            }
        }
        crate::aver_generated::domain::token::Token::TkBang => {
            let __subject = crate::aver_generated::domain::parser_match::tokenAt(
                tokens,
                pos.add(&aver_rt::AverInt::from_i64(1)),
            );
            match __subject {
                crate::aver_generated::domain::token::Token::TkLBracket => Ok((
                    crate::aver_generated::domain::ast::Expr::ExprTuple(
                        crate::aver_generated::domain::ast::Expr__ExprTuple(items.clone()),
                    ),
                    pos,
                )),
                _ => Ok((
                    crate::aver_generated::domain::ast::Expr::ExprIndependentProduct(
                        crate::aver_generated::domain::ast::Expr__ExprIndependentProduct(
                            items.clone(),
                            false,
                        ),
                    ),
                    pos.add(&aver_rt::AverInt::from_i64(1)),
                )),
            }
        }
        _ => Ok((
            crate::aver_generated::domain::ast::Expr::ExprTuple(
                crate::aver_generated::domain::ast::Expr__ExprTuple(items.clone()),
            ),
            pos,
        )),
    }
}

/// After reading an identifier: call, record constructor, field access, or variable?
pub fn parseIdentOrCall(
    tokens @ _: &aver_rt::AverList<crate::aver_generated::domain::token::Token>,
    pos @ _: aver_rt::AverInt,
    name @ _: AverStr,
) -> Result<(crate::aver_generated::domain::ast::Expr, aver_rt::AverInt), AverStr> {
    crate::cancel_checkpoint();
    let nextPos @ _ = pos.add(&aver_rt::AverInt::from_i64(1));
    match crate::aver_generated::domain::parser_match::tokenAt(tokens, pos.clone()) {
        crate::aver_generated::domain::token::Token::TkLParen => {
            crate::aver_generated::domain::parser::expr::chainFieldAccess(tokens, nextPos, name)
        }
        crate::aver_generated::domain::token::Token::TkDot => {
            let __arg4 @ _ = crate::aver_generated::domain::ast::Expr::ExprVar(
                crate::aver_generated::domain::ast::Expr__ExprVar(name),
            );
            crate::aver_generated::domain::parser::expr::parseFieldAccess(tokens, nextPos, __arg4)
        }
        _ => Ok((
            crate::aver_generated::domain::ast::Expr::ExprVar(
                crate::aver_generated::domain::ast::Expr__ExprVar(name),
            ),
            pos,
        )),
    }
}

/// Parse call/record then chain .field access if present.
pub fn chainFieldAccess(
    tokens @ _: &aver_rt::AverList<crate::aver_generated::domain::token::Token>,
    pos @ _: aver_rt::AverInt,
    name @ _: AverStr,
) -> Result<(crate::aver_generated::domain::ast::Expr, aver_rt::AverInt), AverStr> {
    crate::cancel_checkpoint();
    let r @ _ = crate::aver_generated::domain::parser::expr::parseCallOrRecord(tokens, pos, name)?;
    {
        let (expr, pos2) = r;
        crate::aver_generated::domain::parser::expr::parseFieldAccessTail(tokens, pos2, expr)
    }
}

/// Skip newlines and indents inside parenthesized expressions.
pub fn skipNl(
    tokens @ _: &aver_rt::AverList<crate::aver_generated::domain::token::Token>,
    mut pos @ _: aver_rt::AverInt,
) -> aver_rt::AverInt {
    loop {
        crate::cancel_checkpoint();
        let nextPos @ _ = pos.add(&aver_rt::AverInt::from_i64(1));
        match crate::aver_generated::domain::parser_match::tokenAt(tokens, pos.clone()) {
            crate::aver_generated::domain::token::Token::TkNewline => {
                let __tco1 = nextPos;
                pos = __tco1;
                continue;
            }
            crate::aver_generated::domain::token::Token::TkIndent => {
                let __tco1 = nextPos;
                pos = __tco1;
                continue;
            }
            crate::aver_generated::domain::token::Token::TkDedent => {
                let __tco1 = nextPos;
                pos = __tco1;
                continue;
            }
            _ => {
                return pos;
            }
        }
    }
}

/// After '(': is this a function call f(args) or record Foo(field = val)?
pub fn parseCallOrRecord(
    tokens @ _: &aver_rt::AverList<crate::aver_generated::domain::token::Token>,
    pos @ _: aver_rt::AverInt,
    name @ _: AverStr,
) -> Result<(crate::aver_generated::domain::ast::Expr, aver_rt::AverInt), AverStr> {
    crate::cancel_checkpoint();
    let pos2 @ _ = crate::aver_generated::domain::parser::expr::skipNl(tokens, pos);
    match crate::aver_generated::domain::parser_match::tokenAt(tokens, pos2.clone()) {
        crate::aver_generated::domain::token::Token::TkRParen => Ok((
            crate::aver_generated::domain::ast::Expr::ExprCall(
                crate::aver_generated::domain::ast::Expr__ExprCall(
                    name,
                    aver_rt::AverList::empty(),
                ),
            ),
            pos2.add(&aver_rt::AverInt::from_i64(1)),
        )),
        crate::aver_generated::domain::token::Token::TkIdent(first) => {
            crate::aver_generated::domain::parser::expr::parseCallOrRecordLookahead(
                tokens, pos2, name, &first,
            )
        }
        _ => {
            let __arg5 @ _ = <aver_rt::AverList<crate::aver_generated::domain::ast::Expr>>::empty();
            crate::aver_generated::domain::parser::expr::parseCallArgsList(
                tokens, pos2, name, __arg5,
            )
        }
    }
}

/// Lookahead after first ident in parens: '=' means record, otherwise call.
pub fn parseCallOrRecordLookahead(
    tokens @ _: &aver_rt::AverList<crate::aver_generated::domain::token::Token>,
    pos @ _: aver_rt::AverInt,
    name @ _: AverStr,
    first @ _: &AverStr,
) -> Result<(crate::aver_generated::domain::ast::Expr, aver_rt::AverInt), AverStr> {
    crate::cancel_checkpoint();
    {
        let __subject = crate::aver_generated::domain::parser_match::tokenAt(
            tokens,
            pos.add(&aver_rt::AverInt::from_i64(1)),
        );
        match __subject {
            crate::aver_generated::domain::token::Token::TkEq => {
                let __arg4 @ _ = <aver_rt::AverList<(
                    AverStr,
                    crate::aver_generated::domain::ast::Expr,
                )>>::empty();
                crate::aver_generated::domain::parser::expr::parseRecordFields(
                    tokens, pos, name, __arg4,
                )
            }
            _ => {
                let __arg5 @ _ =
                    <aver_rt::AverList<crate::aver_generated::domain::ast::Expr>>::empty();
                crate::aver_generated::domain::parser::expr::parseCallArgsList(
                    tokens, pos, name, __arg5,
                )
            }
        }
    }
}

/// Parse: match expr NEWLINE INDENT arms DEDENT. Called after TkMatch is consumed.
pub fn parseMatchExpr(
    tokens @ _: &aver_rt::AverList<crate::aver_generated::domain::token::Token>,
    pos @ _: aver_rt::AverInt,
) -> Result<(crate::aver_generated::domain::ast::Expr, aver_rt::AverInt), AverStr> {
    crate::cancel_checkpoint();
    let r @ _ = crate::aver_generated::domain::parser::expr::parseExpr(tokens, pos)?;
    {
        let (subject, pos2) = r;
        crate::aver_generated::domain::parser::expr::parseMatchAfterSubject(tokens, pos2, &subject)
    }
}

/// Skip newlines, consume INDENT, then parse arms.
pub fn parseMatchAfterSubject(
    tokens @ _: &aver_rt::AverList<crate::aver_generated::domain::token::Token>,
    pos @ _: aver_rt::AverInt,
    subject @ _: &crate::aver_generated::domain::ast::Expr,
) -> Result<(crate::aver_generated::domain::ast::Expr, aver_rt::AverInt), AverStr> {
    crate::cancel_checkpoint();
    let pos2 @ _ = crate::aver_generated::domain::parser_match::skipNewlines(tokens, pos);
    match crate::aver_generated::domain::parser_match::tokenAt(tokens, pos2.clone()) {
        crate::aver_generated::domain::token::Token::TkIndent => {
            crate::aver_generated::domain::parser::expr::parseMatchArms(
                tokens,
                pos2.add(&aver_rt::AverInt::from_i64(1)),
                subject,
                aver_rt::AverList::empty(),
            )
        }
        _ => {
            let __arg4 @ _ =
                <aver_rt::AverList<crate::aver_generated::domain::ast::MatchArm>>::empty();
            crate::aver_generated::domain::parser::expr::parseMatchArmsFlat(
                tokens, pos2, subject, __arg4,
            )
        }
    }
}

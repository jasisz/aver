#[allow(unused_imports)]
use crate::aver_generated::domain::ast::*;
#[allow(unused_imports)]
use crate::aver_generated::domain::parser_match::*;
#[allow(unused_imports)]
use crate::aver_generated::domain::token::*;
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
    tokens: &aver_rt::AverList<crate::aver_generated::domain::token::Token>,
) -> Result<(crate::aver_generated::domain::ast::Expr, aver_rt::AverInt), AverStr> {
    loop {
        __state = match __state {
            __MutualTco1::ParseAddExprTail(mut pos, mut left) => {
                crate::cancel_checkpoint();
                let nextPos @ _ = pos.add(&aver_rt::AverInt::from_i64(1));
                let t @ _ =
                    crate::aver_generated::domain::parser_match::tokenAt(&*tokens, pos.clone());
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
            __MutualTco1::ParseAddExprRight(mut pos, mut left, mut op) => {
                crate::cancel_checkpoint();
                let r @ _ =
                    crate::aver_generated::domain::parser::expr::parseMulExpr(&*tokens, pos)?;
                {
                    let (right, pos2) = r;
                    if (op == aver_rt::AverInt::from_i64(0)) {
                        __MutualTco1::ParseAddExprTail(
                            pos2,
                            crate::aver_generated::domain::ast::Expr::ExprAdd(
                                std::sync::Arc::new(left),
                                std::sync::Arc::new(right),
                            ),
                        )
                    } else {
                        __MutualTco1::ParseAddExprTail(
                            pos2,
                            crate::aver_generated::domain::ast::Expr::ExprSub(
                                std::sync::Arc::new(left),
                                std::sync::Arc::new(right),
                            ),
                        )
                    }
                }
            }
        };
    }
}

/// Continue parsing additive operators.
pub fn parseAddExprTail(
    tokens: &aver_rt::AverList<crate::aver_generated::domain::token::Token>,
    pos: aver_rt::AverInt,
    left: &crate::aver_generated::domain::ast::Expr,
) -> Result<(crate::aver_generated::domain::ast::Expr, aver_rt::AverInt), AverStr> {
    __mutual_tco_trampoline_1(__MutualTco1::ParseAddExprTail(pos, left.clone()), &tokens)
}

/// Parse right side of additive op.
pub fn parseAddExprRight(
    tokens: &aver_rt::AverList<crate::aver_generated::domain::token::Token>,
    pos: aver_rt::AverInt,
    left: &crate::aver_generated::domain::ast::Expr,
    op: aver_rt::AverInt,
) -> Result<(crate::aver_generated::domain::ast::Expr, aver_rt::AverInt), AverStr> {
    __mutual_tco_trampoline_1(
        __MutualTco1::ParseAddExprRight(pos, left.clone(), op),
        &tokens,
    )
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
    tokens: &aver_rt::AverList<crate::aver_generated::domain::token::Token>,
) -> Result<(crate::aver_generated::domain::ast::Expr, aver_rt::AverInt), AverStr> {
    loop {
        __state = match __state {
            __MutualTco2::ParseMulExprTail(mut pos, mut left) => {
                crate::cancel_checkpoint();
                let nextPos @ _ = pos.add(&aver_rt::AverInt::from_i64(1));
                match crate::aver_generated::domain::parser_match::tokenAt(&*tokens, pos.clone()) {
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
            __MutualTco2::ParseMulExprRight(mut pos, mut left, mut op) => {
                crate::cancel_checkpoint();
                let r @ _ = crate::aver_generated::domain::parser::expr::parseAtom(&*tokens, pos)?;
                {
                    let (right, pos2) = r;
                    if (op == aver_rt::AverInt::from_i64(0)) {
                        __MutualTco2::ParseMulExprTail(
                            pos2,
                            crate::aver_generated::domain::ast::Expr::ExprMul(
                                std::sync::Arc::new(left),
                                std::sync::Arc::new(right),
                            ),
                        )
                    } else {
                        __MutualTco2::ParseMulExprTail(
                            pos2,
                            crate::aver_generated::domain::ast::Expr::ExprDiv(
                                std::sync::Arc::new(left),
                                std::sync::Arc::new(right),
                            ),
                        )
                    }
                }
            }
        };
    }
}

/// Continue parsing multiplicative operators.
pub fn parseMulExprTail(
    tokens: &aver_rt::AverList<crate::aver_generated::domain::token::Token>,
    pos: aver_rt::AverInt,
    left: &crate::aver_generated::domain::ast::Expr,
) -> Result<(crate::aver_generated::domain::ast::Expr, aver_rt::AverInt), AverStr> {
    __mutual_tco_trampoline_2(__MutualTco2::ParseMulExprTail(pos, left.clone()), &tokens)
}

/// Parse right side of mul/div op.
pub fn parseMulExprRight(
    tokens: &aver_rt::AverList<crate::aver_generated::domain::token::Token>,
    pos: aver_rt::AverInt,
    left: &crate::aver_generated::domain::ast::Expr,
    op: aver_rt::AverInt,
) -> Result<(crate::aver_generated::domain::ast::Expr, aver_rt::AverInt), AverStr> {
    __mutual_tco_trampoline_2(
        __MutualTco2::ParseMulExprRight(pos, left.clone(), op),
        &tokens,
    )
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
    tokens: &aver_rt::AverList<crate::aver_generated::domain::token::Token>,
) -> Result<(crate::aver_generated::domain::ast::Expr, aver_rt::AverInt), AverStr> {
    loop {
        __state = match __state {
            __MutualTco3::ParseMapEntries(mut pos, mut acc) => {
                crate::cancel_checkpoint();
                let kr @ _ = crate::aver_generated::domain::parser::expr::parseExpr(&*tokens, pos)?;
                {
                    let (keyExpr, pos2) = kr;
                    __MutualTco3::ParseMapAfterKey(pos2, acc, keyExpr)
                }
            }
            __MutualTco3::ParseMapAfterKey(mut pos, mut acc, mut keyExpr) => {
                crate::cancel_checkpoint();
                let pos2 @ _ = crate::aver_generated::domain::parser_match::expect(
                    &*tokens,
                    pos,
                    &crate::aver_generated::domain::token::Token::TkFatArrow,
                )?;
                let vr @ _ =
                    crate::aver_generated::domain::parser::expr::parseExpr(&*tokens, pos2)?;
                {
                    let (valExpr, pos3) = vr;
                    __MutualTco3::ParseMapEntryTail(
                        pos3,
                        aver_rt::AverList::prepend(
                            crate::aver_generated::domain::ast::Expr::ExprTuple(
                                aver_rt::AverList::from_vec(vec![keyExpr, valExpr]),
                            ),
                            &acc,
                        ),
                    )
                }
            }
            __MutualTco3::ParseMapEntryTail(mut pos, mut acc) => {
                crate::cancel_checkpoint();
                let pos2 @ _ =
                    crate::aver_generated::domain::parser::expr::skipNl((*tokens).clone(), pos);
                match crate::aver_generated::domain::parser_match::tokenAt(&*tokens, pos2.clone()) {
                    crate::aver_generated::domain::token::Token::TkComma => {
                        __MutualTco3::ParseMapEntries(
                            crate::aver_generated::domain::parser::expr::skipNl(
                                (*tokens).clone(),
                                pos2.add(&aver_rt::AverInt::from_i64(1)),
                            ),
                            acc,
                        )
                    }
                    crate::aver_generated::domain::token::Token::TkRBrace => {
                        return Ok((
                            crate::aver_generated::domain::ast::Expr::ExprCall(
                                AverStr::from("Map.fromList"),
                                aver_rt::AverList::from_vec(vec![
                                    crate::aver_generated::domain::ast::Expr::ExprList(
                                        acc.reverse(),
                                    ),
                                ]),
                            ),
                            pos2.add(&aver_rt::AverInt::from_i64(1)),
                        ));
                    }
                    _ => return Err(AverStr::from("Expected ',' or '}' in map literal")),
                }
            }
        };
    }
}

/// Parse key => value pairs.
pub fn parseMapEntries(
    tokens: &aver_rt::AverList<crate::aver_generated::domain::token::Token>,
    pos: aver_rt::AverInt,
    acc: &aver_rt::AverList<crate::aver_generated::domain::ast::Expr>,
) -> Result<(crate::aver_generated::domain::ast::Expr, aver_rt::AverInt), AverStr> {
    __mutual_tco_trampoline_3(__MutualTco3::ParseMapEntries(pos, acc.clone()), &tokens)
}

/// After key, expect => then value.
pub fn parseMapAfterKey(
    tokens: &aver_rt::AverList<crate::aver_generated::domain::token::Token>,
    pos: aver_rt::AverInt,
    acc: &aver_rt::AverList<crate::aver_generated::domain::ast::Expr>,
    keyExpr: &crate::aver_generated::domain::ast::Expr,
) -> Result<(crate::aver_generated::domain::ast::Expr, aver_rt::AverInt), AverStr> {
    __mutual_tco_trampoline_3(
        __MutualTco3::ParseMapAfterKey(pos, acc.clone(), keyExpr.clone()),
        &tokens,
    )
}

/// After entry: , for more or } to end.
pub fn parseMapEntryTail(
    tokens: &aver_rt::AverList<crate::aver_generated::domain::token::Token>,
    pos: aver_rt::AverInt,
    acc: &aver_rt::AverList<crate::aver_generated::domain::ast::Expr>,
) -> Result<(crate::aver_generated::domain::ast::Expr, aver_rt::AverInt), AverStr> {
    __mutual_tco_trampoline_3(__MutualTco3::ParseMapEntryTail(pos, acc.clone()), &tokens)
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
    tokens: &aver_rt::AverList<crate::aver_generated::domain::token::Token>,
) -> Result<(crate::aver_generated::domain::ast::Expr, aver_rt::AverInt), AverStr> {
    loop {
        __state = match __state {
            __MutualTco4::ParseInterpParts(mut pos, mut acc) => {
                crate::cancel_checkpoint();
                let r @ _ = crate::aver_generated::domain::parser::expr::parseExpr(&*tokens, pos)?;
                {
                    let (expr, pos2) = r;
                    __MutualTco4::ParseInterpAfterExpr(pos2, aver_rt::AverList::prepend(expr, &acc))
                }
            }
            __MutualTco4::ParseInterpAfterExpr(mut pos, mut acc) => {
                crate::cancel_checkpoint();
                match crate::aver_generated::domain::parser_match::tokenAt(&*tokens, pos.clone()) {
                    crate::aver_generated::domain::token::Token::TkInterpEnd => {
                        __MutualTco4::ParseInterpContinue(
                            pos.add(&aver_rt::AverInt::from_i64(1)),
                            acc,
                        )
                    }
                    _ => return Err(AverStr::from("Expected } in string interpolation")),
                }
            }
            __MutualTco4::ParseInterpContinue(mut pos, mut acc) => {
                crate::cancel_checkpoint();
                let nextPos @ _ = pos.add(&aver_rt::AverInt::from_i64(1));
                match crate::aver_generated::domain::parser_match::tokenAt(&*tokens, pos.clone()) {
                    crate::aver_generated::domain::token::Token::TkStr(s) => {
                        __MutualTco4::ParseInterpAfterStr(
                            nextPos,
                            aver_rt::AverList::prepend(
                                crate::aver_generated::domain::ast::Expr::ExprStr(s),
                                &acc,
                            ),
                        )
                    }
                    crate::aver_generated::domain::token::Token::TkInterpStart => {
                        __MutualTco4::ParseInterpParts(nextPos, acc)
                    }
                    _ => {
                        return Ok((
                            crate::aver_generated::domain::ast::Expr::ExprConcat(acc.reverse()),
                            pos,
                        ));
                    }
                }
            }
            __MutualTco4::ParseInterpAfterStr(mut pos, mut acc) => {
                crate::cancel_checkpoint();
                match crate::aver_generated::domain::parser_match::tokenAt(&*tokens, pos.clone()) {
                    crate::aver_generated::domain::token::Token::TkInterpStart => {
                        __MutualTco4::ParseInterpParts(pos.add(&aver_rt::AverInt::from_i64(1)), acc)
                    }
                    _ => {
                        return Ok((
                            crate::aver_generated::domain::ast::Expr::ExprConcat(acc.reverse()),
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
    tokens: &aver_rt::AverList<crate::aver_generated::domain::token::Token>,
    pos: aver_rt::AverInt,
    acc: &aver_rt::AverList<crate::aver_generated::domain::ast::Expr>,
) -> Result<(crate::aver_generated::domain::ast::Expr, aver_rt::AverInt), AverStr> {
    __mutual_tco_trampoline_4(__MutualTco4::ParseInterpParts(pos, acc.clone()), &tokens)
}

/// After interpolation expr: expect TkInterpEnd, then maybe more string.
pub fn parseInterpAfterExpr(
    tokens: &aver_rt::AverList<crate::aver_generated::domain::token::Token>,
    pos: aver_rt::AverInt,
    acc: &aver_rt::AverList<crate::aver_generated::domain::ast::Expr>,
) -> Result<(crate::aver_generated::domain::ast::Expr, aver_rt::AverInt), AverStr> {
    __mutual_tco_trampoline_4(
        __MutualTco4::ParseInterpAfterExpr(pos, acc.clone()),
        &tokens,
    )
}

/// After }: next TkStr continues, or end of string.
pub fn parseInterpContinue(
    tokens: &aver_rt::AverList<crate::aver_generated::domain::token::Token>,
    pos: aver_rt::AverInt,
    acc: &aver_rt::AverList<crate::aver_generated::domain::ast::Expr>,
) -> Result<(crate::aver_generated::domain::ast::Expr, aver_rt::AverInt), AverStr> {
    __mutual_tco_trampoline_4(__MutualTco4::ParseInterpContinue(pos, acc.clone()), &tokens)
}

/// After string segment: more interpolation or end.
pub fn parseInterpAfterStr(
    tokens: &aver_rt::AverList<crate::aver_generated::domain::token::Token>,
    pos: aver_rt::AverInt,
    acc: &aver_rt::AverList<crate::aver_generated::domain::ast::Expr>,
) -> Result<(crate::aver_generated::domain::ast::Expr, aver_rt::AverInt), AverStr> {
    __mutual_tco_trampoline_4(__MutualTco4::ParseInterpAfterStr(pos, acc.clone()), &tokens)
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
    tokens: &aver_rt::AverList<crate::aver_generated::domain::token::Token>,
) -> Result<(crate::aver_generated::domain::ast::Expr, aver_rt::AverInt), AverStr> {
    loop {
        __state = match __state {
            __MutualTco5::ParseListItems(mut pos, mut acc) => {
                crate::cancel_checkpoint();
                let r @ _ = crate::aver_generated::domain::parser::expr::parseExpr(&*tokens, pos)?;
                {
                    let (expr, pos2) = r;
                    __MutualTco5::ParseListItemsTail(pos2, aver_rt::AverList::prepend(expr, &acc))
                }
            }
            __MutualTco5::ParseListItemsTail(mut pos0, mut items) => {
                crate::cancel_checkpoint();
                let pos @ _ =
                    crate::aver_generated::domain::parser::expr::skipNl((*tokens).clone(), pos0);
                match crate::aver_generated::domain::parser_match::tokenAt(&*tokens, pos.clone()) {
                    crate::aver_generated::domain::token::Token::TkComma => {
                        __MutualTco5::ParseListAfterComma(
                            crate::aver_generated::domain::parser::expr::skipNl(
                                (*tokens).clone(),
                                pos.add(&aver_rt::AverInt::from_i64(1)),
                            ),
                            items,
                        )
                    }
                    crate::aver_generated::domain::token::Token::TkRBracket => {
                        return Ok((
                            crate::aver_generated::domain::ast::Expr::ExprList(items.reverse()),
                            pos.add(&aver_rt::AverInt::from_i64(1)),
                        ));
                    }
                    _ => return Err(AverStr::from("Expected ',' or ']' in list literal")),
                }
            }
            __MutualTco5::ParseListAfterComma(mut pos, mut items) => {
                crate::cancel_checkpoint();
                match crate::aver_generated::domain::parser_match::tokenAt(&*tokens, pos.clone()) {
                    crate::aver_generated::domain::token::Token::TkRBracket => {
                        return Ok((
                            crate::aver_generated::domain::ast::Expr::ExprList(items),
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
    tokens: &aver_rt::AverList<crate::aver_generated::domain::token::Token>,
    pos: aver_rt::AverInt,
    acc: &aver_rt::AverList<crate::aver_generated::domain::ast::Expr>,
) -> Result<(crate::aver_generated::domain::ast::Expr, aver_rt::AverInt), AverStr> {
    __mutual_tco_trampoline_5(__MutualTco5::ParseListItems(pos, acc.clone()), &tokens)
}

/// After a list item: ',' for more or ']' to end.
pub fn parseListItemsTail(
    tokens: &aver_rt::AverList<crate::aver_generated::domain::token::Token>,
    pos0: aver_rt::AverInt,
    items: &aver_rt::AverList<crate::aver_generated::domain::ast::Expr>,
) -> Result<(crate::aver_generated::domain::ast::Expr, aver_rt::AverInt), AverStr> {
    __mutual_tco_trampoline_5(
        __MutualTco5::ParseListItemsTail(pos0, items.clone()),
        &tokens,
    )
}

/// Allow a trailing comma before ']' in list literals.
pub fn parseListAfterComma(
    tokens: &aver_rt::AverList<crate::aver_generated::domain::token::Token>,
    pos: aver_rt::AverInt,
    items: &aver_rt::AverList<crate::aver_generated::domain::ast::Expr>,
) -> Result<(crate::aver_generated::domain::ast::Expr, aver_rt::AverInt), AverStr> {
    __mutual_tco_trampoline_5(
        __MutualTco5::ParseListAfterComma(pos, items.clone()),
        &tokens,
    )
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
    tokens: &aver_rt::AverList<crate::aver_generated::domain::token::Token>,
) -> Result<(crate::aver_generated::domain::ast::Expr, aver_rt::AverInt), AverStr> {
    loop {
        __state = match __state {
            __MutualTco6::ParseTupleRest(mut pos, mut acc) => {
                crate::cancel_checkpoint();
                let r @ _ = crate::aver_generated::domain::parser::expr::parseExpr(&*tokens, pos)?;
                {
                    let (expr, pos2) = r;
                    __MutualTco6::ParseTupleRestTail(pos2, aver_rt::AverList::prepend(expr, &acc))
                }
            }
            __MutualTco6::ParseTupleRestTail(mut pos0, mut items) => {
                crate::cancel_checkpoint();
                let pos @ _ =
                    crate::aver_generated::domain::parser::expr::skipNl((*tokens).clone(), pos0);
                match crate::aver_generated::domain::parser_match::tokenAt(&*tokens, pos.clone()) {
                    crate::aver_generated::domain::token::Token::TkComma => {
                        __MutualTco6::ParseTupleAfterComma(
                            crate::aver_generated::domain::parser::expr::skipNl(
                                (*tokens).clone(),
                                pos.add(&aver_rt::AverInt::from_i64(1)),
                            ),
                            items,
                        )
                    }
                    crate::aver_generated::domain::token::Token::TkRParen => {
                        return crate::aver_generated::domain::parser::expr::finishTupleOrProduct(
                            &*tokens,
                            pos.add(&aver_rt::AverInt::from_i64(1)),
                            &items.reverse(),
                        );
                    }
                    _ => return Err(AverStr::from("Expected ')' or ',' in tuple")),
                }
            }
            __MutualTco6::ParseTupleAfterComma(mut pos, mut items) => {
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
    tokens: &aver_rt::AverList<crate::aver_generated::domain::token::Token>,
    pos: aver_rt::AverInt,
    acc: &aver_rt::AverList<crate::aver_generated::domain::ast::Expr>,
) -> Result<(crate::aver_generated::domain::ast::Expr, aver_rt::AverInt), AverStr> {
    __mutual_tco_trampoline_6(__MutualTco6::ParseTupleRest(pos, acc.clone()), &tokens)
}

/// After tuple element: ',' for more or ')' to end.
pub fn parseTupleRestTail(
    tokens: &aver_rt::AverList<crate::aver_generated::domain::token::Token>,
    pos0: aver_rt::AverInt,
    items: &aver_rt::AverList<crate::aver_generated::domain::ast::Expr>,
) -> Result<(crate::aver_generated::domain::ast::Expr, aver_rt::AverInt), AverStr> {
    __mutual_tco_trampoline_6(
        __MutualTco6::ParseTupleRestTail(pos0, items.clone()),
        &tokens,
    )
}

/// Allow a trailing comma before ')' in tuples.
pub fn parseTupleAfterComma(
    tokens: &aver_rt::AverList<crate::aver_generated::domain::token::Token>,
    pos: aver_rt::AverInt,
    items: &aver_rt::AverList<crate::aver_generated::domain::ast::Expr>,
) -> Result<(crate::aver_generated::domain::ast::Expr, aver_rt::AverInt), AverStr> {
    __mutual_tco_trampoline_6(
        __MutualTco6::ParseTupleAfterComma(pos, items.clone()),
        &tokens,
    )
}

#[allow(non_camel_case_types)]
enum __MutualTco7 {
    ParseFieldAccess(aver_rt::AverInt, crate::aver_generated::domain::ast::Expr),
    ParseFieldAccessTail(aver_rt::AverInt, crate::aver_generated::domain::ast::Expr),
}

fn __mutual_tco_trampoline_7(
    mut __state: __MutualTco7,
    tokens: &aver_rt::AverList<crate::aver_generated::domain::token::Token>,
) -> Result<(crate::aver_generated::domain::ast::Expr, aver_rt::AverInt), AverStr> {
    loop {
        __state = match __state {
            __MutualTco7::ParseFieldAccess(mut pos, mut obj) => {
                crate::cancel_checkpoint();
                match crate::aver_generated::domain::parser_match::tokenAt(&*tokens, pos.clone()) {
                    crate::aver_generated::domain::token::Token::TkIdent(field) => {
                        __MutualTco7::ParseFieldAccessTail(
                            pos.add(&aver_rt::AverInt::from_i64(1)),
                            crate::aver_generated::domain::ast::Expr::ExprFieldAccess(
                                std::sync::Arc::new(obj),
                                field,
                            ),
                        )
                    }
                    _ => return Err(AverStr::from("Expected field name after '.'")),
                }
            }
            __MutualTco7::ParseFieldAccessTail(mut pos, mut expr) => {
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
    tokens: &aver_rt::AverList<crate::aver_generated::domain::token::Token>,
    pos: aver_rt::AverInt,
    obj: &crate::aver_generated::domain::ast::Expr,
) -> Result<(crate::aver_generated::domain::ast::Expr, aver_rt::AverInt), AverStr> {
    __mutual_tco_trampoline_7(__MutualTco7::ParseFieldAccess(pos, obj.clone()), &tokens)
}

/// Check for chained field access.
pub fn parseFieldAccessTail(
    tokens: &aver_rt::AverList<crate::aver_generated::domain::token::Token>,
    pos: aver_rt::AverInt,
    expr: &crate::aver_generated::domain::ast::Expr,
) -> Result<(crate::aver_generated::domain::ast::Expr, aver_rt::AverInt), AverStr> {
    __mutual_tco_trampoline_7(
        __MutualTco7::ParseFieldAccessTail(pos, expr.clone()),
        &tokens,
    )
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
    tokens: &aver_rt::AverList<crate::aver_generated::domain::token::Token>,
) -> Result<(crate::aver_generated::domain::ast::Expr, aver_rt::AverInt), AverStr> {
    loop {
        __state = match __state {
            __MutualTco8::ParseRecordFields(mut pos, mut name, mut acc) => {
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
                    _ => return Err(AverStr::from("Expected field name in record constructor")),
                }
            }
            __MutualTco8::ParseRecordField(mut pos, mut name, mut acc, mut field) => {
                crate::cancel_checkpoint();
                let pos2 @ _ = crate::aver_generated::domain::parser_match::expect(
                    &*tokens,
                    pos,
                    &crate::aver_generated::domain::token::Token::TkEq,
                )?;
                let r @ _ = crate::aver_generated::domain::parser::expr::parseExpr(&*tokens, pos2)?;
                {
                    let (expr, pos3) = r;
                    __MutualTco8::ParseRecordFieldsTail(
                        pos3,
                        name,
                        aver_rt::AverList::prepend((field, expr), &acc),
                    )
                }
            }
            __MutualTco8::ParseRecordFieldsTail(mut pos, mut name, mut fields) => {
                crate::cancel_checkpoint();
                let pos2 @ _ =
                    crate::aver_generated::domain::parser::expr::skipNl((*tokens).clone(), pos);
                match crate::aver_generated::domain::parser_match::tokenAt(&*tokens, pos2.clone()) {
                    crate::aver_generated::domain::token::Token::TkComma => {
                        __MutualTco8::ParseRecordAfterComma(
                            crate::aver_generated::domain::parser::expr::skipNl(
                                (*tokens).clone(),
                                pos2.add(&aver_rt::AverInt::from_i64(1)),
                            ),
                            name,
                            fields,
                        )
                    }
                    crate::aver_generated::domain::token::Token::TkRParen => {
                        return Ok((
                            crate::aver_generated::domain::ast::Expr::ExprRecord(
                                name,
                                fields.reverse(),
                            ),
                            pos2.add(&aver_rt::AverInt::from_i64(1)),
                        ));
                    }
                    _ => return Err(AverStr::from("Expected ',' or ')' in record constructor")),
                }
            }
            __MutualTco8::ParseRecordAfterComma(mut pos, mut name, mut fields) => {
                crate::cancel_checkpoint();
                match crate::aver_generated::domain::parser_match::tokenAt(&*tokens, pos.clone()) {
                    crate::aver_generated::domain::token::Token::TkRParen => {
                        return Ok((
                            crate::aver_generated::domain::ast::Expr::ExprRecord(name, fields),
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
    tokens: &aver_rt::AverList<crate::aver_generated::domain::token::Token>,
    pos: aver_rt::AverInt,
    name: AverStr,
    acc: &aver_rt::AverList<(AverStr, crate::aver_generated::domain::ast::Expr)>,
) -> Result<(crate::aver_generated::domain::ast::Expr, aver_rt::AverInt), AverStr> {
    __mutual_tco_trampoline_8(
        __MutualTco8::ParseRecordFields(pos, name, acc.clone()),
        &tokens,
    )
}

/// Parse = expr after field name.
pub fn parseRecordField(
    tokens: &aver_rt::AverList<crate::aver_generated::domain::token::Token>,
    pos: aver_rt::AverInt,
    name: AverStr,
    acc: &aver_rt::AverList<(AverStr, crate::aver_generated::domain::ast::Expr)>,
    field: AverStr,
) -> Result<(crate::aver_generated::domain::ast::Expr, aver_rt::AverInt), AverStr> {
    __mutual_tco_trampoline_8(
        __MutualTco8::ParseRecordField(pos, name, acc.clone(), field),
        &tokens,
    )
}

/// After a field: ',' for more or ')' to end.
pub fn parseRecordFieldsTail(
    tokens: &aver_rt::AverList<crate::aver_generated::domain::token::Token>,
    pos: aver_rt::AverInt,
    name: AverStr,
    fields: &aver_rt::AverList<(AverStr, crate::aver_generated::domain::ast::Expr)>,
) -> Result<(crate::aver_generated::domain::ast::Expr, aver_rt::AverInt), AverStr> {
    __mutual_tco_trampoline_8(
        __MutualTco8::ParseRecordFieldsTail(pos, name, fields.clone()),
        &tokens,
    )
}

/// Allow a trailing comma before ')' in record constructors.
pub fn parseRecordAfterComma(
    tokens: &aver_rt::AverList<crate::aver_generated::domain::token::Token>,
    pos: aver_rt::AverInt,
    name: AverStr,
    fields: &aver_rt::AverList<(AverStr, crate::aver_generated::domain::ast::Expr)>,
) -> Result<(crate::aver_generated::domain::ast::Expr, aver_rt::AverInt), AverStr> {
    __mutual_tco_trampoline_8(
        __MutualTco8::ParseRecordAfterComma(pos, name, fields.clone()),
        &tokens,
    )
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
    tokens: &aver_rt::AverList<crate::aver_generated::domain::token::Token>,
) -> Result<(crate::aver_generated::domain::ast::Expr, aver_rt::AverInt), AverStr> {
    loop {
        __state = match __state {
            __MutualTco9::ParseCallArgsList(mut pos, mut name, mut acc) => {
                crate::cancel_checkpoint();
                let r @ _ = crate::aver_generated::domain::parser::expr::parseExpr(&*tokens, pos)?;
                {
                    let (expr, pos2) = r;
                    __MutualTco9::ParseCallArgsListTail(
                        pos2,
                        name,
                        aver_rt::AverList::prepend(expr, &acc),
                    )
                }
            }
            __MutualTco9::ParseCallArgsListTail(mut pos0, mut name, mut args) => {
                crate::cancel_checkpoint();
                let pos @ _ =
                    crate::aver_generated::domain::parser::expr::skipNl((*tokens).clone(), pos0);
                match crate::aver_generated::domain::parser_match::tokenAt(&*tokens, pos.clone()) {
                    crate::aver_generated::domain::token::Token::TkComma => {
                        __MutualTco9::ParseCallArgsAfterComma(
                            crate::aver_generated::domain::parser::expr::skipNl(
                                (*tokens).clone(),
                                pos.add(&aver_rt::AverInt::from_i64(1)),
                            ),
                            name,
                            args,
                        )
                    }
                    crate::aver_generated::domain::token::Token::TkRParen => {
                        return Ok((
                            crate::aver_generated::domain::ast::Expr::ExprCall(
                                name,
                                args.reverse(),
                            ),
                            pos.add(&aver_rt::AverInt::from_i64(1)),
                        ));
                    }
                    _ => return Err(AverStr::from("Expected ',' or ')' in argument list")),
                }
            }
            __MutualTco9::ParseCallArgsAfterComma(mut pos, mut name, mut args) => {
                crate::cancel_checkpoint();
                match crate::aver_generated::domain::parser_match::tokenAt(&*tokens, pos.clone()) {
                    crate::aver_generated::domain::token::Token::TkRParen => {
                        return Ok((
                            crate::aver_generated::domain::ast::Expr::ExprCall(
                                name,
                                args.reverse(),
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
            __MutualTco9::ParseCallArgsCheckNamed(mut pos, mut name, mut args, mut field) => {
                crate::cancel_checkpoint();
                match crate::aver_generated::domain::parser_match::tokenAt(
                    &*tokens,
                    pos.add(&aver_rt::AverInt::from_i64(1)),
                ) {
                    crate::aver_generated::domain::token::Token::TkEq => {
                        return crate::aver_generated::domain::parser::expr::parseNamedArgs(
                            &*tokens,
                            pos,
                            name,
                            &args,
                            &aver_rt::AverList::empty(),
                        );
                    }
                    _ => __MutualTco9::ParseCallArgsList(pos, name, args),
                }
            }
        };
    }
}

/// Parse comma-separated argument list.
pub fn parseCallArgsList(
    tokens: &aver_rt::AverList<crate::aver_generated::domain::token::Token>,
    pos: aver_rt::AverInt,
    name: AverStr,
    acc: &aver_rt::AverList<crate::aver_generated::domain::ast::Expr>,
) -> Result<(crate::aver_generated::domain::ast::Expr, aver_rt::AverInt), AverStr> {
    __mutual_tco_trampoline_9(
        __MutualTco9::ParseCallArgsList(pos, name, acc.clone()),
        &tokens,
    )
}

/// After an argument: ',' for more or ')' to end. Detects named args (field = expr).
pub fn parseCallArgsListTail(
    tokens: &aver_rt::AverList<crate::aver_generated::domain::token::Token>,
    pos0: aver_rt::AverInt,
    name: AverStr,
    args: &aver_rt::AverList<crate::aver_generated::domain::ast::Expr>,
) -> Result<(crate::aver_generated::domain::ast::Expr, aver_rt::AverInt), AverStr> {
    __mutual_tco_trampoline_9(
        __MutualTco9::ParseCallArgsListTail(pos0, name, args.clone()),
        &tokens,
    )
}

/// After comma in call args: check if next is ident = (named arg) or regular expr.
pub fn parseCallArgsAfterComma(
    tokens: &aver_rt::AverList<crate::aver_generated::domain::token::Token>,
    pos: aver_rt::AverInt,
    name: AverStr,
    args: &aver_rt::AverList<crate::aver_generated::domain::ast::Expr>,
) -> Result<(crate::aver_generated::domain::ast::Expr, aver_rt::AverInt), AverStr> {
    __mutual_tco_trampoline_9(
        __MutualTco9::ParseCallArgsAfterComma(pos, name, args.clone()),
        &tokens,
    )
}

/// Lookahead: if next token after ident is =, switch to named args mode.
pub fn parseCallArgsCheckNamed(
    tokens: &aver_rt::AverList<crate::aver_generated::domain::token::Token>,
    pos: aver_rt::AverInt,
    name: AverStr,
    args: &aver_rt::AverList<crate::aver_generated::domain::ast::Expr>,
    field: AverStr,
) -> Result<(crate::aver_generated::domain::ast::Expr, aver_rt::AverInt), AverStr> {
    __mutual_tco_trampoline_9(
        __MutualTco9::ParseCallArgsCheckNamed(pos, name, args.clone(), field),
        &tokens,
    )
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
    tokens: &aver_rt::AverList<crate::aver_generated::domain::token::Token>,
    positionalArgs: &aver_rt::AverList<crate::aver_generated::domain::ast::Expr>,
) -> Result<(crate::aver_generated::domain::ast::Expr, aver_rt::AverInt), AverStr> {
    loop {
        __state = match __state {
            __MutualTco10::ParseNamedArgs(mut pos, mut name, mut namedAcc) => {
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
                                name,
                                aver_rt::AverList::concat(
                                    &positionalArgs.reverse(),
                                    &aver_rt::AverList::from_vec(vec![
                                        crate::aver_generated::domain::ast::Expr::ExprRecord(
                                            AverStr::from("_named"),
                                            namedAcc.reverse(),
                                        ),
                                    ]),
                                ),
                            ),
                            pos.add(&aver_rt::AverInt::from_i64(1)),
                        ));
                    }
                    _ => {
                        return Err(AverStr::from(
                            "Expected field name or ')' in named arguments",
                        ));
                    }
                }
            }
            __MutualTco10::ParseNamedArgField(mut pos, mut name, mut namedAcc, mut field) => {
                crate::cancel_checkpoint();
                let pos2 @ _ = crate::aver_generated::domain::parser_match::expect(
                    &*tokens,
                    pos,
                    &crate::aver_generated::domain::token::Token::TkEq,
                )?;
                let r @ _ = crate::aver_generated::domain::parser::expr::parseExpr(&*tokens, pos2)?;
                {
                    let (expr, pos3) = r;
                    __MutualTco10::ParseNamedArgsTail(
                        pos3,
                        name,
                        aver_rt::AverList::prepend((field, expr), &namedAcc),
                    )
                }
            }
            __MutualTco10::ParseNamedArgsTail(mut pos0, mut name, mut namedAcc) => {
                crate::cancel_checkpoint();
                let pos @ _ =
                    crate::aver_generated::domain::parser::expr::skipNl((*tokens).clone(), pos0);
                match crate::aver_generated::domain::parser_match::tokenAt(&*tokens, pos.clone()) {
                    crate::aver_generated::domain::token::Token::TkComma => {
                        __MutualTco10::ParseNamedArgs(
                            crate::aver_generated::domain::parser::expr::skipNl(
                                (*tokens).clone(),
                                pos.add(&aver_rt::AverInt::from_i64(1)),
                            ),
                            name,
                            namedAcc,
                        )
                    }
                    crate::aver_generated::domain::token::Token::TkRParen => {
                        return Ok((
                            crate::aver_generated::domain::ast::Expr::ExprCall(
                                name,
                                aver_rt::AverList::concat(
                                    &positionalArgs.reverse(),
                                    &aver_rt::AverList::from_vec(vec![
                                        crate::aver_generated::domain::ast::Expr::ExprRecord(
                                            AverStr::from("_named"),
                                            namedAcc.reverse(),
                                        ),
                                    ]),
                                ),
                            ),
                            pos.add(&aver_rt::AverInt::from_i64(1)),
                        ));
                    }
                    _ => return Err(AverStr::from("Expected ',' or ')' in named arguments")),
                }
            }
        };
    }
}

/// Parse field = expr pairs for named/record-update args.
pub fn parseNamedArgs(
    tokens: &aver_rt::AverList<crate::aver_generated::domain::token::Token>,
    pos: aver_rt::AverInt,
    name: AverStr,
    positionalArgs: &aver_rt::AverList<crate::aver_generated::domain::ast::Expr>,
    namedAcc: &aver_rt::AverList<(AverStr, crate::aver_generated::domain::ast::Expr)>,
) -> Result<(crate::aver_generated::domain::ast::Expr, aver_rt::AverInt), AverStr> {
    __mutual_tco_trampoline_10(
        __MutualTco10::ParseNamedArgs(pos, name, namedAcc.clone()),
        &tokens,
        &positionalArgs,
    )
}

/// Parse = expr after field name in named args.
pub fn parseNamedArgField(
    tokens: &aver_rt::AverList<crate::aver_generated::domain::token::Token>,
    pos: aver_rt::AverInt,
    name: AverStr,
    positionalArgs: &aver_rt::AverList<crate::aver_generated::domain::ast::Expr>,
    namedAcc: &aver_rt::AverList<(AverStr, crate::aver_generated::domain::ast::Expr)>,
    field: AverStr,
) -> Result<(crate::aver_generated::domain::ast::Expr, aver_rt::AverInt), AverStr> {
    __mutual_tco_trampoline_10(
        __MutualTco10::ParseNamedArgField(pos, name, namedAcc.clone(), field),
        &tokens,
        &positionalArgs,
    )
}

/// After named arg: ',' for more or ')' to end.
pub fn parseNamedArgsTail(
    tokens: &aver_rt::AverList<crate::aver_generated::domain::token::Token>,
    pos0: aver_rt::AverInt,
    name: AverStr,
    positionalArgs: &aver_rt::AverList<crate::aver_generated::domain::ast::Expr>,
    namedAcc: &aver_rt::AverList<(AverStr, crate::aver_generated::domain::ast::Expr)>,
) -> Result<(crate::aver_generated::domain::ast::Expr, aver_rt::AverInt), AverStr> {
    __mutual_tco_trampoline_10(
        __MutualTco10::ParseNamedArgsTail(pos0, name, namedAcc.clone()),
        &tokens,
        &positionalArgs,
    )
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
    tokens: &aver_rt::AverList<crate::aver_generated::domain::token::Token>,
    subject: &crate::aver_generated::domain::ast::Expr,
) -> Result<(crate::aver_generated::domain::ast::Expr, aver_rt::AverInt), AverStr> {
    loop {
        __state = match __state {
            __MutualTco11::ParseMatchArms(mut pos, mut acc) => {
                crate::cancel_checkpoint();
                let pos2 @ _ = crate::aver_generated::domain::parser_match::skipNewlines(
                    (*tokens).clone(),
                    pos,
                );
                match crate::aver_generated::domain::parser_match::tokenAt(&*tokens, pos2.clone()) {
                    crate::aver_generated::domain::token::Token::TkDedent => {
                        let __list_subject = acc.clone();
                        if __list_subject.is_empty() {
                            return Err(AverStr::from("Expected at least one match arm"));
                        } else {
                            return Ok((
                                crate::aver_generated::domain::ast::Expr::ExprMatch(
                                    std::sync::Arc::new((*subject).clone()),
                                    acc.reverse(),
                                ),
                                pos2.add(&aver_rt::AverInt::from_i64(1)),
                            ));
                        }
                    }
                    crate::aver_generated::domain::token::Token::TkEof => {
                        let __list_subject = acc.clone();
                        if __list_subject.is_empty() {
                            return Err(AverStr::from("Expected at least one match arm"));
                        } else {
                            return Ok((
                                crate::aver_generated::domain::ast::Expr::ExprMatch(
                                    std::sync::Arc::new((*subject).clone()),
                                    acc.reverse(),
                                ),
                                pos2,
                            ));
                        }
                    }
                    _ => __MutualTco11::ParseOneArm(pos2, acc),
                }
            }
            __MutualTco11::ParseOneArm(mut pos, mut acc) => {
                crate::cancel_checkpoint();
                let pr @ _ =
                    crate::aver_generated::domain::parser_match::parsePattern(&*tokens, pos)?;
                {
                    let (pat, pos2) = pr;
                    __MutualTco11::ParseOneArmBody(pos2, acc, pat)
                }
            }
            __MutualTco11::ParseOneArmBody(mut pos, mut acc, mut pat) => {
                crate::cancel_checkpoint();
                let pos2 @ _ = crate::aver_generated::domain::parser_match::expect(
                    &*tokens,
                    pos,
                    &crate::aver_generated::domain::token::Token::TkArrow,
                )?;
                let er @ _ =
                    crate::aver_generated::domain::parser::expr::parseExpr(&*tokens, pos2)?;
                {
                    let (body, pos3) = er;
                    __MutualTco11::ParseMatchArms(
                        crate::aver_generated::domain::parser_match::skipNewlines(
                            (*tokens).clone(),
                            pos3,
                        ),
                        aver_rt::AverList::prepend(
                            crate::aver_generated::domain::ast::MatchArm {
                                pattern: pat,
                                body: body,
                                bindingSlots: HashMap::new(),
                            },
                            &acc,
                        ),
                    )
                }
            }
        };
    }
}

/// Parse match arms until DEDENT.
pub fn parseMatchArms(
    tokens: &aver_rt::AverList<crate::aver_generated::domain::token::Token>,
    pos: aver_rt::AverInt,
    subject: &crate::aver_generated::domain::ast::Expr,
    acc: &aver_rt::AverList<crate::aver_generated::domain::ast::MatchArm>,
) -> Result<(crate::aver_generated::domain::ast::Expr, aver_rt::AverInt), AverStr> {
    __mutual_tco_trampoline_11(
        __MutualTco11::ParseMatchArms(pos, acc.clone()),
        &tokens,
        &subject,
    )
}

/// Parse one match arm: pattern '->' expr NEWLINE
pub fn parseOneArm(
    tokens: &aver_rt::AverList<crate::aver_generated::domain::token::Token>,
    pos: aver_rt::AverInt,
    subject: &crate::aver_generated::domain::ast::Expr,
    acc: &aver_rt::AverList<crate::aver_generated::domain::ast::MatchArm>,
) -> Result<(crate::aver_generated::domain::ast::Expr, aver_rt::AverInt), AverStr> {
    __mutual_tco_trampoline_11(
        __MutualTco11::ParseOneArm(pos, acc.clone()),
        &tokens,
        &subject,
    )
}

/// Parse arrow and body of a match arm.
pub fn parseOneArmBody(
    tokens: &aver_rt::AverList<crate::aver_generated::domain::token::Token>,
    pos: aver_rt::AverInt,
    subject: &crate::aver_generated::domain::ast::Expr,
    acc: &aver_rt::AverList<crate::aver_generated::domain::ast::MatchArm>,
    pat: &crate::aver_generated::domain::ast::Pattern,
) -> Result<(crate::aver_generated::domain::ast::Expr, aver_rt::AverInt), AverStr> {
    __mutual_tco_trampoline_11(
        __MutualTco11::ParseOneArmBody(pos, acc.clone(), pat.clone()),
        &tokens,
        &subject,
    )
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
    tokens: &aver_rt::AverList<crate::aver_generated::domain::token::Token>,
    subject: &crate::aver_generated::domain::ast::Expr,
) -> Result<(crate::aver_generated::domain::ast::Expr, aver_rt::AverInt), AverStr> {
    loop {
        __state = match __state {
            __MutualTco12::ParseMatchArmsFlat(mut pos, mut acc) => {
                crate::cancel_checkpoint();
                if crate::aver_generated::domain::parser_match::isArmStart(&*tokens, pos.clone()) {
                    __MutualTco12::ParseOneArmFlat(pos, acc)
                } else {
                    {
                        let __list_subject = acc.clone();
                        if __list_subject.is_empty() {
                            return Err(AverStr::from("Expected at least one match arm"));
                        } else {
                            return Ok((
                                crate::aver_generated::domain::ast::Expr::ExprMatch(
                                    std::sync::Arc::new((*subject).clone()),
                                    acc.reverse(),
                                ),
                                pos,
                            ));
                        }
                    }
                }
            }
            __MutualTco12::ParseOneArmFlat(mut pos, mut acc) => {
                crate::cancel_checkpoint();
                let pr @ _ =
                    crate::aver_generated::domain::parser_match::parsePattern(&*tokens, pos)?;
                {
                    let (pat, pos2) = pr;
                    __MutualTco12::ParseOneArmFlatBody(pos2, acc, pat)
                }
            }
            __MutualTco12::ParseOneArmFlatBody(mut pos, mut acc, mut pat) => {
                crate::cancel_checkpoint();
                let pos2 @ _ = crate::aver_generated::domain::parser_match::expect(
                    &*tokens,
                    pos,
                    &crate::aver_generated::domain::token::Token::TkArrow,
                )?;
                let er @ _ =
                    crate::aver_generated::domain::parser::expr::parseExpr(&*tokens, pos2)?;
                {
                    let (body, pos3) = er;
                    __MutualTco12::ParseMatchArmsFlat(
                        crate::aver_generated::domain::parser_match::skipNewlines(
                            (*tokens).clone(),
                            pos3,
                        ),
                        aver_rt::AverList::prepend(
                            crate::aver_generated::domain::ast::MatchArm {
                                pattern: pat,
                                body: body,
                                bindingSlots: HashMap::new(),
                            },
                            &acc,
                        ),
                    )
                }
            }
        };
    }
}

/// Parse match arms without INDENT/DEDENT (fallback for flat token streams).
pub fn parseMatchArmsFlat(
    tokens: &aver_rt::AverList<crate::aver_generated::domain::token::Token>,
    pos: aver_rt::AverInt,
    subject: &crate::aver_generated::domain::ast::Expr,
    acc: &aver_rt::AverList<crate::aver_generated::domain::ast::MatchArm>,
) -> Result<(crate::aver_generated::domain::ast::Expr, aver_rt::AverInt), AverStr> {
    __mutual_tco_trampoline_12(
        __MutualTco12::ParseMatchArmsFlat(pos, acc.clone()),
        &tokens,
        &subject,
    )
}

/// Parse one arm and continue in flat mode.
pub fn parseOneArmFlat(
    tokens: &aver_rt::AverList<crate::aver_generated::domain::token::Token>,
    pos: aver_rt::AverInt,
    subject: &crate::aver_generated::domain::ast::Expr,
    acc: &aver_rt::AverList<crate::aver_generated::domain::ast::MatchArm>,
) -> Result<(crate::aver_generated::domain::ast::Expr, aver_rt::AverInt), AverStr> {
    __mutual_tco_trampoline_12(
        __MutualTco12::ParseOneArmFlat(pos, acc.clone()),
        &tokens,
        &subject,
    )
}

/// Parse arrow and body, then continue flat.
pub fn parseOneArmFlatBody(
    tokens: &aver_rt::AverList<crate::aver_generated::domain::token::Token>,
    pos: aver_rt::AverInt,
    subject: &crate::aver_generated::domain::ast::Expr,
    acc: &aver_rt::AverList<crate::aver_generated::domain::ast::MatchArm>,
    pat: &crate::aver_generated::domain::ast::Pattern,
) -> Result<(crate::aver_generated::domain::ast::Expr, aver_rt::AverInt), AverStr> {
    __mutual_tco_trampoline_12(
        __MutualTco12::ParseOneArmFlatBody(pos, acc.clone(), pat.clone()),
        &tokens,
        &subject,
    )
}

/// Parse an expression: comparison level, then optional ? postfix.
pub fn parseExpr(
    tokens: &aver_rt::AverList<crate::aver_generated::domain::token::Token>,
    pos: aver_rt::AverInt,
) -> Result<(crate::aver_generated::domain::ast::Expr, aver_rt::AverInt), AverStr> {
    crate::cancel_checkpoint();
    let r @ _ = crate::aver_generated::domain::parser::expr::parseCmpExpr(tokens, pos)?;
    {
        let (expr, pos2) = r;
        Ok(crate::aver_generated::domain::parser::expr::parsePostfixQuestion(tokens, pos2, &expr))
    }
}

/// Check for postfix ? (error propagation).
pub fn parsePostfixQuestion(
    tokens: &aver_rt::AverList<crate::aver_generated::domain::token::Token>,
    pos: aver_rt::AverInt,
    expr: &crate::aver_generated::domain::ast::Expr,
) -> (crate::aver_generated::domain::ast::Expr, aver_rt::AverInt) {
    crate::cancel_checkpoint();
    match crate::aver_generated::domain::parser_match::tokenAt(tokens, pos.clone()) {
        crate::aver_generated::domain::token::Token::TkQuestion => (
            crate::aver_generated::domain::ast::Expr::ExprPropagate(std::sync::Arc::new(
                expr.clone(),
            )),
            pos.add(&aver_rt::AverInt::from_i64(1)),
        ),
        _ => (expr.clone(), pos),
    }
}

/// Parse comparison: addExpr (('==' | '<' | '>') addExpr)?
pub fn parseCmpExpr(
    tokens: &aver_rt::AverList<crate::aver_generated::domain::token::Token>,
    pos: aver_rt::AverInt,
) -> Result<(crate::aver_generated::domain::ast::Expr, aver_rt::AverInt), AverStr> {
    crate::cancel_checkpoint();
    let r @ _ = crate::aver_generated::domain::parser::expr::parseAddExpr(tokens, pos)?;
    {
        let (left, pos2) = r;
        crate::aver_generated::domain::parser::expr::parseCmpExprTail(tokens, pos2, &left)
    }
}

/// Check for comparison operator.
pub fn parseCmpExprTail(
    tokens: &aver_rt::AverList<crate::aver_generated::domain::token::Token>,
    pos: aver_rt::AverInt,
    left: &crate::aver_generated::domain::ast::Expr,
) -> Result<(crate::aver_generated::domain::ast::Expr, aver_rt::AverInt), AverStr> {
    crate::cancel_checkpoint();
    let nextPos @ _ = pos.add(&aver_rt::AverInt::from_i64(1));
    match crate::aver_generated::domain::parser_match::tokenAt(tokens, pos.clone()) {
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
        _ => Ok((left.clone(), pos)),
    }
}

/// Parse right side of comparison.
pub fn parseCmpExprRight(
    tokens: &aver_rt::AverList<crate::aver_generated::domain::token::Token>,
    pos: aver_rt::AverInt,
    left: &crate::aver_generated::domain::ast::Expr,
    op: aver_rt::AverInt,
) -> Result<(crate::aver_generated::domain::ast::Expr, aver_rt::AverInt), AverStr> {
    crate::cancel_checkpoint();
    let r @ _ = crate::aver_generated::domain::parser::expr::parseAddExpr(tokens, pos)?;
    {
        let (right, pos2) = r;
        crate::aver_generated::domain::parser::expr::buildCmpExpr(left, &right, pos2, op)
    }
}

/// Build comparison AST node from op code.
#[inline(always)]
pub fn buildCmpExpr(
    left: &crate::aver_generated::domain::ast::Expr,
    right: &crate::aver_generated::domain::ast::Expr,
    pos: aver_rt::AverInt,
    op: aver_rt::AverInt,
) -> Result<(crate::aver_generated::domain::ast::Expr, aver_rt::AverInt), AverStr> {
    crate::cancel_checkpoint();
    {
        let __dispatch_subject = op;
        if __dispatch_subject == aver_rt::AverInt::from_i64(0) {
            Ok((
                crate::aver_generated::domain::ast::Expr::ExprEq(
                    std::sync::Arc::new(left.clone()),
                    std::sync::Arc::new(right.clone()),
                ),
                pos,
            ))
        } else {
            if __dispatch_subject == aver_rt::AverInt::from_i64(1) {
                Ok((
                    crate::aver_generated::domain::ast::Expr::ExprNeq(
                        std::sync::Arc::new(left.clone()),
                        std::sync::Arc::new(right.clone()),
                    ),
                    pos,
                ))
            } else {
                if __dispatch_subject == aver_rt::AverInt::from_i64(2) {
                    Ok((
                        crate::aver_generated::domain::ast::Expr::ExprLt(
                            std::sync::Arc::new(left.clone()),
                            std::sync::Arc::new(right.clone()),
                        ),
                        pos,
                    ))
                } else {
                    if __dispatch_subject == aver_rt::AverInt::from_i64(3) {
                        Ok((
                            crate::aver_generated::domain::ast::Expr::ExprGt(
                                std::sync::Arc::new(left.clone()),
                                std::sync::Arc::new(right.clone()),
                            ),
                            pos,
                        ))
                    } else {
                        if __dispatch_subject == aver_rt::AverInt::from_i64(4) {
                            Ok((
                                crate::aver_generated::domain::ast::Expr::ExprLte(
                                    std::sync::Arc::new(left.clone()),
                                    std::sync::Arc::new(right.clone()),
                                ),
                                pos,
                            ))
                        } else {
                            if __dispatch_subject == aver_rt::AverInt::from_i64(5) {
                                Ok((
                                    crate::aver_generated::domain::ast::Expr::ExprGte(
                                        std::sync::Arc::new(left.clone()),
                                        std::sync::Arc::new(right.clone()),
                                    ),
                                    pos,
                                ))
                            } else {
                                Err(AverStr::from("unknown cmp operator"))
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
    tokens: &aver_rt::AverList<crate::aver_generated::domain::token::Token>,
    pos: aver_rt::AverInt,
) -> Result<(crate::aver_generated::domain::ast::Expr, aver_rt::AverInt), AverStr> {
    crate::cancel_checkpoint();
    let r @ _ = crate::aver_generated::domain::parser::expr::parseMulExpr(tokens, pos)?;
    {
        let (expr, pos2) = r;
        crate::aver_generated::domain::parser::expr::parseAddExprTail(tokens, pos2, &expr)
    }
}

/// Parse multiplicative: atom ('*' atom)*
pub fn parseMulExpr(
    tokens: &aver_rt::AverList<crate::aver_generated::domain::token::Token>,
    pos: aver_rt::AverInt,
) -> Result<(crate::aver_generated::domain::ast::Expr, aver_rt::AverInt), AverStr> {
    crate::cancel_checkpoint();
    let r @ _ = crate::aver_generated::domain::parser::expr::parseAtom(tokens, pos)?;
    {
        let (expr, pos2) = r;
        crate::aver_generated::domain::parser::expr::parseMulExprTail(tokens, pos2, &expr)
    }
}

/// Parse atomic expression: literal, variable, call, match, or parenthesized.
pub fn parseAtom(
    tokens: &aver_rt::AverList<crate::aver_generated::domain::token::Token>,
    pos: aver_rt::AverInt,
) -> Result<(crate::aver_generated::domain::ast::Expr, aver_rt::AverInt), AverStr> {
    crate::cancel_checkpoint();
    let nextPos @ _ = pos.add(&aver_rt::AverInt::from_i64(1));
    let t @ _ = crate::aver_generated::domain::parser_match::tokenAt(tokens, pos);
    match t.clone() {
        crate::aver_generated::domain::token::Token::TkMinus => {
            crate::aver_generated::domain::parser::expr::parseNegAtom(tokens, nextPos)
        }
        crate::aver_generated::domain::token::Token::TkInt(n) => Ok((
            crate::aver_generated::domain::ast::Expr::ExprInt(n),
            nextPos,
        )),
        crate::aver_generated::domain::token::Token::TkFloat(f) => Ok((
            crate::aver_generated::domain::ast::Expr::ExprFloat(f),
            nextPos,
        )),
        crate::aver_generated::domain::token::Token::TkStr(s) => {
            crate::aver_generated::domain::parser::expr::parseStringOrInterp(tokens, nextPos, s)
        }
        crate::aver_generated::domain::token::Token::TkTrue => Ok((
            crate::aver_generated::domain::ast::Expr::ExprBool(true),
            nextPos,
        )),
        crate::aver_generated::domain::token::Token::TkFalse => Ok((
            crate::aver_generated::domain::ast::Expr::ExprBool(false),
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
            crate::aver_generated::domain::parser::expr::parseIdentOrCall(tokens, nextPos, name)
        }
        _ => Err(aver_rt::AverStr::from({
            let mut __b = {
                let mut __b = aver_rt::Buffer::with_capacity(
                    (aver_rt::AverInt::from_i64(41)).to_usize().unwrap_or(0),
                );
                __b.push_str(&AverStr::from("Expected expression, got "));
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
    tokens: &aver_rt::AverList<crate::aver_generated::domain::token::Token>,
    pos: aver_rt::AverInt,
) -> Result<(crate::aver_generated::domain::ast::Expr, aver_rt::AverInt), AverStr> {
    crate::cancel_checkpoint();
    let pos2 @ _ = crate::aver_generated::domain::parser::expr::skipNl(tokens.clone(), pos);
    match crate::aver_generated::domain::parser_match::tokenAt(tokens, pos2.clone()) {
        crate::aver_generated::domain::token::Token::TkRBrace => Ok((
            crate::aver_generated::domain::ast::Expr::ExprCall(
                AverStr::from("Map.fromList"),
                aver_rt::AverList::from_vec(vec![
                    crate::aver_generated::domain::ast::Expr::ExprList(aver_rt::AverList::empty()),
                ]),
            ),
            pos2.add(&aver_rt::AverInt::from_i64(1)),
        )),
        _ => crate::aver_generated::domain::parser::expr::parseMapEntries(
            tokens,
            pos2,
            &aver_rt::AverList::empty(),
        ),
    }
}

/// Parse unary minus into the first-class Expr.ExprNeg node.
pub fn parseNegAtom(
    tokens: &aver_rt::AverList<crate::aver_generated::domain::token::Token>,
    pos: aver_rt::AverInt,
) -> Result<(crate::aver_generated::domain::ast::Expr, aver_rt::AverInt), AverStr> {
    crate::cancel_checkpoint();
    let r @ _ = crate::aver_generated::domain::parser::expr::parseAtom(tokens, pos)?;
    {
        let (expr, pos2) = r;
        Ok((
            crate::aver_generated::domain::ast::Expr::ExprNeg(std::sync::Arc::new(expr)),
            pos2,
        ))
    }
}

/// After TkStr: check for interpolation (TkInterpStart) or plain string.
pub fn parseStringOrInterp(
    tokens: &aver_rt::AverList<crate::aver_generated::domain::token::Token>,
    pos: aver_rt::AverInt,
    prefix: AverStr,
) -> Result<(crate::aver_generated::domain::ast::Expr, aver_rt::AverInt), AverStr> {
    crate::cancel_checkpoint();
    match crate::aver_generated::domain::parser_match::tokenAt(tokens, pos.clone()) {
        crate::aver_generated::domain::token::Token::TkInterpStart => {
            crate::aver_generated::domain::parser::expr::parseInterpParts(
                tokens,
                pos.add(&aver_rt::AverInt::from_i64(1)),
                &aver_rt::AverList::from_vec(vec![
                    crate::aver_generated::domain::ast::Expr::ExprStr(prefix),
                ]),
            )
        }
        _ => Ok((
            crate::aver_generated::domain::ast::Expr::ExprStr(prefix),
            pos,
        )),
    }
}

/// Parse list literal: [expr, expr, ...] or [].
pub fn parseListExpr(
    tokens: &aver_rt::AverList<crate::aver_generated::domain::token::Token>,
    pos0: aver_rt::AverInt,
) -> Result<(crate::aver_generated::domain::ast::Expr, aver_rt::AverInt), AverStr> {
    crate::cancel_checkpoint();
    let pos @ _ = crate::aver_generated::domain::parser::expr::skipNl(tokens.clone(), pos0);
    match crate::aver_generated::domain::parser_match::tokenAt(tokens, pos.clone()) {
        crate::aver_generated::domain::token::Token::TkRBracket => Ok((
            crate::aver_generated::domain::ast::Expr::ExprList(aver_rt::AverList::empty()),
            pos.add(&aver_rt::AverInt::from_i64(1)),
        )),
        _ => crate::aver_generated::domain::parser::expr::parseListItems(
            tokens,
            pos,
            &aver_rt::AverList::empty(),
        ),
    }
}

/// Parse parenthesized expression or tuple.
pub fn parseParenExpr(
    tokens: &aver_rt::AverList<crate::aver_generated::domain::token::Token>,
    pos: aver_rt::AverInt,
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
                    &aver_rt::AverList::from_vec(vec![expr]),
                )
            }
            crate::aver_generated::domain::token::Token::TkRParen => {
                Ok((expr, pos2.add(&aver_rt::AverInt::from_i64(1))))
            }
            _ => Err(AverStr::from("Expected ')' or ',' after expression")),
        }
    }
}

/// After tuple closing ')': check for ?! or ! postfix to form independent product.
pub fn finishTupleOrProduct(
    tokens: &aver_rt::AverList<crate::aver_generated::domain::token::Token>,
    pos: aver_rt::AverInt,
    items: &aver_rt::AverList<crate::aver_generated::domain::ast::Expr>,
) -> Result<(crate::aver_generated::domain::ast::Expr, aver_rt::AverInt), AverStr> {
    crate::cancel_checkpoint();
    match crate::aver_generated::domain::parser_match::tokenAt(tokens, pos.clone()) {
        crate::aver_generated::domain::token::Token::TkQuestion => {
            match crate::aver_generated::domain::parser_match::tokenAt(
                tokens,
                pos.add(&aver_rt::AverInt::from_i64(1)),
            ) {
                crate::aver_generated::domain::token::Token::TkBang => Ok((
                    crate::aver_generated::domain::ast::Expr::ExprIndependentProduct(
                        items.clone(),
                        true,
                    ),
                    pos.add(&aver_rt::AverInt::from_i64(2)),
                )),
                _ => Ok((
                    crate::aver_generated::domain::ast::Expr::ExprTuple(items.clone()),
                    pos,
                )),
            }
        }
        crate::aver_generated::domain::token::Token::TkBang => {
            match crate::aver_generated::domain::parser_match::tokenAt(
                tokens,
                pos.add(&aver_rt::AverInt::from_i64(1)),
            ) {
                crate::aver_generated::domain::token::Token::TkLBracket => Ok((
                    crate::aver_generated::domain::ast::Expr::ExprTuple(items.clone()),
                    pos,
                )),
                _ => Ok((
                    crate::aver_generated::domain::ast::Expr::ExprIndependentProduct(
                        items.clone(),
                        false,
                    ),
                    pos.add(&aver_rt::AverInt::from_i64(1)),
                )),
            }
        }
        _ => Ok((
            crate::aver_generated::domain::ast::Expr::ExprTuple(items.clone()),
            pos,
        )),
    }
}

/// After reading an identifier: call, record constructor, field access, or variable?
pub fn parseIdentOrCall(
    tokens: &aver_rt::AverList<crate::aver_generated::domain::token::Token>,
    pos: aver_rt::AverInt,
    name: AverStr,
) -> Result<(crate::aver_generated::domain::ast::Expr, aver_rt::AverInt), AverStr> {
    crate::cancel_checkpoint();
    let nextPos @ _ = pos.add(&aver_rt::AverInt::from_i64(1));
    match crate::aver_generated::domain::parser_match::tokenAt(tokens, pos.clone()) {
        crate::aver_generated::domain::token::Token::TkLParen => {
            crate::aver_generated::domain::parser::expr::chainFieldAccess(tokens, nextPos, name)
        }
        crate::aver_generated::domain::token::Token::TkDot => {
            crate::aver_generated::domain::parser::expr::parseFieldAccess(
                tokens,
                nextPos,
                &crate::aver_generated::domain::ast::Expr::ExprVar(name),
            )
        }
        _ => Ok((crate::aver_generated::domain::ast::Expr::ExprVar(name), pos)),
    }
}

/// Parse call/record then chain .field access if present.
pub fn chainFieldAccess(
    tokens: &aver_rt::AverList<crate::aver_generated::domain::token::Token>,
    pos: aver_rt::AverInt,
    name: AverStr,
) -> Result<(crate::aver_generated::domain::ast::Expr, aver_rt::AverInt), AverStr> {
    crate::cancel_checkpoint();
    let r @ _ = crate::aver_generated::domain::parser::expr::parseCallOrRecord(tokens, pos, name)?;
    {
        let (expr, pos2) = r;
        crate::aver_generated::domain::parser::expr::parseFieldAccessTail(tokens, pos2, &expr)
    }
}

/// Skip newlines and indents inside parenthesized expressions.
pub fn skipNl(
    tokens: aver_rt::AverList<crate::aver_generated::domain::token::Token>,
    mut pos: aver_rt::AverInt,
) -> aver_rt::AverInt {
    let tokens = std::sync::Arc::new(tokens);
    loop {
        crate::cancel_checkpoint();
        let nextPos @ _ = pos.add(&aver_rt::AverInt::from_i64(1));
        match crate::aver_generated::domain::parser_match::tokenAt(&*tokens, pos.clone()) {
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
    tokens: &aver_rt::AverList<crate::aver_generated::domain::token::Token>,
    pos: aver_rt::AverInt,
    name: AverStr,
) -> Result<(crate::aver_generated::domain::ast::Expr, aver_rt::AverInt), AverStr> {
    crate::cancel_checkpoint();
    let pos2 @ _ = crate::aver_generated::domain::parser::expr::skipNl(tokens.clone(), pos);
    match crate::aver_generated::domain::parser_match::tokenAt(tokens, pos2.clone()) {
        crate::aver_generated::domain::token::Token::TkRParen => Ok((
            crate::aver_generated::domain::ast::Expr::ExprCall(name, aver_rt::AverList::empty()),
            pos2.add(&aver_rt::AverInt::from_i64(1)),
        )),
        crate::aver_generated::domain::token::Token::TkIdent(first) => {
            crate::aver_generated::domain::parser::expr::parseCallOrRecordLookahead(
                tokens, pos2, name, first,
            )
        }
        _ => crate::aver_generated::domain::parser::expr::parseCallArgsList(
            tokens,
            pos2,
            name,
            &aver_rt::AverList::empty(),
        ),
    }
}

/// Lookahead after first ident in parens: '=' means record, otherwise call.
pub fn parseCallOrRecordLookahead(
    tokens: &aver_rt::AverList<crate::aver_generated::domain::token::Token>,
    pos: aver_rt::AverInt,
    name: AverStr,
    first: AverStr,
) -> Result<(crate::aver_generated::domain::ast::Expr, aver_rt::AverInt), AverStr> {
    crate::cancel_checkpoint();
    match crate::aver_generated::domain::parser_match::tokenAt(
        tokens,
        pos.add(&aver_rt::AverInt::from_i64(1)),
    ) {
        crate::aver_generated::domain::token::Token::TkEq => {
            crate::aver_generated::domain::parser::expr::parseRecordFields(
                tokens,
                pos,
                name,
                &aver_rt::AverList::empty(),
            )
        }
        _ => crate::aver_generated::domain::parser::expr::parseCallArgsList(
            tokens,
            pos,
            name,
            &aver_rt::AverList::empty(),
        ),
    }
}

/// Parse: match expr NEWLINE INDENT arms DEDENT. Called after TkMatch is consumed.
pub fn parseMatchExpr(
    tokens: &aver_rt::AverList<crate::aver_generated::domain::token::Token>,
    pos: aver_rt::AverInt,
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
    tokens: &aver_rt::AverList<crate::aver_generated::domain::token::Token>,
    pos: aver_rt::AverInt,
    subject: &crate::aver_generated::domain::ast::Expr,
) -> Result<(crate::aver_generated::domain::ast::Expr, aver_rt::AverInt), AverStr> {
    crate::cancel_checkpoint();
    let pos2 @ _ = crate::aver_generated::domain::parser_match::skipNewlines(tokens.clone(), pos);
    match crate::aver_generated::domain::parser_match::tokenAt(tokens, pos2.clone()) {
        crate::aver_generated::domain::token::Token::TkIndent => {
            crate::aver_generated::domain::parser::expr::parseMatchArms(
                tokens,
                pos2.add(&aver_rt::AverInt::from_i64(1)),
                subject,
                &aver_rt::AverList::empty(),
            )
        }
        _ => crate::aver_generated::domain::parser::expr::parseMatchArmsFlat(
            tokens,
            pos2,
            subject,
            &aver_rt::AverList::empty(),
        ),
    }
}

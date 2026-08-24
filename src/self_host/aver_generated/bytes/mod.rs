#[allow(unused_imports)]
use crate::*;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Bytes {
    pub values: aver_rt::AverPackedU8,
}

impl PartialOrd for Bytes {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Bytes {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        std::cmp::Ordering::Equal.then_with(|| self.values.cmp(&other.values))
    }
}

impl aver_rt::AverDisplay for Bytes {
    fn aver_display(&self) -> String {
        format!(
            "Bytes({})",
            format!("values: {}", self.values.aver_display_inner())
        )
    }
    fn aver_display_inner(&self) -> String {
        self.aver_display()
    }
}

impl aver_rt::provider::ProviderCodec for Bytes {
    fn into_provider_value(
        self,
        _registry: &aver_rt::provider::NativeProviderRegistry,
        _capability: &str,
    ) -> Result<aver_rt::provider::ProviderValue, String> {
        Ok(aver_rt::provider::ProviderValue::Bytes(
            self.values.into_vec(),
        ))
    }

    fn from_provider_value(
        value: aver_rt::provider::ProviderValue,
        _registry: &aver_rt::provider::NativeProviderRegistry,
        _capability: &str,
        _minted_resource: Option<&str>,
    ) -> Result<Self, String> {
        match value {
            aver_rt::provider::ProviderValue::Bytes(bytes) => Ok(Self {
                values: aver_rt::AverPackedU8::from_vec(bytes),
            }),
            other => Err(format!("expected Bytes, got {}", other.shape())),
        }
    }
}

impl aver_replay::ReplayValue for Bytes {
    fn to_replay_json(&self) -> serde_json::Value {
        let mut fields = serde_json::Map::new();
        fields.insert(
            "values".to_string(),
            ReplayValue::to_replay_json(&self.values.to_int_list()),
        );
        let mut payload = serde_json::Map::new();
        payload.insert(
            "type".to_string(),
            serde_json::Value::String("Bytes".to_string()),
        );
        payload.insert("fields".to_string(), serde_json::Value::Object(fields));
        aver_replay::wrap_marker("$record", serde_json::Value::Object(payload))
    }

    fn from_replay_json(value: &serde_json::Value) -> Result<Self, String> {
        let payload = aver_replay::expect_marker(value, "$record")?;
        let obj = aver_replay::expect_object(payload, "$record")?;
        let type_name = aver_replay::expect_string(
            obj.get("type")
                .ok_or_else(|| "$record missing field 'type'".to_string())?,
            "$record.type",
        )?;
        if type_name != "Bytes" {
            return Err(format!(
                "$record type mismatch: expected Bytes, got {}",
                type_name
            ));
        }
        let fields = aver_replay::expect_object(
            obj.get("fields")
                .ok_or_else(|| "$record missing field 'fields'".to_string())?,
            "$record.fields",
        )?;
        Ok(Self {
            values: (<aver_rt::AverIntList as ReplayValue>::from_replay_json(
                fields
                    .get("values")
                    .ok_or_else(|| "$record Bytes missing field 'values'".to_string())?,
            )?)
            .into_packed()?,
        })
    }
}

/// Return true when every integer in the list is an octet.
#[inline(always)]
pub fn allInRange(mut xs: aver_rt::AverIntList) -> bool {
    loop {
        crate::cancel_checkpoint();
        aver_list_match!(xs, [] => { return true; }, [head, tail] => { if ((head >= aver_rt::AverInt::from_i64(0)) && (head <= aver_rt::AverInt::from_i64(255))) { {
            let __tco0 = tail;
            xs = __tco0;
            continue;
        } } else { return false; } })
    }
}

/// Return the first non-octet value; -1 when every value is an octet.
#[inline(always)]
pub fn firstOutOfRange(mut xs: aver_rt::AverIntList) -> aver_rt::AverInt {
    loop {
        crate::cancel_checkpoint();
        aver_list_match!(xs, [] => { return aver_rt::AverInt::from_i64(-1); }, [head, tail] => { if ((head >= aver_rt::AverInt::from_i64(0)) && (head <= aver_rt::AverInt::from_i64(255))) { {
            let __tco0 = tail;
            xs = __tco0;
            continue;
        } } else { return head; } })
    }
}

/// Return the index of the first non-octet value; the length when every value is an octet.
#[inline(always)]
pub fn firstOutOfRangeIndex(xs: &aver_rt::AverIntList) -> aver_rt::AverInt {
    crate::cancel_checkpoint();
    aver_list_match!(xs.clone(), [] => aver_rt::AverInt::from_i64(0), [head, tail] => if ((head >= aver_rt::AverInt::from_i64(0)) && (head <= aver_rt::AverInt::from_i64(255))) { aver_rt::AverInt::from_i64(1).add(&crate::aver_generated::bytes::firstOutOfRangeIndex(&tail)) } else { aver_rt::AverInt::from_i64(0) })
}

/// Validate raw integers and construct a byte sequence.
#[inline(always)]
pub fn fromList(xs: &aver_rt::AverIntList) -> Result<Bytes, AverStr> {
    crate::cancel_checkpoint();
    if crate::aver_generated::bytes::allInRange(xs.clone()) {
        Ok(crate::aver_generated::bytes::Bytes {
            values: aver_rt::into_packed_u8(xs.clone())
                .expect("proof-packed U8 construction escaped its refinement gate"),
        })
    } else {
        Err(aver_rt::AverStr::from({
            let mut __b = {
                let mut __b = {
                    let mut __b = {
                        let mut __b = {
                            let mut __b = aver_rt::Buffer::with_capacity(
                                (aver_rt::AverInt::from_i64(66)).to_usize().unwrap_or(0),
                            );
                            __b.push_str(&AverStr::from("byte "));
                            __b
                        };
                        __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                            &(crate::aver_generated::bytes::firstOutOfRange(xs.clone())),
                        )));
                        __b
                    };
                    __b.push_str(&AverStr::from(" at index "));
                    __b
                };
                __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                    &(crate::aver_generated::bytes::firstOutOfRangeIndex(xs)),
                )));
                __b
            };
            __b.push_str(&AverStr::from(" is outside 0..=255"));
            __b
        }))
    }
}

/// Expose the validated octets for ordinary List operations.
#[inline(always)]
pub fn octets(bytes: &Bytes) -> aver_rt::AverIntList {
    crate::cancel_checkpoint();
    (bytes.values).to_int_list().clone()
}

/// Parse hexadecimal pairs into octets from left to right.
#[inline(always)]
pub fn parseHexChars(
    mut chars: aver_rt::AverList<AverStr>,
    mut acc: aver_rt::AverIntList,
) -> Result<aver_rt::AverIntList, AverStr> {
    loop {
        crate::cancel_checkpoint();
        aver_list_match!(chars, [] => { return Ok(acc.reverse()); }, [highChar, afterHigh] => { aver_list_match!(afterHigh, [] => { return Err(AverStr::from("Bytes.fromHex: expected an even number of hex characters")); }, [lowChar, rest] => { match crate::aver_generated::bytes::hexDigitValue(highChar.clone()) { None => { return Err(aver_rt::AverStr::from({ let mut __b = { let mut __b = { let mut __b = aver_rt::Buffer::with_capacity((aver_rt::AverInt::from_i64(63)).to_usize().unwrap_or(0)); __b.push_str(&AverStr::from("Bytes.fromHex: invalid hexadecimal character '")); __b }; __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(highChar)))); __b }; __b.push_str(&AverStr::from("'")); __b })); }, Some(high @ _) => { match crate::aver_generated::bytes::hexDigitValue(lowChar.clone()) { None => { return Err(aver_rt::AverStr::from({ let mut __b = { let mut __b = { let mut __b = aver_rt::Buffer::with_capacity((aver_rt::AverInt::from_i64(63)).to_usize().unwrap_or(0)); __b.push_str(&AverStr::from("Bytes.fromHex: invalid hexadecimal character '")); __b }; __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(lowChar)))); __b }; __b.push_str(&AverStr::from("'")); __b })); }, Some(low @ _) => { {
            let __tco0 = rest;
            let __tco1 = aver_rt::AverIntList::prepend(high.mul(&aver_rt::AverInt::from_i64(16)).add(&low), &acc);
            chars = __tco0;
            acc = __tco1;
            continue;
        } } } } } }) })
    }
}

/// Decode one case-insensitive hexadecimal digit.
#[inline(always)]
pub fn hexDigitValue(digit: AverStr) -> Option<aver_rt::AverInt> {
    crate::cancel_checkpoint();
    {
        let __dispatch_subject = aver_rt::AverInt::from_i64(aver_rt::str_code1_lower(&digit));
        if __dispatch_subject == aver_rt::AverInt::from_i64(48) {
            Some(aver_rt::AverInt::from_i64(0))
        } else {
            if __dispatch_subject == aver_rt::AverInt::from_i64(49) {
                Some(aver_rt::AverInt::from_i64(1))
            } else {
                if __dispatch_subject == aver_rt::AverInt::from_i64(50) {
                    Some(aver_rt::AverInt::from_i64(2))
                } else {
                    if __dispatch_subject == aver_rt::AverInt::from_i64(51) {
                        Some(aver_rt::AverInt::from_i64(3))
                    } else {
                        if __dispatch_subject == aver_rt::AverInt::from_i64(52) {
                            Some(aver_rt::AverInt::from_i64(4))
                        } else {
                            if __dispatch_subject == aver_rt::AverInt::from_i64(53) {
                                Some(aver_rt::AverInt::from_i64(5))
                            } else {
                                if __dispatch_subject == aver_rt::AverInt::from_i64(54) {
                                    Some(aver_rt::AverInt::from_i64(6))
                                } else {
                                    if __dispatch_subject == aver_rt::AverInt::from_i64(55) {
                                        Some(aver_rt::AverInt::from_i64(7))
                                    } else {
                                        if __dispatch_subject == aver_rt::AverInt::from_i64(56) {
                                            Some(aver_rt::AverInt::from_i64(8))
                                        } else {
                                            if __dispatch_subject == aver_rt::AverInt::from_i64(57)
                                            {
                                                Some(aver_rt::AverInt::from_i64(9))
                                            } else {
                                                if __dispatch_subject
                                                    == aver_rt::AverInt::from_i64(97)
                                                {
                                                    Some(aver_rt::AverInt::from_i64(10))
                                                } else {
                                                    if __dispatch_subject
                                                        == aver_rt::AverInt::from_i64(98)
                                                    {
                                                        Some(aver_rt::AverInt::from_i64(11))
                                                    } else {
                                                        if __dispatch_subject
                                                            == aver_rt::AverInt::from_i64(99)
                                                        {
                                                            Some(aver_rt::AverInt::from_i64(12))
                                                        } else {
                                                            if __dispatch_subject
                                                                == aver_rt::AverInt::from_i64(100)
                                                            {
                                                                Some(aver_rt::AverInt::from_i64(13))
                                                            } else {
                                                                if __dispatch_subject
                                                                    == aver_rt::AverInt::from_i64(
                                                                        101,
                                                                    )
                                                                {
                                                                    Some(
                                                                        aver_rt::AverInt::from_i64(
                                                                            14,
                                                                        ),
                                                                    )
                                                                } else {
                                                                    if __dispatch_subject == aver_rt::AverInt::from_i64(102) { Some(aver_rt::AverInt::from_i64(15)) } else { None }
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
                        }
                    }
                }
            }
        }
    }
}

/// Decode an even-length hexadecimal string into validated bytes.
#[inline(always)]
pub fn fromHex(text: AverStr) -> Result<Bytes, AverStr> {
    crate::cancel_checkpoint();
    crate::aver_generated::bytes::parseHexChars__cursor__collected(
        text,
        aver_rt::AverInt::from_i64(0),
        aver_rt::byte_builder_new((aver_rt::AverInt::from_i64(0)).to_usize().unwrap_or(0)),
    )
}

/// Encode one integer in 0..=15 as lowercase hexadecimal.
#[inline(always)]
pub fn hexDigit(value: aver_rt::AverInt) -> AverStr {
    crate::cancel_checkpoint();
    {
        let __dispatch_subject = value;
        if __dispatch_subject == aver_rt::AverInt::from_i64(0) {
            AverStr::from("0")
        } else {
            if __dispatch_subject == aver_rt::AverInt::from_i64(1) {
                AverStr::from("1")
            } else {
                if __dispatch_subject == aver_rt::AverInt::from_i64(2) {
                    AverStr::from("2")
                } else {
                    if __dispatch_subject == aver_rt::AverInt::from_i64(3) {
                        AverStr::from("3")
                    } else {
                        if __dispatch_subject == aver_rt::AverInt::from_i64(4) {
                            AverStr::from("4")
                        } else {
                            if __dispatch_subject == aver_rt::AverInt::from_i64(5) {
                                AverStr::from("5")
                            } else {
                                if __dispatch_subject == aver_rt::AverInt::from_i64(6) {
                                    AverStr::from("6")
                                } else {
                                    if __dispatch_subject == aver_rt::AverInt::from_i64(7) {
                                        AverStr::from("7")
                                    } else {
                                        if __dispatch_subject == aver_rt::AverInt::from_i64(8) {
                                            AverStr::from("8")
                                        } else {
                                            if __dispatch_subject == aver_rt::AverInt::from_i64(9) {
                                                AverStr::from("9")
                                            } else {
                                                if __dispatch_subject
                                                    == aver_rt::AverInt::from_i64(10)
                                                {
                                                    AverStr::from("a")
                                                } else {
                                                    if __dispatch_subject
                                                        == aver_rt::AverInt::from_i64(11)
                                                    {
                                                        AverStr::from("b")
                                                    } else {
                                                        if __dispatch_subject
                                                            == aver_rt::AverInt::from_i64(12)
                                                        {
                                                            AverStr::from("c")
                                                        } else {
                                                            if __dispatch_subject
                                                                == aver_rt::AverInt::from_i64(13)
                                                            {
                                                                AverStr::from("d")
                                                            } else {
                                                                if __dispatch_subject
                                                                    == aver_rt::AverInt::from_i64(
                                                                        14,
                                                                    )
                                                                {
                                                                    AverStr::from("e")
                                                                } else {
                                                                    if __dispatch_subject == aver_rt::AverInt::from_i64(15) { AverStr::from("f") } else { AverStr::from("") }
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
                        }
                    }
                }
            }
        }
    }
}

/// Compute a validated octet's high nibble by total division on a literal divisor.
#[inline(always)]
pub fn highNibble(value: aver_rt::AverInt) -> aver_rt::AverInt {
    crate::cancel_checkpoint();
    (value)
        .div_euclid(&(aver_rt::AverInt::from_i64(16)))
        .unwrap()
}

/// Compute a validated octet's low nibble by total modulo on a literal divisor.
#[inline(always)]
pub fn lowNibble(value: aver_rt::AverInt) -> aver_rt::AverInt {
    crate::cancel_checkpoint();
    (value)
        .rem_euclid(&(aver_rt::AverInt::from_i64(16)))
        .unwrap()
}

/// Encode one validated octet as two lowercase hexadecimal characters.
pub fn byteToHex(value: aver_rt::AverInt) -> AverStr {
    crate::cancel_checkpoint();
    (crate::aver_generated::bytes::hexDigit(crate::aver_generated::bytes::highNibble(
        value.clone(),
    )) + &crate::aver_generated::bytes::hexDigit(crate::aver_generated::bytes::lowNibble(value)))
}

/// Encode validated octets into lowercase two-character pieces.
#[inline(always)]
pub fn hexParts(
    mut values: aver_rt::AverIntList,
    mut acc: aver_rt::AverList<AverStr>,
) -> aver_rt::AverList<AverStr> {
    loop {
        crate::cancel_checkpoint();
        aver_list_match!(values, [] => { return acc.reverse(); }, [head, tail] => { {
            let __tco0 = tail;
            let __tco1 = aver_rt::AverList::prepend(crate::aver_generated::bytes::byteToHex(head), &acc);
            values = __tco0;
            acc = __tco1;
            continue;
        } })
    }
}

/// Encode bytes as lowercase hexadecimal; total because Bytes contains only octets.
#[inline(always)]
pub fn toHex(bytes: &Bytes) -> AverStr {
    crate::cancel_checkpoint();
    aver_rt::AverStr::from(crate::aver_generated::bytes::hexParts__buffered(
        crate::aver_generated::bytes::octets(bytes),
        aver_rt::Buffer::with_capacity((aver_rt::AverInt::from_i64(8192)).to_usize().unwrap_or(0)),
        AverStr::from(""),
    ))
}

/// Synthesized buffered variant of `hexParts` for deforestation lowering. Call sites that match `String.join(hexParts(...), sep)` are rewritten to alloc a buffer + call this variant + finalize, skipping the intermediate List.
#[inline(always)]
pub fn hexParts__buffered(
    mut values: aver_rt::AverIntList,
    mut __buf: Buffer,
    mut __sep: AverStr,
) -> Buffer {
    loop {
        crate::cancel_checkpoint();
        aver_list_match!(values, [] => { return __buf; }, [head, tail] => { {
            let __tco0 = tail;
            let __tco1 = { let mut __b = { let mut __b = __buf; if !__b.is_empty() { __b.push_str(&__sep); } __b }; __b.push_str(&crate::aver_generated::bytes::byteToHex(head)); __b };
            values = __tco0;
            __buf = __tco1;
            continue;
        } })
    }
}

/// Synthesized cursor variant of `parseHexChars`. Call sites that hand it `String.chars(s)` walk `s` directly — a byte offset stepped one codepoint at a time — instead of materialising the list of one-character strings.
#[inline(always)]
pub fn parseHexChars__cursor(
    mut __cur_s: AverStr,
    mut __cur_i: aver_rt::AverInt,
    mut acc: aver_rt::AverIntList,
) -> Result<aver_rt::AverIntList, AverStr> {
    loop {
        crate::cancel_checkpoint();
        if aver_rt::str_cursor_end(&__cur_s, (__cur_i).to_usize().unwrap_or(usize::MAX)) {
            return Ok(acc.reverse());
        } else {
            {
                let __cur_c2 =
                    aver_rt::str_cursor_code(&__cur_s, (__cur_i).to_usize().unwrap_or(usize::MAX));
                {
                    let __cur_i1 = aver_rt::AverInt::from_i64(aver_rt::str_cursor_next(
                        &__cur_s,
                        (__cur_i).to_usize().unwrap_or(usize::MAX),
                    ) as i64);
                    if aver_rt::str_cursor_end(
                        &__cur_s,
                        (__cur_i1).to_usize().unwrap_or(usize::MAX),
                    ) {
                        return Err(AverStr::from(
                            "Bytes.fromHex: expected an even number of hex characters",
                        ));
                    } else {
                        {
                            let __cur_c1 = aver_rt::str_cursor_code(
                                &__cur_s,
                                (__cur_i1).to_usize().unwrap_or(usize::MAX),
                            );
                            {
                                let __cur_i2 = aver_rt::AverInt::from_i64(
                                    aver_rt::str_cursor_next(
                                        &__cur_s,
                                        (__cur_i1).to_usize().unwrap_or(usize::MAX),
                                    ) as i64,
                                );
                                match crate::aver_generated::bytes::hexDigitValue__code(__cur_c2) {
                                    None => {
                                        return Err(aver_rt::AverStr::from({
                                            let mut __b = {
                                                let mut __b = {
                                                    let mut __b = aver_rt::Buffer::with_capacity(
                                                        (aver_rt::AverInt::from_i64(63))
                                                            .to_usize()
                                                            .unwrap_or(0),
                                                    );
                                                    __b.push_str(&AverStr::from("Bytes.fromHex: invalid hexadecimal character '"));
                                                    __b
                                                };
                                                __b.push_str(&aver_rt::AverStr::from(
                                                    aver_rt::aver_display(
                                                        &(aver_rt::AverStr::from(
                                                            aver_rt::str_cursor_head(
                                                                &__cur_s,
                                                                (__cur_i)
                                                                    .to_usize()
                                                                    .unwrap_or(usize::MAX),
                                                            ),
                                                        )),
                                                    ),
                                                ));
                                                __b
                                            };
                                            __b.push_str(&AverStr::from("'"));
                                            __b
                                        }));
                                    }
                                    Some(high @ _) => {
                                        match crate::aver_generated::bytes::hexDigitValue__code(
                                            __cur_c1,
                                        ) {
                                            None => {
                                                return Err(aver_rt::AverStr::from({
                                                    let mut __b = {
                                                        let mut __b = {
                                                            let mut __b =
                                                                aver_rt::Buffer::with_capacity(
                                                                    (aver_rt::AverInt::from_i64(
                                                                        63,
                                                                    ))
                                                                    .to_usize()
                                                                    .unwrap_or(0),
                                                                );
                                                            __b.push_str(&AverStr::from("Bytes.fromHex: invalid hexadecimal character '"));
                                                            __b
                                                        };
                                                        __b.push_str(&aver_rt::AverStr::from(
                                                            aver_rt::aver_display(
                                                                &(aver_rt::AverStr::from(
                                                                    aver_rt::str_cursor_head(
                                                                        &__cur_s,
                                                                        (__cur_i1)
                                                                            .to_usize()
                                                                            .unwrap_or(usize::MAX),
                                                                    ),
                                                                )),
                                                            ),
                                                        ));
                                                        __b
                                                    };
                                                    __b.push_str(&AverStr::from("'"));
                                                    __b
                                                }));
                                            }
                                            Some(low @ _) => {
                                                let __tco1 = __cur_i2;
                                                let __tco2 = aver_rt::AverIntList::prepend(
                                                    high.mul(&aver_rt::AverInt::from_i64(16))
                                                        .add(&low),
                                                    &acc,
                                                );
                                                __cur_i = __tco1;
                                                acc = __tco2;
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
    }
}

/// Synthesized codepoint variant of `hexDigitValue`. A cursor loop whose head only ever reaches this classifier hands over the character's code instead of materialising a one-character string.
#[inline(always)]
pub fn hexDigitValue__code(__str_code: i64) -> Option<aver_rt::AverInt> {
    crate::cancel_checkpoint();
    {
        let __dispatch_subject = aver_rt::str_fold_lower(__str_code);
        if __dispatch_subject == 48i64 {
            Some(aver_rt::AverInt::from_i64(0))
        } else {
            if __dispatch_subject == 49i64 {
                Some(aver_rt::AverInt::from_i64(1))
            } else {
                if __dispatch_subject == 50i64 {
                    Some(aver_rt::AverInt::from_i64(2))
                } else {
                    if __dispatch_subject == 51i64 {
                        Some(aver_rt::AverInt::from_i64(3))
                    } else {
                        if __dispatch_subject == 52i64 {
                            Some(aver_rt::AverInt::from_i64(4))
                        } else {
                            if __dispatch_subject == 53i64 {
                                Some(aver_rt::AverInt::from_i64(5))
                            } else {
                                if __dispatch_subject == 54i64 {
                                    Some(aver_rt::AverInt::from_i64(6))
                                } else {
                                    if __dispatch_subject == 55i64 {
                                        Some(aver_rt::AverInt::from_i64(7))
                                    } else {
                                        if __dispatch_subject == 56i64 {
                                            Some(aver_rt::AverInt::from_i64(8))
                                        } else {
                                            if __dispatch_subject == 57i64 {
                                                Some(aver_rt::AverInt::from_i64(9))
                                            } else {
                                                if __dispatch_subject == 97i64 {
                                                    Some(aver_rt::AverInt::from_i64(10))
                                                } else {
                                                    if __dispatch_subject == 98i64 {
                                                        Some(aver_rt::AverInt::from_i64(11))
                                                    } else {
                                                        if __dispatch_subject == 99i64 {
                                                            Some(aver_rt::AverInt::from_i64(12))
                                                        } else {
                                                            if __dispatch_subject == 100i64 {
                                                                Some(aver_rt::AverInt::from_i64(13))
                                                            } else {
                                                                if __dispatch_subject == 101i64 {
                                                                    Some(
                                                                        aver_rt::AverInt::from_i64(
                                                                            14,
                                                                        ),
                                                                    )
                                                                } else {
                                                                    if __dispatch_subject == 102i64
                                                                    {
                                                                        Some(aver_rt::AverInt::from_i64(15))
                                                                    } else {
                                                                        None
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
                            }
                        }
                    }
                }
            }
        }
    }
}

/// Synthesized collecting variant of `parseHexChars__cursor`. Appends to a builder where `parseHexChars__cursor` prepends to `acc` and reverses on the way out, which reaches the same list without the cons chain or the reversal. Call sites that start the accumulator at `[]` are moved here. Its only reader handed the collected list straight to the standard library's `fromList`, so the builder collects bytes: the range check rides every push, and the exits answer the `Result<Bytes, String>` the pair used to compute.
#[inline(always)]
pub fn parseHexChars__cursor__collected(
    mut __cur_s: AverStr,
    mut __cur_i: aver_rt::AverInt,
    mut acc: ByteBuilder,
) -> Result<Bytes, AverStr> {
    loop {
        crate::cancel_checkpoint();
        if aver_rt::str_cursor_end(&__cur_s, (__cur_i).to_usize().unwrap_or(usize::MAX)) {
            match aver_rt::byte_builder_finalize(acc) {
                Ok(__byt_vals @ _) => {
                    return Ok(crate::aver_generated::bytes::Bytes {
                        values: aver_rt::into_packed_u8(__byt_vals)
                            .expect("proof-packed U8 construction escaped its refinement gate"),
                    });
                }
                Err(__byt_msg @ _) => {
                    return Err(__byt_msg);
                }
            }
        } else {
            {
                let __cur_c2 =
                    aver_rt::str_cursor_code(&__cur_s, (__cur_i).to_usize().unwrap_or(usize::MAX));
                {
                    let __cur_i1 = aver_rt::AverInt::from_i64(aver_rt::str_cursor_next(
                        &__cur_s,
                        (__cur_i).to_usize().unwrap_or(usize::MAX),
                    ) as i64);
                    if aver_rt::str_cursor_end(
                        &__cur_s,
                        (__cur_i1).to_usize().unwrap_or(usize::MAX),
                    ) {
                        return Err(AverStr::from(
                            "Bytes.fromHex: expected an even number of hex characters",
                        ));
                    } else {
                        {
                            let __cur_c1 = aver_rt::str_cursor_code(
                                &__cur_s,
                                (__cur_i1).to_usize().unwrap_or(usize::MAX),
                            );
                            {
                                let __cur_i2 = aver_rt::AverInt::from_i64(
                                    aver_rt::str_cursor_next(
                                        &__cur_s,
                                        (__cur_i1).to_usize().unwrap_or(usize::MAX),
                                    ) as i64,
                                );
                                match crate::aver_generated::bytes::hexDigitValue__code(__cur_c2) {
                                    None => {
                                        return Err(aver_rt::AverStr::from({
                                            let mut __b = {
                                                let mut __b = {
                                                    let mut __b = aver_rt::Buffer::with_capacity(
                                                        (aver_rt::AverInt::from_i64(63))
                                                            .to_usize()
                                                            .unwrap_or(0),
                                                    );
                                                    __b.push_str(&AverStr::from("Bytes.fromHex: invalid hexadecimal character '"));
                                                    __b
                                                };
                                                __b.push_str(&aver_rt::AverStr::from(
                                                    aver_rt::aver_display(
                                                        &(aver_rt::AverStr::from(
                                                            aver_rt::str_cursor_head(
                                                                &__cur_s,
                                                                (__cur_i)
                                                                    .to_usize()
                                                                    .unwrap_or(usize::MAX),
                                                            ),
                                                        )),
                                                    ),
                                                ));
                                                __b
                                            };
                                            __b.push_str(&AverStr::from("'"));
                                            __b
                                        }));
                                    }
                                    Some(high @ _) => {
                                        match crate::aver_generated::bytes::hexDigitValue__code(
                                            __cur_c1,
                                        ) {
                                            None => {
                                                return Err(aver_rt::AverStr::from({
                                                    let mut __b = {
                                                        let mut __b = {
                                                            let mut __b =
                                                                aver_rt::Buffer::with_capacity(
                                                                    (aver_rt::AverInt::from_i64(
                                                                        63,
                                                                    ))
                                                                    .to_usize()
                                                                    .unwrap_or(0),
                                                                );
                                                            __b.push_str(&AverStr::from("Bytes.fromHex: invalid hexadecimal character '"));
                                                            __b
                                                        };
                                                        __b.push_str(&aver_rt::AverStr::from(
                                                            aver_rt::aver_display(
                                                                &(aver_rt::AverStr::from(
                                                                    aver_rt::str_cursor_head(
                                                                        &__cur_s,
                                                                        (__cur_i1)
                                                                            .to_usize()
                                                                            .unwrap_or(usize::MAX),
                                                                    ),
                                                                )),
                                                            ),
                                                        ));
                                                        __b
                                                    };
                                                    __b.push_str(&AverStr::from("'"));
                                                    __b
                                                }));
                                            }
                                            Some(low @ _) => {
                                                let __tco1 = __cur_i2;
                                                let __tco2 = aver_rt::byte_builder_push(
                                                    acc,
                                                    high.mul(&aver_rt::AverInt::from_i64(16))
                                                        .add(&low),
                                                );
                                                __cur_i = __tco1;
                                                acc = __tco2;
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
    }
}

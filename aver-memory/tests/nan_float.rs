use aver_memory::NanValue;

const BOX_PREFIX: u64 = 0x7FFC_0000_0000_0000;
const BOX_PREFIX_MASK: u64 = 0xFFFC_0000_0000_0000;
const BOX_MARKER_BIT: u64 = 1 << 50;
const BOX_TAG_SHIFT: u32 = 46;
const BOX_PAYLOAD_MASK: u64 = (1 << BOX_TAG_SHIFT) - 1;
const BOX_PAYLOAD_REF_BIT: u64 = 1 << 45;
const SIGN_BIT: u64 = 1 << 63;

fn assert_float_bits(input_bits: u64, expected_bits: u64) {
    let value = NanValue::new_float(f64::from_bits(input_bits));
    assert!(
        value.is_float(),
        "input {input_bits:#018x} became boxed as {:#018x}",
        value.bits()
    );
    assert!(!value.is_nan_boxed());
    assert_eq!(value.bits(), expected_bits);
    assert_eq!(value.as_float().to_bits(), expected_bits);
}

#[test]
fn noncolliding_float_bit_patterns_remain_exact() {
    for bits in [
        0.0f64.to_bits(),
        (-0.0f64).to_bits(),
        f64::INFINITY.to_bits(),
        f64::NEG_INFINITY.to_bits(),
        0x7FF0_0000_0000_0001, // positive signaling NaN
        0xFFF0_0000_0000_0001, // negative signaling NaN
        0x7FF8_0000_0000_0000, // canonical positive quiet NaN
        0xFFF8_0000_0000_0000, // canonical negative quiet NaN
        BOX_PREFIX - 1,        // last positive NaN below the boxed range
        BOX_PREFIX | SIGN_BIT, // negative mirror of the first boxed pattern
        u64::MAX,              // negative mirror of the last boxed pattern
    ] {
        assert_ne!(bits & BOX_PREFIX_MASK, BOX_PREFIX);
        assert_float_bits(bits, bits);
    }
}

#[test]
fn colliding_nan_matrix_clears_only_the_box_marker() {
    let payload_boundaries = [
        0,
        1,
        BOX_PAYLOAD_REF_BIT - 1,
        BOX_PAYLOAD_REF_BIT,
        BOX_PAYLOAD_REF_BIT + 1,
        BOX_PAYLOAD_MASK - 1,
        BOX_PAYLOAD_MASK,
    ];

    for tag in 0..=15 {
        for payload in payload_boundaries {
            let bits = BOX_PREFIX | (tag << BOX_TAG_SHIFT) | payload;
            assert_eq!(bits & BOX_PREFIX_MASK, BOX_PREFIX);

            let expected = bits & !BOX_MARKER_BIT;
            assert_float_bits(bits, expected);
            assert!(f64::from_bits(expected).is_nan());
            assert_eq!(bits ^ expected, BOX_MARKER_BIT);

            // The sign bit is part of the boxed-prefix mask. Therefore the
            // negative NaN with the same tag/payload never collides and must
            // retain every bit, including the marker bit.
            let negative_bits = bits | SIGN_BIT;
            assert_ne!(negative_bits & BOX_PREFIX_MASK, BOX_PREFIX);
            assert_float_bits(negative_bits, negative_bits);
        }
    }
}

#[test]
fn boxed_range_endpoints_are_remapped_outside_the_marker() {
    let boxed_end = BOX_PREFIX | ((1 << 50) - 1);
    let cases = [
        (BOX_PREFIX - 1, BOX_PREFIX - 1),
        (BOX_PREFIX, BOX_PREFIX & !BOX_MARKER_BIT),
        (boxed_end, boxed_end & !BOX_MARKER_BIT),
        (boxed_end + 1, boxed_end + 1),
    ];

    for (input, expected) in cases {
        assert_float_bits(input, expected);
    }
}

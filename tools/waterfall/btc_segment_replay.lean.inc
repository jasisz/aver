-- Exact stronger statements and ordinary Lean scripts returned by waterfall?.
-- Only the generated sample-membership premises were removed; source guards remain.

set_option maxHeartbeats 1000000 in
theorem place_law_staysUnderCap : ∀ (state : SegmentState) (blockBytes : Int), (((headerBytes + blockBytes) <= capBytes) && (state.used <= capBytes)) = true -> ((place state blockBytes).state.used <= capBytes) = true := by
  (
    expose_names
    simp_all (config := { maxSteps := 100000, maxDischargeDepth := 2 }) [Segment.headerBytes, Segment.capBytes,
      Segment.place]
    expose_names
    grind +lax [Segment.headerBytes, Segment.capBytes, Segment.rolls, Segment.rolled, Segment.appended])

#print axioms place_law_staysUnderCap

set_option maxHeartbeats 1000000 in
theorem place_law_recordEndsAtUsed : ∀ (state : SegmentState) (blockBytes : Int), ((state.used >= 0) && (blockBytes >= 0)) = true -> endsAtUsed (place state blockBytes) blockBytes = true := by
  (
    expose_names
    simp_all (config := { maxSteps := 100000, maxDischargeDepth := 2 }) [Segment.endsAtUsed, Segment.place]
    expose_names
    grind +lax [Segment.rolls, Segment.rolled, Segment.appended, Segment.headerBytes])

#print axioms place_law_recordEndsAtUsed

set_option maxHeartbeats 1000000 in
theorem place_law_consecutiveRecordsTile : ∀ (state : SegmentState) (first : Int) (second : Int), ((state.used >= 0) && ((first >= 0) && (second >= 0))) = true -> tiles (place state first) (place ((place state first).state) second) = true := by
  (
    expose_names
    simp_all (config := { maxSteps := 100000, maxDischargeDepth := 2 }) [Segment.tiles, Segment.place]
    expose_names
    grind +lax [Segment.opensNext, Segment.rolls, Segment.rolled, Segment.appended, Segment.place, Segment.followsOn])

#print axioms place_law_consecutiveRecordsTile

set_option maxHeartbeats 1000000 in
theorem agreesWithDisk_law_anyOtherSizeRefuses : ∀ (state : SegmentState) (delta : Int), (delta != 0) = true -> agreesWithDisk state (state.used + delta) = Except.error (s!"Segment {state.segment} is {(state.used + delta)} bytes on the disk where the count says {state.used}; a Location placed from the count would be {((delta : Int).natAbs : Int)} bytes off, so none was written, and the next open counts from the disk again") := by
  (
    expose_names
    grind +lax [Segment.agreesWithDisk])

#print axioms agreesWithDisk_law_anyOtherSizeRefuses

set_option maxHeartbeats 1000000 in
theorem place_law_agreesWithTheReader : ∀ (state : SegmentState) (blockBytes : Int), (((state.used >= 0) && (blockBytes >= 0)) && (!rolls state blockBytes)) = true -> readerAgrees state blockBytes (place state blockBytes) = true := by
  (
    expose_names
    simp_all (config := { maxSteps := 100000, maxDischargeDepth := 2 }) [Segment.rolls, Segment.readerAgrees,
      Segment.place]
    expose_names
    grind +lax [Segment.headerBytes, Segment.capBytes, Segment.rolled, Segment.appended, Segment.payloadAt,
      Segment.nextHeader, Segment.complete])

#print axioms place_law_agreesWithTheReader

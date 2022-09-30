module Record2 (RecordWithConflictingFieldName (..)) where


data RecordWithConflictingFieldName =
  AnotherRecord
  { firstName :: String }

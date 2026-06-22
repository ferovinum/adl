# @generated from ADL module common

import enum
import pydantic
import typing

import fero_adl.common.strings as common_strings

T = typing.TypeVar("T")

# A instant in time, represented as milliseconds from
# the epoch of "1970-01-01T00:00:00Z
class Instant(pydantic.RootModel[int]):
  pass

# A date in ISO8601 format
class LocalDate(pydantic.RootModel[str]):
  pass

# A time in ISO8601 format
class LocalTime(pydantic.RootModel[str]):
  pass

# A datetime in ISO8601 format
class LocalDateTime(pydantic.RootModel[str]):
  pass

class DayOfWeek(str, enum.Enum):
  monday: str = "monday"
  tuesday: str = "tuesday"
  wednesday: str = "wednesday"
  thursday: str = "thursday"
  friday: str = "friday"
  saturday: str = "saturday"
  sunday: str = "sunday"

# A duration in ISO8601 format
class Duration(pydantic.RootModel[str]):
  pass

# An IANA timezone
class Timezone(pydantic.RootModel[common_strings.StringNE]):
  pass

# A holder for paginated results
class Paginated(pydantic.BaseModel, typing.Generic[T]):
  # The paginated items
  items: list[T]
  # The offset used for this query
  current_offset: int
  # The size of the entire date set
  total_size: int

# Empty Struct (Used mostly for Void RPC responses)
class Unit(pydantic.BaseModel):
  pass

# Phantom type to capture a StringMap with a named string key type:
type StringKeyMap[_K, V] = dict[str, V]

# Naming aid for strings used as keys
type Key[_T] = str

# A value of type T along with the Key<T>
class WithKey(pydantic.BaseModel, typing.Generic[T]):
  key: "Key[T]"
  value: T

# Postgres array of strings type that is serialized in to a list of Strings
class StringList(pydantic.RootModel[list[str]]):
  pass

class TSVector(pydantic.RootModel[str]):
  pass

# Postgres Geography type that is serialized using GeoJson
class GeographyGeoJson(pydantic.RootModel[str]):
  pass

# Postgres Geometry type
class GeometryWKT(pydantic.RootModel[str]):
  pass

# A floating point decimal value
class BigDecimal(pydantic.RootModel[common_strings.StringNE]):
  pass

class ContactPerson(pydantic.BaseModel):
  name: common_strings.StringNE
  email: typing.Union[common_strings.StringNE, None] = pydantic.Field(default=None)
  phoneNumber: typing.Union[common_strings.StringNE, None] = pydantic.Field(default=None)

class Country(str, enum.Enum):
  AE: str = "AE"
  AF: str = "AF"
  AG: str = "AG"
  AI: str = "AI"
  AL: str = "AL"
  AM: str = "AM"
  AO: str = "AO"
  AQ: str = "AQ"
  AR: str = "AR"
  AS: str = "AS"
  AT: str = "AT"
  AU: str = "AU"
  AW: str = "AW"
  AX: str = "AX"
  AZ: str = "AZ"
  BA: str = "BA"
  BB: str = "BB"
  BD: str = "BD"
  BE: str = "BE"
  BF: str = "BF"
  BG: str = "BG"
  BH: str = "BH"
  BI: str = "BI"
  BJ: str = "BJ"
  BL: str = "BL"
  BM: str = "BM"
  BN: str = "BN"
  BO: str = "BO"
  BQ: str = "BQ"
  BR: str = "BR"
  BS: str = "BS"
  BT: str = "BT"
  BV: str = "BV"
  BW: str = "BW"
  BY: str = "BY"
  BZ: str = "BZ"
  CA: str = "CA"
  CC: str = "CC"
  CD: str = "CD"
  CF: str = "CF"
  CG: str = "CG"
  CH: str = "CH"
  CI: str = "CI"
  CK: str = "CK"
  CL: str = "CL"
  CM: str = "CM"
  CN: str = "CN"
  CO: str = "CO"
  CR: str = "CR"
  CU: str = "CU"
  CV: str = "CV"
  CW: str = "CW"
  CX: str = "CX"
  CY: str = "CY"
  CZ: str = "CZ"
  DE: str = "DE"
  DJ: str = "DJ"
  DK: str = "DK"
  DM: str = "DM"
  DO: str = "DO"
  DZ: str = "DZ"
  EC: str = "EC"
  EE: str = "EE"
  EG: str = "EG"
  EH: str = "EH"
  ER: str = "ER"
  ES: str = "ES"
  ET: str = "ET"
  FI: str = "FI"
  FJ: str = "FJ"
  FK: str = "FK"
  FM: str = "FM"
  FO: str = "FO"
  FR: str = "FR"
  GA: str = "GA"
  GB: str = "GB"
  GD: str = "GD"
  GE: str = "GE"
  GF: str = "GF"
  GG: str = "GG"
  GH: str = "GH"
  GI: str = "GI"
  GL: str = "GL"
  GM: str = "GM"
  GN: str = "GN"
  GP: str = "GP"
  GQ: str = "GQ"
  GR: str = "GR"
  GS: str = "GS"
  GT: str = "GT"
  GU: str = "GU"
  GW: str = "GW"
  GY: str = "GY"
  HK: str = "HK"
  HM: str = "HM"
  HN: str = "HN"
  HR: str = "HR"
  HT: str = "HT"
  HU: str = "HU"
  ID: str = "ID"
  IE: str = "IE"
  IL: str = "IL"
  IM: str = "IM"
  IN: str = "IN"
  IO: str = "IO"
  IQ: str = "IQ"
  IR: str = "IR"
  IS: str = "IS"
  IT: str = "IT"
  JE: str = "JE"
  JM: str = "JM"
  JO: str = "JO"
  JP: str = "JP"
  KE: str = "KE"
  KG: str = "KG"
  KH: str = "KH"
  KI: str = "KI"
  KM: str = "KM"
  KN: str = "KN"
  KP: str = "KP"
  KR: str = "KR"
  KW: str = "KW"
  KY: str = "KY"
  KZ: str = "KZ"
  LA: str = "LA"
  LB: str = "LB"
  LC: str = "LC"
  LI: str = "LI"
  LK: str = "LK"
  LR: str = "LR"
  LS: str = "LS"
  LT: str = "LT"
  LU: str = "LU"
  LV: str = "LV"
  LY: str = "LY"
  MA: str = "MA"
  MC: str = "MC"
  MD: str = "MD"
  ME: str = "ME"
  MF: str = "MF"
  MG: str = "MG"
  MH: str = "MH"
  MK: str = "MK"
  ML: str = "ML"
  MM: str = "MM"
  MN: str = "MN"
  MO: str = "MO"
  MP: str = "MP"
  MQ: str = "MQ"
  MR: str = "MR"
  MS: str = "MS"
  MT: str = "MT"
  MU: str = "MU"
  MV: str = "MV"
  MW: str = "MW"
  MX: str = "MX"
  MY: str = "MY"
  MZ: str = "MZ"
  NA: str = "NA"
  NC: str = "NC"
  NE: str = "NE"
  NF: str = "NF"
  NG: str = "NG"
  NI: str = "NI"
  NL: str = "NL"
  NO: str = "NO"
  NP: str = "NP"
  NR: str = "NR"
  NU: str = "NU"
  NZ: str = "NZ"
  OM: str = "OM"
  PA: str = "PA"
  PE: str = "PE"
  PF: str = "PF"
  PG: str = "PG"
  PH: str = "PH"
  PK: str = "PK"
  PL: str = "PL"
  PM: str = "PM"
  PN: str = "PN"
  PR: str = "PR"
  PS: str = "PS"
  PT: str = "PT"
  PW: str = "PW"
  PY: str = "PY"
  QA: str = "QA"
  RE: str = "RE"
  RO: str = "RO"
  RS: str = "RS"
  RU: str = "RU"
  RW: str = "RW"
  SA: str = "SA"
  SB: str = "SB"
  SC: str = "SC"
  SD: str = "SD"
  SE: str = "SE"
  SG: str = "SG"
  SH: str = "SH"
  SI: str = "SI"
  SJ: str = "SJ"
  SK: str = "SK"
  SL: str = "SL"
  SM: str = "SM"
  SN: str = "SN"
  SO: str = "SO"
  SR: str = "SR"
  SS: str = "SS"
  ST: str = "ST"
  SV: str = "SV"
  SX: str = "SX"
  SY: str = "SY"
  SZ: str = "SZ"
  TC: str = "TC"
  TD: str = "TD"
  TF: str = "TF"
  TG: str = "TG"
  TH: str = "TH"
  TJ: str = "TJ"
  TK: str = "TK"
  TL: str = "TL"
  TM: str = "TM"
  TN: str = "TN"
  TO: str = "TO"
  TR: str = "TR"
  TT: str = "TT"
  TV: str = "TV"
  TW: str = "TW"
  TZ: str = "TZ"
  UA: str = "UA"
  UG: str = "UG"
  UM: str = "UM"
  US: str = "US"
  UY: str = "UY"
  UZ: str = "UZ"
  VA: str = "VA"
  VC: str = "VC"
  VE: str = "VE"
  VG: str = "VG"
  VI: str = "VI"
  VN: str = "VN"
  VU: str = "VU"
  WF: str = "WF"
  WS: str = "WS"
  YE: str = "YE"
  YT: str = "YT"
  ZA: str = "ZA"
  ZM: str = "ZM"
  ZW: str = "ZW"

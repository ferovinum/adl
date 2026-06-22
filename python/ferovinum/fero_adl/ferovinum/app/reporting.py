# @generated from ADL module ferovinum.app.reporting

import enum
import pydantic
import typing

import fero_adl.common as common
import fero_adl.common.strings as common_strings
import fero_adl.common.tabular as common_tabular
import fero_adl.ferovinum.app.db as ferovinum_app_db
import fero_adl.sys.types as sys_types


class Value_B(pydantic.BaseModel):
  b: bool

class Value_D(pydantic.BaseModel):
  d: float

class Value_Dt(pydantic.BaseModel):
  dt: common.LocalDate

class Value_I64(pydantic.BaseModel):
  i64: int

class Value_J(pydantic.BaseModel):
  j: dict | None

class Value_N(pydantic.BaseModel):
  n: None

class Value_S(pydantic.BaseModel):
  s: str

class Value_Ts(pydantic.BaseModel):
  ts: common.LocalDateTime

class Value(pydantic.RootModel[typing.Union[Value_B | Value_D | Value_Dt | Value_I64 | Value_J | Value_N | Value_S | Value_Ts]]):
  pass

class Error(pydantic.BaseModel):
  message: str

type EType[T] = sys_types.Either[T, "Error"]

class EValue(pydantic.RootModel["EType["Value"]"]):
  pass

class BiFunctionInput(pydantic.BaseModel):
  x: str
  y: str

class Operator_Sum(pydantic.BaseModel):
  sum: "BiFunctionInput"

class Operator_Difference(pydantic.BaseModel):
  difference: "BiFunctionInput"

class Operator_Multiply(pydantic.BaseModel):
  multiply: "BiFunctionInput"

class Operator_Division(pydantic.BaseModel):
  division: "BiFunctionInput"

class Operator(pydantic.RootModel[typing.Union[Operator_Sum | Operator_Difference | Operator_Multiply | Operator_Division]]):
  pass

class AttributeOpP(pydantic.BaseModel):
  name: str
  operator: "Operator"

class AttributeOpE(pydantic.BaseModel):
  name: str
  value: "EValue"

class AttributeOpR(pydantic.RootModel["EType["AttributeOpE"]"]):
  pass

class BagP(pydantic.BaseModel):
  reportParams: list["Param"]

class BagR(pydantic.RootModel["EType[list["Result"]]"]):
  pass

class FxSpotP(pydantic.BaseModel):
  baseCurrency: common_strings.StringNE
  quoteCurrency: common_strings.StringNE
  date: typing.Union[common.LocalDate, None] = pydantic.Field(default=None)

class FxSpotE(pydantic.BaseModel):
  baseCurrency: common_strings.StringNE
  quoteCurrency: common_strings.StringNE
  exchangeRate: common.BigDecimal
  date: common.LocalDate

class FxSpotR(pydantic.RootModel["EType[list["FxSpotE"]]"]):
  pass

class CashflowP(pydantic.BaseModel):
  fromDate: typing.Union[common.LocalDate, None] = pydantic.Field(default=None)
  toDate: typing.Union[common.LocalDate, None] = pydantic.Field(default=None)
  includeContingent: bool = pydantic.Field(default=True)
  netPurchaseDeposit: bool = pydantic.Field(default=True)
  netSaleDeposit: bool = pydantic.Field(default=True)

class CashflowE(pydantic.BaseModel):
  date: common.LocalDate
  description: str
  reference: str
  currency: common_strings.StringNE
  amount: common.BigDecimal
  contingent: bool

class CashflowR(pydantic.RootModel["EType[list["CashflowE"]]"]):
  pass

class DealCountP(pydantic.BaseModel):
  includePreDeal: bool = pydantic.Field(default=False)

class DealCountR(pydantic.RootModel["EType[int]"]):
  pass

class DealRollCountP(pydantic.BaseModel):
  includeApplications: typing.Union[list[ferovinum_app_db.DealRollApplication], None] = pydantic.Field(default=None)

class DealRollCountR(pydantic.RootModel["EType[int]"]):
  pass

class DealRollHistoryP(pydantic.BaseModel):
  fromDate: typing.Union[common.LocalDate, None] = pydantic.Field(default=None)
  toDate: typing.Union[common.LocalDate, None] = pydantic.Field(default=None)
  recursiveSearch: bool = pydantic.Field(default=False)
  stopOnExpiry: bool = pydantic.Field(default=False)

class DealRollHistoryE(pydantic.BaseModel):
  date: common.LocalDate
  application: ferovinum_app_db.DealRollApplication
  reference: str
  fromDealLeg: ferovinum_app_db.DealLegId
  toDealLeg: ferovinum_app_db.DealLegId
  units: float

class DealRollHistoryR(pydantic.RootModel["EType[list["DealRollHistoryE"]]"]):
  pass

class DepletionForecastP(pydantic.BaseModel):
  includeRelatedProducts: bool = pydantic.Field(default=False)

class DepletionForecastE(pydantic.BaseModel):
  predictedFinalDate: common.LocalDate
  predictedUnitsAtCompulsorySaleDate: float
  rateOfSale: float

class DepletionForecastR(pydantic.RootModel["EType[list["DepletionForecastE"]]"]):
  pass

class FeeP(pydantic.BaseModel):
  annualised: bool = pydantic.Field(default=False)

class FeeE(pydantic.BaseModel):
  monthly: common.BigDecimal
  throughput: common.BigDecimal
  fulfilment: common.BigDecimal

class FeeR(pydantic.RootModel["EType["FeeE"]"]):
  pass

class FspP(pydantic.BaseModel):
  enableRounding: bool = pydantic.Field(default=True)
  includeCapExBreakdown: bool = pydantic.Field(default=True)
  traceDealRoll: bool = pydantic.Field(default=False)
  separateCapExFee: bool = pydantic.Field(default=False)

class FspCapExE(pydantic.BaseModel):
  id: ferovinum_app_db.CapitalisedExpenseId
  expenseDate: common.LocalDate
  totalPrice: common.BigDecimal
  basePrice: common.BigDecimal
  monthlyFee: common.BigDecimal
  throughputFee: common.BigDecimal

class FspE(pydantic.BaseModel):
  totalPrice: common.BigDecimal
  basePrice: common.BigDecimal
  depositPrice: common.BigDecimal
  monthlyFee: common.BigDecimal
  throughputFee: common.BigDecimal
  capitalisedExpenseTotalPrice: common.BigDecimal
  capitalisedExpenseBasePrice: common.BigDecimal
  capitalisedExpenseMonthlyFee: common.BigDecimal
  capitalisedExpenseThroughputFee: common.BigDecimal
  capitalisedExpenses: list["FspCapExE"]
  residualRounding: common.BigDecimal

class FspR(pydantic.RootModel["EType["FspE"]"]):
  pass

class InitialUnitsP(pydantic.BaseModel):
  normalise: bool = pydantic.Field(default=True)

class InitialUnitsE(pydantic.BaseModel):
  productCode: common_strings.StringNE
  units: float

class InitialUnitsR(pydantic.RootModel["EType[list["InitialUnitsE"]]"]):
  pass

class MonthlyFeeNextDateP(pydantic.BaseModel):
  pivotDate: typing.Union[common.LocalDate, None]

class MonthlyFeeNextDateR(pydantic.RootModel["EType[list[common.LocalDate]]"]):
  pass

class PerDealP(pydantic.BaseModel):
  attributes: list[str]
  reportParam: "Param"

type PerDealE = sys_types.Pair[sys_types.Map[str, "EValue"], "Result"]

class PerDealR(pydantic.RootModel["EType[list["PerDealE"]]"]):
  pass

class RemainingUnitsP(pydantic.BaseModel):
  includeBuyback: bool = pydantic.Field(default=False)

class RemainingUnitsBuybackE(pydantic.BaseModel):
  date: common.LocalDate
  units: float

class RemainingUnitsE(pydantic.BaseModel):
  productCode: common_strings.StringNE
  units: float
  compulsory: typing.Union["RemainingUnitsBuybackE", None]
  final: typing.Union["RemainingUnitsBuybackE", None]

class RemainingUnitsR(pydantic.RootModel["EType[list["RemainingUnitsE"]]"]):
  pass

class RepurchaseType(str, enum.Enum):
  expiryRoll: str = "expiryRoll"
  newDealDelivery: str = "newDealDelivery"
  newDealLoss: str = "newDealLoss"
  productionLoss: str = "productionLoss"
  productionRun: str = "productionRun"
  revaluation: str = "revaluation"
  saleOrder: str = "saleOrder"
  stockMovementRoll: str = "stockMovementRoll"

class RepurchaseP(pydantic.BaseModel):
  includeTypes: typing.Union[list["RepurchaseType"], None] = pydantic.Field(default=None)
  includeCanceled: bool = pydantic.Field(default=True)
  fromDate: typing.Union[common.LocalDate, None] = pydantic.Field(default=None)
  toDate: typing.Union[common.LocalDate, None] = pydantic.Field(default=None)

class RepurchaseE(pydantic.BaseModel):
  date: common.LocalDate
  invoiceNumber: str
  productCode: str
  price: common.BigDecimal
  units: float
  deliveryOption: ferovinum_app_db.DeliveryOption
  repurchaseType: "RepurchaseType"
  lastStatusEvent: typing.Union[ferovinum_app_db.SaleOrderStatusEvent, None] = pydantic.Field(default=None)
  dutyTotal: typing.Union[common.BigDecimal, None] = pydantic.Field(default=None)
  vatTotal: typing.Union[common.BigDecimal, None] = pydantic.Field(default=None)

class RepurchaseR(pydantic.RootModel["EType[list["RepurchaseE"]]"]):
  pass

class StockMovementP(pydantic.BaseModel):
  includeReserved: bool = pydantic.Field(default=False)

class StockMovementE(pydantic.BaseModel):
  date: common.LocalDate
  description: str
  productCode: common_strings.StringNE
  reference: str
  storageLocationSiteCode: common_strings.StringNE
  units: float

class StockMovementR(pydantic.RootModel["EType[list["StockMovementE"]]"]):
  pass

class UnitsRemainingP(pydantic.BaseModel):
  includeReserved: bool = pydantic.Field(default=False)
  includeZeroBalance: bool = pydantic.Field(default=False)

class UnitsRemainingBuybackE(pydantic.BaseModel):
  date: common.LocalDate
  units: float

class UnitsRemainingE(pydantic.BaseModel):
  productCode: common_strings.StringNE
  units: float
  compulsory: "UnitsRemainingBuybackE"
  final: "UnitsRemainingBuybackE"

class UnitsRemainingR(pydantic.RootModel["EType[list["UnitsRemainingE"]]"]):
  pass

class Param_AttributeOp(pydantic.BaseModel):
  attributeOp: "AttributeOpP"

class Param_Bag(pydantic.BaseModel):
  bag: "BagP"

class Param_Cashflow(pydantic.BaseModel):
  cashflow: "CashflowP"

class Param_DealCount(pydantic.BaseModel):
  dealCount: "DealCountP"

class Param_DealRollCount(pydantic.BaseModel):
  dealRollCount: "DealRollCountP"

class Param_DealRollHistory(pydantic.BaseModel):
  dealRollHistory: "DealRollHistoryP"

class Param_DepletionForecast(pydantic.BaseModel):
  depletionForecast: "DepletionForecastP"

class Param_Fee(pydantic.BaseModel):
  fee: "FeeP"

class Param_Fsp(pydantic.BaseModel):
  fsp: "FspP"

class Param_FxSpot(pydantic.BaseModel):
  fxSpot: "FxSpotP"

class Param_InitialUnits(pydantic.BaseModel):
  initialUnits: "InitialUnitsP"

class Param_MonthlyFeeNextDate(pydantic.BaseModel):
  monthlyFeeNextDate: "MonthlyFeeNextDateP"

class Param_PerDeal(pydantic.BaseModel):
  perDeal: "PerDealP"

class Param_RemainingUnits(pydantic.BaseModel):
  remainingUnits: "RemainingUnitsP"

class Param_Repurchase(pydantic.BaseModel):
  repurchase: "RepurchaseP"

class Param_StockMovement(pydantic.BaseModel):
  stockMovement: "StockMovementP"

class Param_UnitsRemaining(pydantic.BaseModel):
  unitsRemaining: "UnitsRemainingP"

class Param(pydantic.RootModel[typing.Union[Param_AttributeOp | Param_Bag | Param_Cashflow | Param_DealCount | Param_DealRollCount | Param_DealRollHistory | Param_DepletionForecast | Param_Fee | Param_Fsp | Param_FxSpot | Param_InitialUnits | Param_MonthlyFeeNextDate | Param_PerDeal | Param_RemainingUnits | Param_Repurchase | Param_StockMovement | Param_UnitsRemaining]]):
  pass

class Result_AttributeOp(pydantic.BaseModel):
  attributeOp: "AttributeOpR"

class Result_Bag(pydantic.BaseModel):
  bag: "BagR"

class Result_Cashflow(pydantic.BaseModel):
  cashflow: "CashflowR"

class Result_DealCount(pydantic.BaseModel):
  dealCount: "DealCountR"

class Result_DealRollCount(pydantic.BaseModel):
  dealRollCount: "DealRollCountR"

class Result_DealRollHistory(pydantic.BaseModel):
  dealRollHistory: "DealRollHistoryR"

class Result_DepletionForecast(pydantic.BaseModel):
  depletionForecast: "DepletionForecastR"

class Result_Fee(pydantic.BaseModel):
  fee: "FeeR"

class Result_Fsp(pydantic.BaseModel):
  fsp: "FspR"

class Result_FxSpot(pydantic.BaseModel):
  fxSpot: "FxSpotR"

class Result_InitialUnits(pydantic.BaseModel):
  initialUnits: "InitialUnitsR"

class Result_MonthlyFeeNextDate(pydantic.BaseModel):
  monthlyFeeNextDate: "MonthlyFeeNextDateR"

class Result_PerDeal(pydantic.BaseModel):
  perDeal: "PerDealR"

class Result_RemainingUnits(pydantic.BaseModel):
  remainingUnits: "RemainingUnitsR"

class Result_Repurchase(pydantic.BaseModel):
  repurchase: "RepurchaseR"

class Result_StockMovement(pydantic.BaseModel):
  stockMovement: "StockMovementR"

class Result_UnitsRemaining(pydantic.BaseModel):
  unitsRemaining: "UnitsRemainingR"

class Result(pydantic.RootModel[typing.Union[Result_AttributeOp | Result_Bag | Result_Cashflow | Result_DealCount | Result_DealRollCount | Result_DealRollHistory | Result_DepletionForecast | Result_Fee | Result_Fsp | Result_FxSpot | Result_InitialUnits | Result_MonthlyFeeNextDate | Result_PerDeal | Result_RemainingUnits | Result_Repurchase | Result_StockMovement | Result_UnitsRemaining]]):
  pass

class DealsLoader(pydantic.BaseModel):
  filter: common_tabular.FieldPredicate = pydantic.Field(default={"literal": True})
  organisations: sys_types.Set[str] = pydantic.Field(default=[])
  onlyLive: bool = pydantic.Field(default=True)
  liveOn: typing.Union[common.LocalDate, None] = pydantic.Field(default=None)
  includeDepleted: bool = pydantic.Field(default=False)
  depletedOn: typing.Union[common.LocalDate, None] = pydantic.Field(default=None)

class FormatterP_Csv(pydantic.BaseModel):
  csv: "CsvP"

class FormatterP_Json(pydantic.BaseModel):
  json: "JsonP"

class FormatterP_Tabular(pydantic.BaseModel):
  tabular: "TabularP"

class FormatterP(pydantic.RootModel[typing.Union[FormatterP_Csv | FormatterP_Json | FormatterP_Tabular]]):
  pass

class CsvP(pydantic.BaseModel):
  quoteCharacter: str = pydantic.Field(default="")

class JsonP(pydantic.BaseModel):
  indentSize: int = pydantic.Field(default=2, ge=0, le=4294967295)

class TabularP(pydantic.BaseModel):
  unstackReportName: bool = pydantic.Field(default=False)
  throwOverlapColumnName: bool = pydantic.Field(default=False)

type Record = dict[str, "EValue"]

class Table(pydantic.BaseModel):
  records: list["Record"]

class FormatterR_Csv(pydantic.BaseModel):
  csv: str

class FormatterR_Json(pydantic.BaseModel):
  json: dict | None

class FormatterR_Tabular(pydantic.BaseModel):
  tabular: "Table"

class FormatterR(pydantic.RootModel[typing.Union[FormatterR_Csv | FormatterR_Json | FormatterR_Tabular]]):
  pass

class ReportReq(pydantic.BaseModel):
  dealsLoader: "DealsLoader"
  refDate: common.LocalDate
  reportParam: "Param"
  formatter: typing.Union["FormatterP", None] = pydantic.Field(default=None)

class ReportResp(pydantic.BaseModel):
  result: "Result"
  output: typing.Union["FormatterR", None] = pydantic.Field(default=None)

# @generated from ADL module ferovinum.app.db

import enum
import pydantic
import typing

import fero_adl.common as common
import fero_adl.common.db as common_db
import fero_adl.common.strings as common_strings
import fero_adl.ferovinum.app.types as ferovinum_app_types
import fero_adl.sys.types as sys_types


class YearMonth(pydantic.RootModel[str]):
  pass

class YearOnly(pydantic.RootModel[str]):
  pass

class PartialDate_FullDate(pydantic.BaseModel):
  fullDate: common.LocalDate

class PartialDate_YearMonth(pydantic.BaseModel):
  yearMonth: "YearMonth"

class PartialDate_YearOnly(pydantic.BaseModel):
  yearOnly: "YearOnly"

class PartialDate(pydantic.RootModel[typing.Union[PartialDate_FullDate | PartialDate_YearMonth | PartialDate_YearOnly]]):
  pass

class ApprovalStatus(str, enum.Enum):
  PENDING: str = "PENDING"
  APPROVED: str = "APPROVED"
  REJECTED: str = "REJECTED"

class ClientSegment(str, enum.Enum):
  WineProducer: str = "WineProducer"
  SpiritsDistiller: str = "SpiritsDistiller"
  SpiritsTrader: str = "SpiritsTrader"
  Distributor: str = "Distributor"

class FacilitySegment(str, enum.Enum):
  Producer: str = "Producer"
  Wholesaler: str = "Wholesaler"

class Organisation(pydantic.BaseModel):
  businessName: common_strings.StringNE
  shortName: typing.Union[common_strings.StringNE, None]
  # The Alcohol Wholesaler Registration Scheme (AWRS) Registration Number
  awrsRegistrationNumber: typing.Union[common_strings.StringNE, None]
  d2cFulfilmentFee: common.BigDecimal
  addressLine1: typing.Union[str, None]
  addressLine2: typing.Union[str, None]
  addressLine3: typing.Union[str, None]
  town: typing.Union[str, None]
  postCode: typing.Union[str, None]
  masterAgreementSignedDates: list[common.LocalDate]
  currency: "Currency"
  clientSegment: "ClientSegment"
  facilitySegment: "FacilitySegment"
  # Fixed fee applied to purchase requests (in %) 
  nominatedPurchaserFee: typing.Union[common.BigDecimal, None]
  # Default fees to be used in new deal requests and purchase requests
  defaultMonthlyFee: typing.Union["MonthlyFee", None]
  defaultThroughputFee: typing.Union[common.BigDecimal, None]
  logoUrl: typing.Union[str, None]
  # Default contact email to be used for contacting the org from emails.
  contactEmail: typing.Union[str, None]
  # Emails that will receive invoice emails
  accountsEmails: list[str]
  defaultFeroFxServiceCharge: common.BigDecimal = pydantic.Field(default="0")
  vatNumber: typing.Union[common_strings.StringNE, None]
  crnNumber: typing.Union[common_strings.StringNE, None]
  wowgrNumber: typing.Union[common_strings.StringNE, None]

type OrganisationId = common_db.DbKey["Organisation"]

class GuardedFeature(str, enum.Enum):
  newStockProcurement: str = "newStockProcurement"
  existingStockProcurement: str = "existingStockProcurement"
  nominatedPurchaser: str = "nominatedPurchaser"
  productionOrder: str = "productionOrder"
  expiryRoll: str = "expiryRoll"
  stockMovement: str = "stockMovement"
  stockRevaluation: str = "stockRevaluation"
  saleOrderDeliveryOrder: str = "saleOrderDeliveryOrder"
  do_not_advance_duty_for_trade_sale_from_own_location: str = "do_not_advance_duty_for_trade_sale_from_own_location"
  allow_negative_receivable_trade_sales: str = "allow_negative_receivable_trade_sales"
  inplatform_invoices: str = "inplatform_invoices"
  sale_order_delivery_to_purchasers: str = "sale_order_delivery_to_purchasers"
  custom_third_party_sale_invoice: str = "custom_third_party_sale_invoice"
  disable_invoice_email_notification: str = "disable_invoice_email_notification"
  new_trade_sales_ui: str = "new_trade_sales_ui"
  enable_storefront_ui: str = "enable_storefront_ui"
  trade_sale_custom_and_additional_delivery_costs: str = "trade_sale_custom_and_additional_delivery_costs"
  trade_sale_product_attribute_selection: str = "trade_sale_product_attribute_selection"

class OrgFeatures(pydantic.BaseModel):
  orgId: "OrganisationId"
  feature: "GuardedFeature"

# Details for a user
class AppUser(pydantic.BaseModel):
  fullname: common_strings.StringNE
  email: common_strings.StringNE
  jobTitle: str
  adminRole: typing.Union["AdminRole", None]
  hashedPassword: common_strings.StringNE
  passwordExpiresAt: typing.Union[common.Instant, None]

class AdminRole(str, enum.Enum):
  admin: str = "admin"
  superAdmin: str = "superAdmin"
  opsAlertsDashboard: str = "opsAlertsDashboard"

type AppUserId = common_db.DbKey["AppUser"]

class AppUserOrganisation(pydantic.BaseModel):
  appUserId: "AppUserId"
  organisationId: "OrganisationId"
  permissions: list["OrgUserPermission"]

class OrgUserPermission(str, enum.Enum):
  designate_sign_delivery_order: str = "designate_sign_delivery_order"
  action_trade_sales: str = "action_trade_sales"
  action_new_deal: str = "action_new_deal"
  action_expiring_stock: str = "action_expiring_stock"
  action_production_roll: str = "action_production_roll"
  action_sale_order: str = "action_sale_order"
  action_stock_revaluation: str = "action_stock_revaluation"
  action_stock_movement: str = "action_stock_movement"
  notify_signed_delivery_order: str = "notify_signed_delivery_order"
  notify_finance: str = "notify_finance"
  notify_trade_sales: str = "notify_trade_sales"
  notify_new_deal: str = "notify_new_deal"
  notify_expiring_stock: str = "notify_expiring_stock"
  notify_general: str = "notify_general"
  notify_production_roll: str = "notify_production_roll"
  notify_sale_order: str = "notify_sale_order"
  notify_stock_movement: str = "notify_stock_movement"
  notify_stock_revaluation: str = "notify_stock_revaluation"
  notify_logistics: str = "notify_logistics"

type AppUserOrganisationId = common_db.DbKey["AppUserOrganisation"]

class AppUserStorageLocation(pydantic.BaseModel):
  appUserId: "AppUserId"
  storageLocationId: "StorageLocationId"

type AppUserStorageLocationId = common_db.DbKey["AppUserStorageLocation"]

class AppUserSupplier(pydantic.BaseModel):
  appUserId: "AppUserId"
  supplierId: "SupplierId"

type AppUserSupplierId = common_db.DbKey["AppUserSupplier"]

class AppUserCarrier(pydantic.BaseModel):
  appUserId: "AppUserId"
  carrierId: "CarrierId"

type AppUserCarrierId = common_db.DbKey["AppUserCarrier"]

class Address(pydantic.BaseModel):
  streetName: common_strings.StringNE
  line1: typing.Union[common_strings.StringNE, None] = pydantic.Field(default=None)
  line2: typing.Union[common_strings.StringNE, None] = pydantic.Field(default=None)
  town: common_strings.StringNE
  state: typing.Union[common_strings.StringNE, None] = pydantic.Field(default=None)
  postCode: common_strings.StringNE
  country: common.Country

type AddressId = common_db.DbKey["Address"]

class VisionEndpoint(str, enum.Enum):
  lcbDefault: str = "lcbDefault"
  lcbHillington: str = "lcbHillington"
  lcbGroup3: str = "lcbGroup3"
  ehdDefault: str = "ehdDefault"
  rarterGroup1: str = "rarterGroup1"

class ExternalEndpoint_Vision(pydantic.BaseModel):
  vision: "VisionEndpoint"

class ExternalEndpoint_Amsterdam(pydantic.BaseModel):
  amsterdam: None

class ExternalEndpoint_LawDistribution(pydantic.BaseModel):
  lawDistribution: None

class ExternalEndpoint(pydantic.RootModel[typing.Union[ExternalEndpoint_Vision | ExternalEndpoint_Amsterdam | ExternalEndpoint_LawDistribution]]):
  pass

class WarehouseGroup(str, enum.Enum):
  # London City Bond
  lcb: str = "lcb"
  ehd: str = "ehd"
  # Amsterdam warehouse company
  awc: str = "awc"

class DeliveryInstructionConfig(pydantic.BaseModel):
  telephoneRequired: bool
  emailRequired: bool
  deliveryInstructionsMax: int

class StorageLocation(pydantic.BaseModel):
  locationName: common_strings.StringNE
  ownerOrganisationId: typing.Union["OrganisationId", None]
  ferovinumAccountCode: typing.Union[str, None]
  siteCode: typing.Union[str, None]
  addressLine1: typing.Union[str, None]
  addressLine2: typing.Union[str, None]
  town: typing.Union[str, None]
  postCode: typing.Union[str, None]
  country: typing.Union[common.Country, None]
  billingBasis: "BillingBasis"
  rotationNumberRequired: bool
  locale: "Locale"
  publiclyAvailable: bool
  currency: "Currency"
  hasD2cFulfilment: bool
  endpoint: typing.Union["ExternalEndpoint", None]
  ownerId: typing.Union["OrganisationId", None]
  warehouseGroup: typing.Union["WarehouseGroup", None]
  deliveryInstructionConfig: typing.Union["DeliveryInstructionConfig", None]
  deliveryCountries: list[common.Country]

class BillingBasis(str, enum.Enum):
  # Implies that clients are charged based on units ordered
  ordered: str = "ordered"
  # Implies that clients are charged based on units finished
  finished: str = "finished"

type StorageLocationId = common_db.DbKey["StorageLocation"]

class OrganisationStorageLocationAccess(pydantic.BaseModel):
  organisationId: "OrganisationId"
  storageLocationId: "StorageLocationId"

# Wine / Spirits
class Product(pydantic.BaseModel):
  code: common_strings.StringNE
  name: common_strings.StringNE
  producerName: common_strings.StringNE
  # Vintage / Distillation Date (or NonVintage flag)
  productDate: "ProductDate"
  # Vessel size (currently null for non-singles products as in casks, tanks etc...)
  vesselSize: typing.Union["VesselSize", None]
  unitType: "UnitType"
  countryOfOrigin: common.Country
  regionOrigin: str
  # Alcohol by volume (percentage 0.0 - 100.0)  Null for presently unknown.
  alcoholByVolumePc: float
  productType: "ProductType"
  alcoholDetail: "AlcoholDetail"
  vesselType: "VesselType"
  ownerId: "OrganisationId"
  updatedAt: common.Instant
  physicalDetails: typing.Union["PhysicalDetails", None]

# Temporary physical details for Moth hotfix
class PhysicalDetails(pydantic.BaseModel):
  batchNumber: str
  expiryDate: common.LocalDate

# -----------------------------------------------------------------------------------------------
class SaleProduct(pydantic.BaseModel):
  productId: "ProductId"
  hsCode: typing.Union[str, None]

type SaleProductId = common_db.DbKey["SaleProduct"]

# ===============================================================================================
# NEW PRODUCT MODEL
# ===============================================================================================
# *temporarily* disabled pending migration
# -----------------------------------------------------------------------------------------------
# -----------------------------------------------------------------------------------------------
class AlcoholDetail_CaskedWhisky(pydantic.BaseModel):
  caskedWhisky: "CaskedWhisky"

class AlcoholDetail_BundledSingles(pydantic.BaseModel):
  bundledSingles: list["BundledSinglesItem"]

class AlcoholDetail_Unknown(pydantic.BaseModel):
  unknown: "Empty"

class AlcoholDetail(pydantic.RootModel[typing.Union[AlcoholDetail_CaskedWhisky | AlcoholDetail_BundledSingles | AlcoholDetail_Unknown]]):
  pass

class Empty(pydantic.BaseModel):
  pass

class BundledSinglesItem(pydantic.BaseModel):
  note: str
  alcoholByVolumePc: float
  numberOfSingles: int = pydantic.Field(ge=0, le=4294967295)
  centilitresPerSingle: float
  productType: typing.Union["ProductType", None]

# -----------------------------------------------------------------------------------------------
# WHISKY
class CaskedWhisky(pydantic.BaseModel):
  area: typing.Union[str, None]
  liquid: "WhiskyLiquid"
  make: typing.Union[str, None]
  aysDate: common.LocalDate
  stage: typing.Union["WhiskyStage", None]
  caskFormat: typing.Union["CaskFormat", None]
  caskFill: typing.Union["CaskFill", None]
  caskComposition: typing.Union["CaskComposition", None]
  caskWood: typing.Union["CaskWood", None]
  filledAt: typing.Union["PartialDate", None]
  regaugedAt: typing.Union["PartialDate", None]
  caskOrientation: typing.Union["CaskOrientation", None]
  caskGeographicOrigin: typing.Union["CaskGeographicOrigin", None]
  caskNumber: typing.Union[str, None]
  distilleryProductionParcel: typing.Union[str, None]
  locationInfo: typing.Union[str, None]
  additionalNotes: typing.Union[str, None]

class WhiskyLiquid(str, enum.Enum):
  singleMalt: str = "singleMalt"
  blendedMalt: str = "blendedMalt"
  singleGrain: str = "singleGrain"
  blendedGrain: str = "blendedGrain"
  blended: str = "blended"
  rye: str = "rye"
  singlePotStill: str = "singlePotStill"

class WhiskyStage(str, enum.Enum):
  original: str = "original"
  reracked: str = "reracked"

class CaskFill(str, enum.Enum):
  first: str = "first"
  second: str = "second"
  refill: str = "refill"

class CaskFormat(str, enum.Enum):
  barrel: str = "barrel"
  hogshead: str = "hogshead"
  barrique: str = "barrique"
  puncheon: str = "puncheon"
  butt: str = "butt"

class CaskComposition(str, enum.Enum):
  bourbon: str = "bourbon"
  cognac: str = "cognac"
  rye: str = "rye"
  rum: str = "rum"
  sherry: str = "sherry"
  virgin: str = "virgin"
  wine: str = "wine"
  virginOak: str = "virginOak"

class CaskWood(str, enum.Enum):
  virgin: str = "virgin"
  rechar: str = "rechar"

class CaskOrientation(str, enum.Enum):
  rack: str = "rack"
  pallet: str = "pallet"

class CaskGeographicOrigin(str, enum.Enum):
  am: str = "am"
  eu: str = "eu"

# WHISKY END
# -----------------------------------------------------------------------------------------------
# WINE
# WINE END
# -----------------------------------------------------------------------------------------------
class VesselVolume_Centilitres(pydantic.BaseModel):
  centilitres: float

class VesselVolume_Litres(pydantic.BaseModel):
  litres: float

class VesselVolume_Hectolitres(pydantic.BaseModel):
  hectolitres: float

class VesselVolume(pydantic.RootModel[typing.Union[VesselVolume_Centilitres | VesselVolume_Litres | VesselVolume_Hectolitres]]):
  pass

class Cask(pydantic.BaseModel):
  lpa: float

class VesselType_Cask(pydantic.BaseModel):
  cask: "Cask"

class VesselType_Unknown(pydantic.BaseModel):
  unknown: "Empty"

class VesselType(pydantic.RootModel[typing.Union[VesselType_Cask | VesselType_Unknown]]):
  pass

# END NEW PRODUCT MODEL
# ===============================================================================================
class CaseVessel(pydantic.BaseModel):
  numberOfSingles: int = pydantic.Field(ge=0, le=4294967295)
  centilitresPerSingle: float

# Size of a container in different measurements depending on the product type
class VesselSize_Centilitres(pydantic.BaseModel):
  centilitres: float

class VesselSize_Case(pydantic.BaseModel):
  case: "CaseVessel"

class VesselSize(pydantic.RootModel[typing.Union[VesselSize_Centilitres | VesselSize_Case]]):
  pass

# Wine / Spirits Public Products Legacy Table
# Easy migration and prevent information loss
class LegacyPublicProduct(pydantic.BaseModel):
  code: common_strings.StringNE
  name: common_strings.StringNE
  producerName: common_strings.StringNE
  productDate: "ProductDate"
  unitSize: float
  unitType: "UnitType"
  countryOfOrigin: common.Country
  regionOrigin: str
  alcoholByVolumePc: typing.Union[float, None]
  productType: "ProductType"
  updatedAt: common.Instant
  # defines the link between this legacy public product and any private product that was created from it
  linkedPrivateProducts: list[common_db.DbKey["Product"]]

class ProductDate_NonVintage(pydantic.BaseModel):
  nonVintage: None

class ProductDate_VintageYear(pydantic.BaseModel):
  vintageYear: "LocalDateYear"

class ProductDate(pydantic.RootModel[typing.Union[ProductDate_NonVintage | ProductDate_VintageYear]]):
  pass

type LocalDateYear = int

class TopLevelUnitType(str, enum.Enum):
  cask: str = "cask"
  bottles: str = "bottles"
  cans: str = "cans"
  tank: str = "tank"
  case: str = "case"

class UnitType(str, enum.Enum):
  cask: str = "cask"
  bottles: str = "bottles"
  bottlesCrownCap: str = "bottlesCrownCap"
  bottlesUnlabelled: str = "bottlesUnlabelled"
  cansUnlabelled: str = "cansUnlabelled"
  cans: str = "cans"
  tank: str = "tank"
  case: str = "case"

class ProductType(str, enum.Enum):
  still: str = "still"
  sparkling: str = "sparkling"
  fortified: str = "fortified"
  spirits: str = "spirits"
  whisky: str = "whisky"
  tequila: str = "tequila"
  mezcal: str = "mezcal"
  rum: str = "rum"
  gin: str = "gin"
  vodka: str = "vodka"
  brandy: str = "brandy"
  readyToDrink: str = "readyToDrink"
  liqueurs: str = "liqueurs"
  other: str = "other"
  nonalcoholic: str = "nonalcoholic"

type ProductId = common_db.DbKey["Product"]

class Locale(str, enum.Enum):
  London: str = "London"
  US: str = "US"

class Currency(str, enum.Enum):
  GBP: str = "GBP"
  USD: str = "USD"
  EUR: str = "EUR"
  AUD: str = "AUD"
  JPY: str = "JPY"

class Deal(pydantic.BaseModel):
  # Counterparty
  organisationId: "OrganisationId"
  dealNumber: common_strings.StringNE
  dealDate: common.LocalDate
  locale: "Locale" = pydantic.Field(default="London")
  currency: "Currency"

type DealId = common_db.DbKey["Deal"]

class DatedValue(pydantic.BaseModel):
  date: common.LocalDate
  value: common.BigDecimal

class Interpolation(str, enum.Enum):
  None_: str = "None_"
  Step: str = "Step"

class Period(str, enum.Enum):
  None_: str = "None_"
  Annual: str = "Annual"
  Month: str = "Month"

class RatesCurve(pydantic.BaseModel):
  code: common_strings.StringNE
  interpolation: "Interpolation"
  period: "Period"
  points: list["DatedValue"]

type RatesCurveId = common_db.DbKey["RatesCurve"]

type FixedRate = common.BigDecimal

class VariableRate(pydantic.BaseModel):
  base: "FixedRate"
  reference: "RatesCurveId"

class MonthlyFee_Fixed(pydantic.BaseModel):
  fixed: "FixedRate"

class MonthlyFee_Variable(pydantic.BaseModel):
  variable: "VariableRate"

class MonthlyFee(pydantic.RootModel[typing.Union[MonthlyFee_Fixed | MonthlyFee_Variable]]):
  pass

# A deal leg: a product in a deal.
class DealLeg(pydantic.BaseModel):
  dealId: "DealId"
  productId: "ProductId"
  legIdentifier: common_strings.StringNE
  # Quantity of the product contained in this DealLeg in different measurements
  # depending on the product type
  numberOfUnits: "NumberOfUnits"
  dutyPaid: bool
  purchasePricePerUnit: common.BigDecimal
  depositPricePerUnit: common.BigDecimal
  throughputFeePc: common.BigDecimal
  storageLocationId: "StorageLocationId"
  # Indicates the date at which this deal leg was depleted by sales. Should
  # only be set if the availability status is "depleted" 
  depletedAt: typing.Union[common.LocalDate, None]
  # Defines the availability status of this deal leg
  availabilityStatus: "DealLegAvailabilityStatus"
  paymentDate: common.LocalDate
  deliveryDate: typing.Union[common.LocalDate, None]
  # Compulsory sale date (CSD)
  compulsorySaleDate: common.LocalDate
  # CSD proportion
  compulsorySalePc: common.BigDecimal
  finalDate: common.LocalDate
  # Roll number (amount of times re-negotiated/extended)
  rollNumber: int = pydantic.Field(ge=0, le=4294967295)
  # Monthly fee specification
  monthlyFee: "MonthlyFee"
  rotationNumber: typing.Union[common_strings.StringNE, None]
  rotationLine: typing.Union[int, None]
  # Charge throughput fee on capex's
  capexThroughputFeePc: common.BigDecimal
  # Mutable field that represents the current organisation intention with the remaining units in this the deal leg
  # as they are approaching the CSD / FS date (30 days prior to the CSD / FS date)
  # the absence means that org hasn't expressed an intention on what to do with the remaining units in this deal leg close to expiry day
  expiryRollIntention: typing.Union["ExpiryRollIntention", None]
  # Start date for monthly fee calculation
  monthlyFeeStartDate: common.LocalDate

# Represents the intention of the org to extend the deal
class ExpiryRollIntention_RejectionReason(pydantic.BaseModel):
  rejectionReason: "ExpiryRollRejectionReason"

class ExpiryRollIntention_NumberOfUnitsToRoll(pydantic.BaseModel):
  numberOfUnitsToRoll: "NumberOfUnits"

class ExpiryRollIntention(pydantic.RootModel[typing.Union[ExpiryRollIntention_RejectionReason | ExpiryRollIntention_NumberOfUnitsToRoll]]):
  pass

class ExpiryRollRejectionReason(pydantic.BaseModel):
  internalReason: str
  # Rejection reason to be displayed to the user
  publicReason: str

# Represents the different measure metrics that the platform uses to capture quantities of a product
# Should always match QuantityUnit below
class NumberOfUnits_Singles(pydantic.BaseModel):
  singles: int

class NumberOfUnits_LitresOfPureAlcohol(pydantic.BaseModel):
  litresOfPureAlcohol: float

class NumberOfUnits_Hectolitres(pydantic.BaseModel):
  hectolitres: float

class NumberOfUnits(pydantic.RootModel[typing.Union[NumberOfUnits_Singles | NumberOfUnits_LitresOfPureAlcohol | NumberOfUnits_Hectolitres]]):
  pass

class QuantityUnit(str, enum.Enum):
  # physical individual containers (bottles, cans, etc...)
  singles: str = "singles"
  # normally used to measure casks capacity
  litresOfPureAlcohol: str = "litresOfPureAlcohol"
  # normally used to measure tanks capacity
  hectolitres: str = "hectolitres"

class DealLegAvailabilityStatus(str, enum.Enum):
  # Indicates that the deal has been made between Fero and the client but the
  # stock backed by this deal is not yet in Fero's "possession" and therefore,
  # no repurchases or production orders can be made.
  awaitingProcurement: str = "awaitingProcurement"
  # The stock backed by the deal leg still has units available and is ready
  # to be repurchased or used in a production order.
  available: str = "available"
  # The stock backed by the deal leg is completely depleted (by deal leg sales).
  depleted: str = "depleted"

type DealLegId = common_db.DbKey["DealLeg"]

class DeliveryOption_InWarehouseTransfer(pydantic.BaseModel):
  inWarehouseTransfer: "InWarehouseTransfer"

class DeliveryOption_InWarehouseTransferWithAccountCode(pydantic.BaseModel):
  inWarehouseTransferWithAccountCode: "InWarehouseTransferWithAccountCode"

class DeliveryOption_OrgLocationDelivery(pydantic.BaseModel):
  orgLocationDelivery: "ShippingDelivery"

class DeliveryOption_ShippedDelivery(pydantic.BaseModel):
  shippedDelivery: "ShippingDelivery"

class DeliveryOption_UnknownDelivery(pydantic.BaseModel):
  unknownDelivery: "UnknownDelivery"

class DeliveryOption_ProductionOrderRunDelivery(pydantic.BaseModel):
  productionOrderRunDelivery: "ProductionOrderRunDelivery"

class DeliveryOption_ProductionOrderLossDelivery(pydantic.BaseModel):
  productionOrderLossDelivery: "ProductionOrderLossDelivery"

class DeliveryOption_NewDealRequestLossDelivery(pydantic.BaseModel):
  newDealRequestLossDelivery: "NewDealRequestLossDelivery"

class DeliveryOption_NewDealRequestDelivery(pydantic.BaseModel):
  newDealRequestDelivery: "NewDealRequestDelivery"

class DeliveryOption_Expiry(pydantic.BaseModel):
  expiry: "Expiry"

class DeliveryOption_Revaluation(pydantic.BaseModel):
  revaluation: "Revaluation"

class DeliveryOption_StockMovement(pydantic.BaseModel):
  stockMovement: "StockMovement"

class DeliveryOption_PurchaseRequest(pydantic.BaseModel):
  purchaseRequest: "PurchaseRequestInWarehouseTransfer"

class DeliveryOption_NewDealRequestVoid(pydantic.BaseModel):
  newDealRequestVoid: "NewDealRequestVoid"

class DeliveryOption(pydantic.RootModel[typing.Union[DeliveryOption_InWarehouseTransfer | DeliveryOption_InWarehouseTransferWithAccountCode | DeliveryOption_OrgLocationDelivery | DeliveryOption_ShippedDelivery | DeliveryOption_UnknownDelivery | DeliveryOption_ProductionOrderRunDelivery | DeliveryOption_ProductionOrderLossDelivery | DeliveryOption_NewDealRequestLossDelivery | DeliveryOption_NewDealRequestDelivery | DeliveryOption_Expiry | DeliveryOption_Revaluation | DeliveryOption_StockMovement | DeliveryOption_PurchaseRequest | DeliveryOption_NewDealRequestVoid]]):
  pass

class NewDealRequestVoid(pydantic.BaseModel):
  newDealRequestId: "NewDealRequestId"

class PurchaseRequestInWarehouseTransfer(pydantic.BaseModel):
  purchaseRequestNumber: common_strings.StringNE

class NominatedPurchaserCollection(pydantic.BaseModel):
  bonded: bool = pydantic.Field(default=False)

class StockMovement(pydantic.BaseModel):
  pass

class Expiry(pydantic.BaseModel):
  pass

class Revaluation(pydantic.BaseModel):
  pass

class NewDealRequestDelivery(pydantic.BaseModel):
  pass

class NewDealRequestLossDelivery(pydantic.BaseModel):
  pass

class ProductionOrderLossDelivery(pydantic.BaseModel):
  pass

class ProductionOrderRunDelivery(pydantic.BaseModel):
  pass

class InWarehouseTransfer(pydantic.BaseModel):
  pass

class InWarehouseTransferWithAccountCode(pydantic.BaseModel):
  accountCode: common_strings.StringNE
  siteCode: typing.Union[common_strings.StringNE, None]

class UnknownDelivery(pydantic.BaseModel):
  pass

class ShippingDelivery(pydantic.BaseModel):
  deliveryPointName: str
  streetName: str
  addressLine1: str
  addressLine2: str
  town: str
  postCode: str
  country: typing.Union[common.Country, None] = pydantic.Field(default=None)
  deliveryContact: typing.Union[str, None] = pydantic.Field(default=None)
  deliveryContactEmail: typing.Union[str, None] = pydantic.Field(default=None)
  deliveryContactNumber: typing.Union[str, None] = pydantic.Field(default=None)
  deliveryInstructions: typing.Union[str, None] = pydantic.Field(default=None)
  # Bonded indicates whether this location that the products are being
  # shipped to is a bonded warehouse. Products that are sold and send to a
  # bonded warehouse are treated as though they've been exported and therefore
  # duty and VAT are not payable.
  bonded: bool = pydantic.Field(default=False)
  # Optional delivery date and delivery time window.
  deliveryDate: typing.Union[common.LocalDate, None] = pydantic.Field(default=None)
  deliveryTimeEarliest: typing.Union[common.LocalTime, None] = pydantic.Field(default=None)
  deliveryTimeLatest: typing.Union[common.LocalTime, None] = pydantic.Field(default=None)
  deliveryIncoterms: typing.Union["Incoterms", None] = pydantic.Field(default=None)

class OrgLocationDelivery(pydantic.BaseModel):
  organisationDeliveryLocationId: "OrganisationDeliveryLocationId"
  deliveryInstructions: typing.Union[str, None]

# Represents an order to sell stock
class SaleOrder(pydantic.BaseModel):
  invoiceNumber: common_strings.StringNE
  organisationId: "OrganisationId"
  soldAt: common.Instant
  currency: "Currency"
  netSubtotal: common.BigDecimal
  fulfilmentFee: typing.Union[common.BigDecimal, None]
  duty: typing.Union[common.BigDecimal, None]
  vat: typing.Union[common.BigDecimal, None]
  deliveryFees: typing.Union["SaleOrderDeliveryFees", None]
  orderStatusEvents: list["SaleOrderStatusEvent"]
  releaseReminderSentTime: typing.Union[common.Instant, None]
  deliveryStatuses: list["DeliveryStatus"]
  deliveryOption: "DeliveryOption"
  # If true, indicates that the invoice linked to this sale order has been paid.
  invoicePaid: bool
  # If true, indicates that this sale order has been "paid for" using the
  # organisation's available settlement credit and as such should be counted
  # AGAINST their available credit balance until the invoice has been paid
  usesSettlementCredit: bool
  dueDate: common.Instant
  settlementCreditUsageFee: typing.Union[common.BigDecimal, None]
  deliveryOrderEnvelopeId: typing.Union[common_strings.StringNE, None]
  documents: "SaleOrderDocuments"

class SaleOrderDocuments(pydantic.BaseModel):
  deliveryOrder: typing.Union["StoredFileId", None]
  salesInvoiceId: typing.Union["InvoiceId", None] = pydantic.Field(default=None)

class SaleOrderDeliveryFees(pydantic.BaseModel):
  totalDeliveryFee: common.BigDecimal
  vat: common.BigDecimal

class SaleOrderStatusEvent(pydantic.BaseModel):
  createdAt: common.Instant
  status: "SaleOrderStatus"

class SaleOrderStatus(str, enum.Enum):
  deliveryOrderRequested: str = "deliveryOrderRequested"
  deliveryOrderSigned: str = "deliveryOrderSigned"
  draftInvoiced: str = "draftInvoiced"
  invoiced: str = "invoiced"
  paid: str = "paid"
  released: str = "released"
  canceled: str = "canceled"

class DeliveryStatus(pydantic.BaseModel):
  status: typing.Union["DeliveryState", None]
  bookedDate: typing.Union[common.LocalDate, None]
  originalEtaWindowFrom: typing.Union[common.LocalTime, None]
  originalEtaWindowTo: typing.Union[common.LocalTime, None]
  revisedEtaWindowFrom: typing.Union[common.LocalTime, None]
  revisedEtaWindowTo: typing.Union[common.LocalTime, None]
  confirmedDepartedAt: typing.Union[common.LocalDateTime, None]
  confirmedDeliveredAt: typing.Union[common.LocalDateTime, None]

class DeliveryState(str, enum.Enum):
  delivered: str = "delivered"
  outForDelivery: str = "outForDelivery"
  deliveryPlanning: str = "deliveryPlanning"
  deliveryPlanningHeld: str = "deliveryPlanningHeld"

type SaleOrderId = common_db.DbKey["SaleOrder"]

# Represents a bill to purchase stock
class PurchaseBill(pydantic.BaseModel):
  invoiceNumber: common_strings.StringNE
  organisationId: "OrganisationId"
  billDate: common.LocalDate
  purchasedItems: "PurchasedItems"

type PurchaseBillId = common_db.DbKey["PurchaseBill"]

class PurchasedItems_DealLegIds(pydantic.BaseModel):
  dealLegIds: list["DealLegId"]

class PurchasedItems_NewDealRequestDeposits(pydantic.BaseModel):
  newDealRequestDeposits: list["ProductLineItemId"]

class PurchasedItems_CreditForFreeStock(pydantic.BaseModel):
  creditForFreeStock: list["ProductLineItemId"]

class PurchasedItems(pydantic.RootModel[typing.Union[PurchasedItems_DealLegIds | PurchasedItems_NewDealRequestDeposits | PurchasedItems_CreditForFreeStock]]):
  pass

class ProductionOrder(pydantic.BaseModel):
  orderNumber: common_strings.StringNE
  createdAt: common.Instant
  # Timestamp representing when the production order was cancelled by the /
  # organisation that created it. This effectively deletes the production order
  # and can only be set if the storage location has no yet accepted / rejected
  # it, i.e. latestStatus is null or pending
  cancelledAt: typing.Union[common.Instant, None] = pydantic.Field(default=None)
  requestedCompletionDate: typing.Union[common.LocalDate, None]
  comments: typing.Union[str, None]
  latestStatusCreatedAt: typing.Union[common.LocalDate, None]
  latestStatus: typing.Union["ProductionOrderStatus", None]
  rejectionReason: typing.Union[str, None]
  orderStatusEvents: list["ProductionOrderStatusEvent"]
  organisationId: "OrganisationId"
  storageLocationId: "StorageLocationId"
  storageLocationBillingBasis: "BillingBasis"
  internalIdentifier: int = pydantic.Field(ge=0, le=18446744073709551615)
  lossInvoiceId: typing.Union["InvoiceId", None]

class ProductionOrderStatusEvent(pydantic.BaseModel):
  createdAt: common.LocalDate
  status: "ProductionOrderStatus"

class ProductionOrderStatus(str, enum.Enum):
  pending: str = "pending"
  accepted: str = "accepted"
  rejected: str = "rejected"
  completed: str = "completed"

type ProductionOrderId = common_db.DbKey["ProductionOrder"]

class ProductionOrderTaskSourceProduct(pydantic.BaseModel):
  productionOrderTaskId: "ProductionOrderTaskId"
  productId: "ProductId"
  # The number of source units that we've allocated to this production order
  # task. Once this unit is exhausted, the task is complete. If the value is
  # null, any number of source used can be used to meet the target quantity for
  # the task
  budgetQuantity: typing.Union["NumberOfUnits", None]

type ProductionOrderTaskSourceProductId = common_db.DbKey["ProductionOrderTaskSourceProduct"]

class ProductionOrderTask(pydantic.BaseModel):
  finishingProductId: "ProductId"
  finishingServiceId: "FinishingServiceId"
  productionOrderId: "ProductionOrderId"
  # Represents the number of units of the finishing product that should be
  # produced by this task. Note that if a budget is defined in the
  # ProductionOrderTaskSourceProduct the task can be completed before this
  # target is hit
  targetQuantity: "NumberOfUnits"
  pricePerUnit: common.BigDecimal
  setupCost: common.BigDecimal

type ProductionOrderTaskId = common_db.DbKey["ProductionOrderTask"]

class DealLegSale(pydantic.BaseModel):
  saleOrderId: "SaleOrderId"
  dealLegId: "DealLegId"
  unitsSold: "NumberOfUnits"
  forwardSalePrice: common.BigDecimal

type DealLegSaleId = common_db.DbKey["DealLegSale"]

class OrganisationDeliveryLocation(pydantic.BaseModel):
  organisationId: "OrganisationId"
  deliveryPointName: common_strings.StringNE
  streetName: common_strings.StringNE
  addressLine1: typing.Union[str, None]
  addressLine2: typing.Union[str, None]
  town: common_strings.StringNE
  postCode: common_strings.StringNE
  country: common.Country
  deliveryContact: typing.Union[str, None] = pydantic.Field(default=None)
  deliveryContactEmail: typing.Union[str, None] = pydantic.Field(default=None)
  deliveryContactNumber: typing.Union[str, None] = pydantic.Field(default=None)
  deliveryInstructions: typing.Union[str, None] = pydantic.Field(default=None)
  bonded: bool = pydantic.Field(default=False)

type OrganisationDeliveryLocationId = common_db.DbKey["OrganisationDeliveryLocation"]

class CapitalisedExpenseSpreadType(str, enum.Enum):
  # Based on the portion of the quantites in each deal leg against the total
  # quantity
  quantity: str = "quantity"
  # Based on the portion of the value of each deal leg against the total
  # value
  value: str = "value"

# Defines how to allocate CapEx
class CapitalisedExpenseApplication_DealLeg(pydantic.BaseModel):
  dealLeg: "DealLegId"

class CapitalisedExpenseApplication_Deal(pydantic.BaseModel):
  deal: "DealId"

class CapitalisedExpenseApplication_Organisation(pydantic.BaseModel):
  organisation: "OrganisationId"

class CapitalisedExpenseApplication_StorageLocation(pydantic.BaseModel):
  storageLocation: "StorageLocationId"

class CapitalisedExpenseApplication(pydantic.RootModel[typing.Union[CapitalisedExpenseApplication_DealLeg | CapitalisedExpenseApplication_Deal | CapitalisedExpenseApplication_Organisation | CapitalisedExpenseApplication_StorageLocation]]):
  pass

# Wrapper object around a a number of CapitalisedExpense objects that
# contains the metadata for the original expense incurred
class CapitalisedExpenseDescription(pydantic.BaseModel):
  currency: "Currency"
  amountPaid: common.BigDecimal
  expenseDate: common.LocalDate
  createdAt: common.Instant
  spreadType: "CapitalisedExpenseSpreadType"
  application: "CapitalisedExpenseApplication"
  # Company to which the expense was paid.
  vendor: str
  # Storage, Insurance, Disgorgement, Labelling, Labelling & Packaging, Legal
  # Fees
  expenseType: common_strings.StringNE
  # End customer's reference for the expense, e.g. invoice number
  referenceNumber: typing.Union[common_strings.StringNE, None]
  # Whether the "expense" amount actually acts as an investment in the goods
  # and increases their value as an asset.
  valueAccretive: bool
  # Indicates that this Capex was attempted to be migrated however the
  # migration could not be completed because of some date inconsistency.
  # Objects with this flag set to true should be addressed on a case by case
  # basis when the migration is complete
  # TODO(Barry): Remove this field when all of the issues have been addressed
  needsRectification: bool

type CapitalisedExpenseDescriptionId = common_db.DbKey["CapitalisedExpenseDescription"]

# Represents an expense that should be tracked against a deal leg when
# Forward Sale Price is calculated 
class CapitalisedExpense(pydantic.BaseModel):
  dealLegId: "DealLegId"
  descriptionId: "CapitalisedExpenseDescriptionId"
  # A reference to the production order run that was used to generate this expense.
  # Indicates that this expense covers the per unit cost + the portion of the
  # setup cost of that run
  productionOrderTaskRunId: typing.Union["ProductionOrderTaskRunId", None]
  amountPaid: common.BigDecimal
  expenseDate: common.LocalDate
  createdAt: common.Instant

type CapitalisedExpenseId = common_db.DbKey["CapitalisedExpense"]

class PhysicalStock(pydantic.BaseModel):
  orgId: "OrganisationId"
  productId: "ProductId"
  locationId: "StorageLocationId"
  quantityUnit: "QuantityUnit"
  rotationNumber: str
  rotationLine: str

type PhysicalStockId = common_db.DbKey["PhysicalStock"]

class PhysicalStockAdjustmentReason(str, enum.Enum):
  stock_in: str = "stock_in"
  stock_out: str = "stock_out"
  stock_loss: str = "stock_loss"
  transform: str = "transform"
  transit: str = "transit"
  reverse_stock_in: str = "reverse_stock_in"
  reverse_stock_out: str = "reverse_stock_out"
  manual: str = "manual"

class PhysicalStockAdjustment(pydantic.BaseModel):
  stockId: "PhysicalStockId"
  # The quantity to adjust the stock by. Positive values increase the stock,
  # negative values decrease the stock.
  quantity: common.BigDecimal
  # Whether the stock is in storage and available for sale and movement
  available: bool
  # Reason for the adjustment, for traceability purposes
  reason: "PhysicalStockAdjustmentReason"
  # The process (e.g. NDR, Trade Sale, Production Order) that caused this
  processId: typing.Union[str, None]
  # Used for incremental data migration from DealLegs / DealLegSales
  # To prevent migration duplication
  financialModelRef: str
  # In the case of a transit or transform, the source stock
  createdAt: common.Instant

type PhysicalStockAdjustmentId = common_db.DbKey["PhysicalStockAdjustment"]

class PhysicalStockLinkType(str, enum.Enum):
  one_to_one: str = "one_to_one"
  one_to_many: str = "one_to_many"
  many_to_one: str = "many_to_one"

class PhysicalStockLink(pydantic.BaseModel):
  sourceStockId: "PhysicalStockAdjustmentId"
  resultStockId: "PhysicalStockAdjustmentId"
  linkType: "PhysicalStockLinkType"
  reason: "PhysicalStockAdjustmentReason"

class OAuthToken(pydantic.BaseModel):
  accessToken: str
  tokenType: str
  refreshToken: str
  expiresAt: common.Instant
  scope: typing.Union[str, None] = pydantic.Field(default=None)
  creator: typing.Union["AppUserId", None] = pydantic.Field(default=None)

class XeroConnection(pydantic.BaseModel):
  # Tenant name
  tenantName: str
  # Only one XeroConnection to be enabled
  enabled: bool = pydantic.Field(default=False)
  # Enable to create real invoices?
  enableLiveInvoices: bool = pydantic.Field(default=False)
  # Enable invoice emailing via xero?
  enableInvoiceEmail: bool = pydantic.Field(default=False)

class ConnectionConfig_Lcb(pydantic.BaseModel):
  lcb: "LcbConnectionConfig"

class ConnectionConfig_TrueLayer(pydantic.BaseModel):
  trueLayer: "TrueLayerConnectionConfig"

class ConnectionConfig_Xero(pydantic.BaseModel):
  xero: "XeroConnectionConfig"

class ConnectionConfig_DocuSign(pydantic.BaseModel):
  docuSign: "DocuSignConfig"

class ConnectionConfig(pydantic.RootModel[typing.Union[ConnectionConfig_Lcb | ConnectionConfig_TrueLayer | ConnectionConfig_Xero | ConnectionConfig_DocuSign]]):
  pass

class DocuSignSigningGroup(pydantic.BaseModel):
  name: common_strings.StringNE
  groupId: common_strings.StringNE

class DocuSignConfig(pydantic.BaseModel):
  deliveryOrderSigningGroup: typing.Union["DocuSignSigningGroup", None]

class LcbConnectionConfig(pydantic.BaseModel):
  emailExternalRecipients: bool = pydantic.Field(default=False)

class ApiDeckConnectionConfig(pydantic.BaseModel):
  consumerId: str
  serviceId: str

class ShipEngineConnectionConfig(pydantic.BaseModel):
  carrierId: str

class TrueLayerProvider(str, enum.Enum):
  BarclaysBusiness: str = "BarclaysBusiness"
  NatWest: str = "NatWest"
  Revolut: str = "Revolut"
  RoyalBankOfScotland: str = "RoyalBankOfScotland"

class TrueLayerConnectionConfig(pydantic.BaseModel):
  provider: "TrueLayerProvider"

class VisionConnectionConfig(pydantic.BaseModel):
  emailExternalRecipients: bool = pydantic.Field(default=False)

class XeroConnectionConfig(pydantic.BaseModel):
  id: str
  tenantId: str
  tenantType: str
  tenantName: str
  # flag to create Xero invoices
  enableLiveInvoices: bool = pydantic.Field(default=False)
  # flag to email Xero invoices
  enableInvoiceEmail: bool = pydantic.Field(default=False)

class ApiApp(str, enum.Enum):
  TrueLayer: str = "TrueLayer"
  Vision: str = "Vision"
  Xero: str = "Xero"
  Any: str = "Any"

class ApiConnectionConfig_ApiDeck(pydantic.BaseModel):
  apiDeck: "ApiDeckConnectionConfig"

class ApiConnectionConfig_FreightTransport(pydantic.BaseModel):
  freightTransport: None

class ApiConnectionConfig_ShipEngine(pydantic.BaseModel):
  shipEngine: "ShipEngineConnectionConfig"

class ApiConnectionConfig_DocuSign(pydantic.BaseModel):
  docuSign: None

class ApiConnectionConfig(pydantic.RootModel[typing.Union[ApiConnectionConfig_ApiDeck | ApiConnectionConfig_FreightTransport | ApiConnectionConfig_ShipEngine | ApiConnectionConfig_DocuSign]]):
  pass

type ApiName = str

type ApiConfigName = str

# API connection to third-party application
class ApiConnection(pydantic.BaseModel):
  app: "ApiApp"
  name: "ApiName"
  config: "ConnectionConfig"
  enabled: bool
  oAuthTokenId: typing.Union[common_db.DbKey["OAuthToken"], None] = pydantic.Field(default=None)
  configName: typing.Union["ApiConfigName", None] = pydantic.Field(default=None)
  connectionConfig: typing.Union["ApiConnectionConfig", None] = pydantic.Field(default=None)

class FinishingService(pydantic.BaseModel):
  organisationId: "OrganisationId"
  storageLocationId: "StorageLocationId"
  code: common_strings.StringNE
  name: common_strings.StringNE
  defaultPricePerUnit: common.BigDecimal
  defaultSetupCost: common.BigDecimal

type FinishingServiceId = common_db.DbKey["FinishingService"]

class FinishingProductMapping(pydantic.BaseModel):
  sourceProductId: "ProductId"
  finishingProductId: "ProductId"
  finishingServiceId: "FinishingServiceId"

type FinishingProductMappingId = common_db.DbKey["FinishingProductMapping"]

class ProductionOrderTaskRun(pydantic.BaseModel):
  productionOrderTaskId: "ProductionOrderTaskId"
  sourceSaleOrderId: "SaleOrderId"
  lossesSaleOrderId: typing.Union["SaleOrderId", None]
  completedAt: common.Instant
  sourceProductItems: list["SourceProductItem"]

class SourceProductItem(pydantic.BaseModel):
  productId: "ProductId"
  quantity: "NumberOfUnits"
  # This reflects the number of units lost during production. It will be the same number 
  # as the one in the losses sale order.
  lossQuantity: "NumberOfUnits"

type ProductionOrderTaskRunId = common_db.DbKey["ProductionOrderTaskRun"]

class Supplier(pydantic.BaseModel):
  name: common_strings.StringNE
  contactName: common_strings.StringNE
  contactMobile: common_strings.StringNE
  contactEmail: common_strings.StringNE
  addressLine1: typing.Union[str, None]
  addressLine2: typing.Union[str, None]
  town: common_strings.StringNE
  postCode: common_strings.StringNE
  country: common.Country
  locale: "Locale"
  # Indicates if this supplier is pending approval from the ferovinum team
  status: "ApprovalStatus"
  updatedAt: common.Instant
  approvedCurrencies: list["Currency"]

type SupplierId = common_db.DbKey["Supplier"]

class Carrier(pydantic.BaseModel):
  name: common_strings.StringNE
  contactName: common_strings.StringNE
  contactMobile: common_strings.StringNE
  contactEmail: common_strings.StringNE

type CarrierId = common_db.DbKey["Carrier"]

class NewDealRequestState(str, enum.Enum):
  # initial new deal state
  newDeal: str = "newDeal"
  # Ferovinum HQ has accepted the deal
  feroAccepted: str = "feroAccepted"
  # Ferovinum HQ has rejected the deal
  feroRejected: str = "feroRejected"
  # Client organisation has accepted the deal
  orgAccepted: str = "orgAccepted"
  # Client organisation has rejected the deal
  orgRejected: str = "orgRejected"
  # Delivery order has been generated
  doGenerated: str = "doGenerated"
  # Delivery order has been signed
  doSigned: str = "doSigned"
  # Ferovinum HQ has voided the deal
  feroVoided: str = "feroVoided"
  # Client has cancelled the request
  cancelled: str = "cancelled"
  # The invoice for the deposit of this deal has been sent to the client organisation
  depositInvoiced: str = "depositInvoiced"
  # The deposit invoice has been marked as paid by the client organisation
  depositPaid: str = "depositPaid"
  # The deposit payment has been acknowledged as received by Ferovinum HQ
  depositPaymentReceived: str = "depositPaymentReceived"
  # A purchase order has been created by Ferovinum HQ and sent to the supplier
  poCreated: str = "poCreated"
  # A purchase order has been accepted by the supplier
  poAccepted: str = "poAccepted"
  # Carrier has been made aware of a collection request (this step is triggered immediately after a po has been accepted)
  # carrier collection has been requested and is awaiting carrier to confirm collection date
  collectionRequested: str = "collectionRequested"
  # Carrier has accepted a collection order and supplied an ETA
  collectionAccepted: str = "collectionAccepted"
  # Carrier has collected the po from the supplier
  collectionConfirmed: str = "collectionConfirmed"
  # Carrier has delivered the po to the storage location
  carrierDeliveryConfirmed: str = "carrierDeliveryConfirmed"
  # Storage location has acknowledged receiving the delivery and has assigned rotation numbers to it
  warehouseDeliveryConfirmed: str = "warehouseDeliveryConfirmed"
  # Stock ownership transfer is complete. Part of existing stock procurement
  stockTransferred: str = "stockTransferred"
  # Fero has paid the client. Part of existing stock procurement
  paymentTransferred: str = "paymentTransferred"

class NewDealRequestStateEvent(pydantic.BaseModel):
  time: common.Instant
  state: "NewDealRequestState"

class NewDealRequestType(str, enum.Enum):
  # deals created through the procurement flow
  newStock: str = "newStock"
  # deals created previous to the procurement flow
  existingStock: str = "existingStock"

# represents a deal request submitted by a client organisation
class NewDealRequest(pydantic.BaseModel):
  dealNumber: common_strings.StringNE
  # organisation that has requested this new deal request
  organisationId: "OrganisationId"
  # supplier of products in this new deal request
  supplierId: typing.Union["SupplierId", None]
  # carrier of products from the producer to the storage location
  carrierId: typing.Union["CarrierId", None]
  # deal this new order is attached to or null if it hasn't been accepted yet
  dealId: typing.Union["DealId", None]
  # storage location where this order will be delivered to
  storageLocationId: "StorageLocationId"
  # type of deal request based on stock existence
  dealRequestType: "NewDealRequestType"
  # current state of this new deal request
  state: "NewDealRequestState"
  # Timestamp on when the last state was set
  lastStateSet: common.Instant
  # time series of this new deal request states (oldest -> newest)
  newDealRequestEvents: list["NewDealRequestStateEvent"]
  # earliest collection date informed by the supplier
  supplierEarliestCollectionDate: typing.Union[common.LocalDate, None]
  # Earliest collection date informed by the carrier
  carrierEarliestCollectionDate: typing.Union[common.LocalDate, None]
  # delivery ETA date informed by the carrier
  carrierDeliveryEta: typing.Union[common.LocalDate, None]
  # supplier fees have been paid (null if it hasn't been paid yet)
  supplierIsPaid: typing.Union[common.Instant, None]
  # carrier fees have been paid (null if it hasn't been paid yet)
  carrierIsPaid: typing.Union[common.Instant, None]
  # deal terms attached to this new deal request
  dealTermsId: typing.Union["DealTermsId", None]
  # additional terms set by ferovinum in this deal
  additionalTermsText: common_strings.StringMD
  # timestamp of when the collection reminder was sent to the supplier and
  # carrier, only present if the reminder was actually sent
  collectionReminderSentAt: typing.Union[common.Instant, None]
  # creation timestamp for this deal request from a client organisation
  createdAt: common.Instant
  # loss sale order id for if storage loc receives less products than expected
  lossSaleOrderId: typing.Union[common_db.DbKey["SaleOrder"], None]
  # was new deal request delivered with more products than expected
  hasExtraStockDelivered: bool
  # Locale used for this new deal request clock related events
  locale: "Locale"
  # Map of timestamps by state of nudge notifications sent to ferovinum admins
  feroNudgeNotificationsTimestamps: list[sys_types.MapEntry["NewDealRequestState", list[common.Instant]]]
  # Purchase currency used for this new deal request
  purchaseCurrency: "Currency"
  # The settlement currency that should be used for the created deal
  settlementCurrency: "Currency"
  # Deposit invoice number (tracked in xero, only applicable to new stock)
  depositInvoiceNumber: typing.Union[common_strings.StringNE, None]
  # Incoterm for new stock
  incoterms: typing.Union["Incoterms", None]
  # Total discount percentage applicable for the new deal request
  totalDiscountPct: typing.Union[ferovinum_app_types.Decimal, None]
  # Purchase order number sent to supplier (tracked in xero, only applicable
  # to new stock procurement)
  purchaseOrderNumber: typing.Union[str, None]
  # Purchase bill number (tracked in xero, applicable to both existing and new stock)
  # this purchase bill in xero represents ferovinum holding of the stock (both paid and free stock)
  purchaseBillNumber: typing.Union[str, None]
  # Stores the reason the fero operator rejected this order.
  rejectionReason: typing.Union[common_strings.StringMD, None]
  foreignCurrencySupplierPayment: typing.Union["ForeignCurrencySupplierPaymentValues", None]
  contractNoteId: typing.Union["ContractNoteId", None]
  poFileId: typing.Union["StoredFileId", None]
  depositInvoiceId: typing.Union["InvoiceId", None]
  deliverySaleOrderId: typing.Union["SaleOrderId", None] = pydantic.Field(default=None)
  deliveryLossInvoiceId: typing.Union["InvoiceId", None] = pydantic.Field(default=None)
  voidNewDealInvoiceId: typing.Union["InvoiceId", None] = pydantic.Field(default=None)
  dutyPaid: bool
  # Delivery order envelope id
  deliveryOrderEnvelopeId: typing.Union[common_strings.StringNE, None]
  # Delivery order file id
  doFileId: typing.Union["StoredFileId", None]

type NewDealRequestId = common_db.DbKey["NewDealRequest"]

class ProductLineItem(pydantic.BaseModel):
  productId: "ProductId"
  newDealRequestId: "NewDealRequestId"
  # Decilitres for casks
  # total number units that were purchased by the organisation (includes free stock units)
  totalNumberOfUnits: "NumberOfUnits"
  # number of units that were given to the organisation as free stock
  numberOfFreeUnits: "NumberOfUnits"
  # individual discount percentage per product line item
  individualDiscountPct: typing.Union[ferovinum_app_types.Decimal, None]
  # Price per LPA for casks
  purchaseCurrencyPrice: ferovinum_app_types.Decimal
  clientSpecifiedPurchasePrice: ferovinum_app_types.Decimal
  provisionalPurchasePrice: typing.Union[ferovinum_app_types.Decimal, None]
  finalPurchasePrice: typing.Union[ferovinum_app_types.Decimal, None]
  unifiedPurchasePrice: typing.Union[ferovinum_app_types.Decimal, None]
  depositPrice: typing.Union[ferovinum_app_types.Decimal, None]
  throughputFeePc: typing.Union[ferovinum_app_types.Decimal, None]
  compulsorySaleDate: typing.Union[common.LocalDate, None]
  # CSD proportion
  compulsorySalePc: typing.Union[ferovinum_app_types.Decimal, None]
  finalDate: typing.Union[common.LocalDate, None]
  # Monthly fee specification
  monthlyFee: typing.Union["MonthlyFee", None]
  rotationNumber: typing.Union[common_strings.StringNE, None]
  rotationLine: typing.Union[int, None]
  numberOfUnitsDelivered: typing.Union["NumberOfUnits", None]
  # Mandatory for LCB locations and optional for others
  singlesPerCase: typing.Union[int, None]

class ForeignCurrencySupplierPaymentValues(pydantic.BaseModel):
  dealCurrencyTotalAmountPaid: ferovinum_app_types.Decimal
  fxServiceChargePct: ferovinum_app_types.Decimal

type ProductLineItemId = common_db.DbKey["ProductLineItem"]

class DealTerms(pydantic.BaseModel):
  general: "Term"
  fsp: "Term"
  createdAt: common.Instant

class Term(pydantic.BaseModel):
  header: str
  content: common_strings.StringMD

type DealTermsId = common_db.DbKey["DealTerms"]

class NewSupplier(pydantic.BaseModel):
  name: common_strings.StringNE
  contactName: common_strings.StringNE
  contactMobile: common_strings.StringNE
  contactEmail: common_strings.StringNE
  addressLine1: typing.Union[str, None]
  addressLine2: typing.Union[str, None]
  town: common_strings.StringNE
  postCode: common_strings.StringNE
  country: common.Country
  approvedCurrencies: list["Currency"]

class NewSupplierDetails(pydantic.BaseModel):
  newDealRequestId: "NewDealRequestId"
  supplier: "NewSupplier"

class TransactionMatch(str, enum.Enum):
  exact: str = "exact"
  partial: str = "partial"

class Payment(pydantic.BaseModel):
  transactionId: str
  timestamp: common.Instant
  amount: common.BigDecimal
  currency: str
  transactionMatch: "TransactionMatch"

type PaymentId = common_db.DbKey["Payment"]

class SaleOrderPayment(pydantic.BaseModel):
  saleOrderId: "SaleOrderId"
  paymentId: "PaymentId"
  timestamp: common.Instant

class CounterpartyRef_OrganisationId(pydantic.BaseModel):
  organisationId: "OrganisationId"

class CounterpartyRef_PurchaserId(pydantic.BaseModel):
  purchaserId: "PurchaserId"

class CounterpartyRef(pydantic.RootModel[typing.Union[CounterpartyRef_OrganisationId | CounterpartyRef_PurchaserId]]):
  pass

class CounterpartyPaymentInfo(pydantic.BaseModel):
  counterpartyRef: "CounterpartyRef"
  paymentDescriptionPrefix: common_strings.StringNE

type CounterpartyPaymentInfoId = common_db.DbKey["CounterpartyPaymentInfo"]

class BankTransactionType(str, enum.Enum):
  CREDIT: str = "CREDIT"
  DEBIT: str = "DEBIT"

class BankTransactionStatus(str, enum.Enum):
  Unmatched: str = "Unmatched"
  Matched: str = "Matched"
  Processed: str = "Processed"

# Transactions from our bank accounts
class BankTransaction(pydantic.BaseModel):
  description: common_strings.StringNE
  extTimestamp: common.Instant
  transactionType: "BankTransactionType"
  amount: common.BigDecimal
  currency: "Currency"
  transactionId: common_strings.StringNE
  accountId: common_strings.StringNE
  status: "BankTransactionStatus"
  createdAt: common.Instant

type BankTransactionId = common_db.DbKey["BankTransaction"]

class InvoiceOriginatorRef_SaleOrderId(pydantic.BaseModel):
  saleOrderId: "SaleOrderId"

class InvoiceOriginatorRef_PurchaseRequestId(pydantic.BaseModel):
  purchaseRequestId: "PurchaseRequestId"

class InvoiceOriginatorRef_NewDealRequestId(pydantic.BaseModel):
  newDealRequestId: "NewDealRequestId"

class InvoiceOriginatorRef_NegativeStockAdjustmentId(pydantic.BaseModel):
  negativeStockAdjustmentId: "NegativeStockAdjustmentId"

class InvoiceOriginatorRef(pydantic.RootModel[typing.Union[InvoiceOriginatorRef_SaleOrderId | InvoiceOriginatorRef_PurchaseRequestId | InvoiceOriginatorRef_NewDealRequestId | InvoiceOriginatorRef_NegativeStockAdjustmentId]]):
  pass

class PaymentMatched(pydantic.BaseModel):
  bankTxId: "BankTransactionId"
  timestamp: common.LocalDateTime
  transactionMatch: "TransactionMatch"

class PaymentMatchState_NotMatched(pydantic.BaseModel):
  notMatched: None

class PaymentMatchState_Processed(pydantic.BaseModel):
  processed: None

class PaymentMatchState_Matched(pydantic.BaseModel):
  matched: "PaymentMatched"

class PaymentMatchState_MatchedFailedToProcess(pydantic.BaseModel):
  matchedFailedToProcess: "PaymentMatched"

class PaymentMatchState(pydantic.RootModel[typing.Union[PaymentMatchState_NotMatched | PaymentMatchState_Processed | PaymentMatchState_Matched | PaymentMatchState_MatchedFailedToProcess]]):
  pass

# For matching payments against outstanding invoices.
class InvoicePayment(pydantic.BaseModel):
  invoiceNumber: common_strings.StringNE
  originatorRef: "InvoiceOriginatorRef"
  currency: "Currency"
  amountDue: common.BigDecimal
  invoicedAt: common.Instant
  counterpartyPaymentInfoId: typing.Union["CounterpartyPaymentInfoId", None]
  paymentMatchState: "PaymentMatchState"

class QrCode(pydantic.BaseModel):
  hash: str
  image: str

type QrCodeId = common_db.DbKey["QrCode"]

class DealLegQrCode(pydantic.BaseModel):
  dealLegId: "DealLegId"
  qrCodeId: "QrCodeId"

class DealRollApplication(str, enum.Enum):
  expiry: str = "expiry"
  stockMovement: str = "stockMovement"
  productionRun: str = "productionRun"
  newStockDelivery: str = "newStockDelivery"
  revaluation: str = "revaluation"

class DealRoll(pydantic.BaseModel):
  fromDealLeg: "DealLegId"
  toDealLeg: "DealLegId"
  application: "DealRollApplication"
  dealLegSaleId: "DealLegSaleId"
  createdAt: common.Instant

type DealRollId = common_db.DbKey["DealRoll"]

class Purchaser(pydantic.BaseModel):
  accountCode: str
  # The name of the purchaser
  name: str
  # The purchaser's currency
  currency: "Currency"
  crnNumber: typing.Union[common_strings.StringNE, None]
  vatNumber: typing.Union[common_strings.StringNE, None]
  awrsRegistrationNumber: typing.Union[common_strings.StringNE, None]
  addressLine1: typing.Union[str, None]
  addressLine2: typing.Union[str, None]
  addressLine3: typing.Union[str, None]
  town: typing.Union[str, None]
  postCode: typing.Union[str, None]
  country: typing.Union[common.Country, None]
  accountsEmails: list[str]
  xeroContactId: typing.Union[str, None]
  featureFlags: typing.Union[list["PurchaserFeatureFlag"], None]
  # Note: this field is automatically populated by a DB trigger
  # Setting this field manually will have no effect
  updatedAt: common.Instant

type PurchaserId = common_db.DbKey["Purchaser"]

class PurchaserFeatureFlag(str, enum.Enum):
  allow_po_upload: str = "allow_po_upload"
  inplatform_invoices: str = "inplatform_invoices"
  disable_invoice_email_notification: str = "disable_invoice_email_notification"
  disable_invoice_reminder_email: str = "disable_invoice_reminder_email"

class SavedDeliveryInfo(pydantic.BaseModel):
  deliveryPointName: common_strings.StringNE
  address: "Address"
  contact: typing.Union[common.ContactPerson, None] = pydantic.Field(default=None)
  instruction: typing.Union[common_strings.StringNE, None] = pydantic.Field(default=None)
  bonded: bool = pydantic.Field(default=False)

class NominatedPurchaser(pydantic.BaseModel):
  # The ID of the organisation that the purchaser is linked to
  organisationId: "OrganisationId"
  # The ID of the purchaser being linked
  purchaserId: "PurchaserId"
  # A value for the amount of time that the purchaser has to complete payment
  # once stock is collected. 
  paymentTermsPeriod: "PaymentTermsPeriod"
  # The terms that should be used when creating new purchase requests
  currentPurchaseRequestTerms: "NominatedPurchaserTermsId"
  # Max percentage Fero is comfortable with advancing upon delivery.
  # The feature is disabled if maxAdvancePct is missing.
  maxAdvancePct: typing.Union[common.BigDecimal, None]
  # The value we will pre-populate the discount field with when creating a trade sale
  # We will add a flag later to determine if this is per item or per order
  savedDiscountPct: typing.Union[common.BigDecimal, None]
  savedDeliveryInfoList: list["SavedDeliveryInfo"]
  # Fixed fee applied to purchase requests (in %) - Overrides the values set on an organisation
  serviceFeePc: typing.Union[common.BigDecimal, None]
  # Flag to indicate trades for the purchaser can be auto-accepted
  # Requires the purchaser be trade sale authorised
  autoAcceptTradeSale: bool
  # This nominated purchaser has accepted the order authorisation terms
  # See NominatedPurchaserEmailAuthorisation
  tradeSaleAuthorised: bool

type NominatedPurchaserId = common_db.DbKey["NominatedPurchaser"]

class EmailAuthorisationStatus(str, enum.Enum):
  sent: str = "sent"
  accepted: str = "accepted"

# Represents an email authorisation request for a nominated purchaser
class NominatedPurchaserEmailAuthorisation(pydantic.BaseModel):
  nominatedPurchaserId: "NominatedPurchaserId"
  purchaserName: common_strings.StringNE
  organisationName: common_strings.StringNE
  status: "EmailAuthorisationStatus"
  updatedAt: common.Instant

class PurchaserUserPermission(str, enum.Enum):
  action_trade_sale_order_authorisation: str = "action_trade_sale_order_authorisation"

class AppUserPurchaser(pydantic.BaseModel):
  appUserId: "AppUserId"
  purchaserId: "PurchaserId"
  permissions: list["PurchaserUserPermission"]

class TradeSaleSettlementMethod(str, enum.Enum):
  nominatedPurchaserCredit: str = "nominatedPurchaserCredit"
  organisationCredit: str = "organisationCredit"

class PurchaseRequest(pydantic.BaseModel):
  # The ID of the object tracking the parties engaged in this purchase
  # request
  nominatedPurchaserId: "NominatedPurchaserId"
  # The ID of the storage locations where the products will be purchased from
  storageLocationId: "StorageLocationId"
  # A unique human-readable identifier for this agreement, it takes the form
  purchaseRequestNumber: str
  # The total that will be charged to the purchaser
  netSubtotal: common.BigDecimal
  dealDiscountAmount: typing.Union[common.BigDecimal, None]
  # The delivery fee that will be charged to the purcahser if they are paying
  # for delivery, which is defined by purchaserCoversDeliveryCost. Value is
  # null if there is no delivery charge for this purchase request. 
  deliveryFee: typing.Union["DeliveryFee", None]
  # The duty and VAT paid by the purchaser. Not present if duty and VAT do
  # not apply, for example if the sale is for export
  dutyAndVat: typing.Union["DutyAndVat", None]
  # The duty and VAT paid by the seller for free stock
  freeStockDutyAndVat: typing.Union["DutyAndVat", None]
  # The currency that the purchaser will pay in
  purchaserCurrency: "Currency"
  # The currency that the org selling the stock will be paid in 
  settlementCurrency: "Currency"
  # The terms that cover this purchase request
  nominatedPurchaserTermsId: "NominatedPurchaserTermsId"
  # The credit fee percentage to apply to this purchase request. Taken from
  # Organisation#defaultMonthlyFees and stored here as the fee
  # percentage when the purchase request was created.
  organisationMonthlyFee: "MonthlyFee"
  # The state of this agreement
  state: "PurchaseRequestState"
  # A history of the state transitions for this agreement
  stateEvents: list["PurchaseRequestStateEvent"]
  # The ID of the sale order created when the purchaser collects the goods
  saleOrderId: typing.Union["SaleOrderId", None]
  # The number of days required by the org to prepare the stock for
  # collection.
  stockPreparationDays: int
  # The number of days that the purchaser has after the agreement is created
  # to accept the terms. If the agreement isn't accepted after this many
  # days it is considered expired and the purchaser can no longer accept it.
  expiryDays: int = pydantic.Field(default=15)
  # The number of days that the purchaser has to collect the goods
  collectionDays: int = pydantic.Field(default=30)
  # The amount of time that the purchaser has to complete payment after the
  # stock is collected. Should be based on the terms of contained in
  # nominatedPurchaserTermsId;
  paymentTermsPeriod: "PaymentTermsPeriod"
  # Details how the stock should be transferred to the nominated purchaser.
  purchaseRequestDeliveryOption: "PurchaseRequestDeliveryOption"
  # Present if a reminder has already been sent to a storage location regarding
  # the delivery of this purchase request. 
  deliveryReminderSent: typing.Union[common.Instant, None]
  # Defines whether delivery fees should be presented as a separate line item
  # to the purchaser
  purchaserCoversDeliveryCost: bool
  # The date at which the purchaser paid Ferovinum and credit fees are no
  # longer being charged to the organisation. 
  purchaserPaidDate: typing.Union[common.LocalDate, None]
  # The amount of fees that have been applied to surcharges, e.g. delivery,
  # vat, etc. in the period that Fero was waiting for payment from the
  # purchaser. Does not include additional monthly fees per product sold, 
  # which are tracked separately in the PurchaseRequestLineItem
  surchargeCreditTotal: typing.Union[common.BigDecimal, None]
  # Fixed fee % used to calculate the nominatedPurchaserFee.
  # This is set at the time of creation
  nominatedPurchaserFeePct: common.BigDecimal
  # The fixed fee (based on the subtotal) charged to the organisation for
  # this purchase request. The provisional fee is calculated when the purchase request
  # is created, using the subtotal based in the settlement currency with FX at the time.
  provisionalNominatedPurchaserFee: common.BigDecimal
  # The fixed fee (based on the subtotal) charged to the organisation for
  # this purchase request. The final fee is calculated when the purchase request
  # is marked as paid, using the subtotal based in the settlement currency with FX at the time.
  finalNominatedPurchaserFee: typing.Union[common.BigDecimal, None]
  # Only populated for purchase requests where some of the final products in this request
  # will be manufactured through a production order
  productionOrderId: typing.Union["ProductionOrderId", None]
  # Checks if customer agreed to use additional funding
  # False if not offered to the org, or not accepted by the org.
  additionalFundingRequested: bool = pydantic.Field(default=False)
  # The cash amount we have agreed to advance upon delivery.
  # Set to 0 if the funding advance or net receivables are negative
  cashAmountToAdvanceUponDelivery: typing.Union[common.BigDecimal, None]
  # Date when the advance to be paid upon delivery was actually paid to the org
  advanceUponDeliveryPaymentDate: typing.Union[common.LocalDate, None]
  # Relevant only if we agreed to advance cash upon delivery
  cashAdvanceUponDeliveryPaid: bool = pydantic.Field(default=False)
  # The amount that org is due when the purchaser pays
  orgNetReceivableAmount: typing.Union[common.BigDecimal, None]
  # The date when the net receivable (final payment) was transferred to the org
  orgNetReceivablePaymentDate: typing.Union[common.LocalDate, None]
  # The FX rate captured at the time the PurchaseRequest is created for 
  # deals where the settlement currency is different to the purchase currency
  provisionalFxRate: typing.Union[common.BigDecimal, None]
  # The FX rate captured at the time the PurchaseRequest is marked as organisation paid
  # for deals where the settlement currency is different to the purchase currency
  finalFxRate: typing.Union[common.BigDecimal, None]
  # The way the sale price for this purchase request was defined
  # E.g. if the sale price was defined inc duty, or ex duty
  salePriceType: "PurchaseRequestSalePriceType"
  # Delivery statuses from LCB fulfillment
  deliveryStatuses: list["DeliveryStatus"]
  shipmentNumber: typing.Union[str, None]
  customerPoRef: typing.Union[str, None]
  deliveryInstructionDocs: typing.Union[list["StoredFileId"], None]
  collectedDate: typing.Union[common.LocalDate, None]
  deliveredDate: typing.Union[common.LocalDate, None]
  purchaserInvoiceId: typing.Union["InvoiceId", None]
  poFileId: typing.Union["StoredFileId", None]
  purchaserProformaInvoiceId: typing.Union["InvoiceId", None]
  settlementMethod: "TradeSaleSettlementMethod"
  # Indicates if the order was created using storefront or trade sale screen
  storefrontOrder: bool = pydantic.Field(default=False)

type PurchaseRequestId = common_db.DbKey["PurchaseRequest"]

class PurchaseRequestSalePriceType(str, enum.Enum):
  incDutyExVat: str = "incDutyExVat"
  incDutyAndVat: str = "incDutyAndVat"
  exDutyAndVat: str = "exDutyAndVat"

# Defines when the deadline for the org/purchaser to pay Fero after
# stock collection
class PaymentTermsPeriod_DaysAfterCollection(pydantic.BaseModel):
  daysAfterCollection: int

class PaymentTermsPeriod_EndOfMonth(pydantic.BaseModel):
  endOfMonth: int

class PaymentTermsPeriod(pydantic.RootModel[typing.Union[PaymentTermsPeriod_DaysAfterCollection | PaymentTermsPeriod_EndOfMonth]]):
  pass

class DeliveryFee(pydantic.BaseModel):
  deliveryFee: common.BigDecimal
  vat: typing.Union[common.BigDecimal, None]

class PurchaseRequestDeliveryOption_CollectFromStorageLocation(pydantic.BaseModel):
  collectFromStorageLocation: "CollectFromStorageLocation"

class PurchaseRequestDeliveryOption_DeliveryToNominatedPurchaser(pydantic.BaseModel):
  deliveryToNominatedPurchaser: "DeliveryToNominatedPurchaser"

class PurchaseRequestDeliveryOption(pydantic.RootModel[typing.Union[PurchaseRequestDeliveryOption_CollectFromStorageLocation | PurchaseRequestDeliveryOption_DeliveryToNominatedPurchaser]]):
  pass

class CollectFromStorageLocation(pydantic.BaseModel):
  nominatedPurchaserCollection: "NominatedPurchaserCollection"
  incoterms: "CollectionIncoterms"
  purchaserCollectionDetails: "PurchaserCollectionDetails"

class CollectionIncoterms(str, enum.Enum):
  FCA: str = "FCA"
  EXW: str = "EXW"

class PurchaserCollectionDetails_NotAvailable(pydantic.BaseModel):
  notAvailable: None

class PurchaserCollectionDetails_CollectorDetails(pydantic.BaseModel):
  collectorDetails: "CollectorDetails"

class PurchaserCollectionDetails(pydantic.RootModel[typing.Union[PurchaserCollectionDetails_NotAvailable | PurchaserCollectionDetails_CollectorDetails]]):
  pass

class CollectorDetails(pydantic.BaseModel):
  collectionDate: common.LocalDate
  collectionTime: common.LocalTime
  collectionName: common_strings.StringNE
  vehicleRegistration: typing.Union[common_strings.StringNE, None]
  contactName: typing.Union[common_strings.StringNE, None]
  contactNumber: typing.Union[common_strings.StringNE, None]

class DeliveryToNominatedPurchaser(pydantic.BaseModel):
  deliveryDetails: "ShippingDelivery"

class DeliveryIncoterms(str, enum.Enum):
  DAP: str = "DAP"
  DDP: str = "DDP"
  CFR: str = "CFR"

class DutyAndVat(pydantic.BaseModel):
  duty: common.BigDecimal
  vat: common.BigDecimal

class PurchaseRequestStateEvent(pydantic.BaseModel):
  time: common.Instant
  state: "PurchaseRequestState"

class PurchaseRequestState(str, enum.Enum):
  new: str = "new"
  expired: str = "expired"
  cancelled: str = "cancelled"
  purchaserAccepted: str = "purchaserAccepted"
  purchaserRejected: str = "purchaserRejected"
  # This state is only relevant for purchase requests where some of the final products in this request
  # will be manufactured through a production order
  productionOrderComplete: str = "productionOrderComplete"
  readyForCollection: str = "readyForCollection"
  collected: str = "collected"
  delivered: str = "delivered"
  purchaserInvoiced: str = "purchaserInvoiced"
  purchaserPaid: str = "purchaserPaid"
  # The organisation has been paid by Fero
  organisationPaid: str = "organisationPaid"

class PurchaseRequestLineItem(pydantic.BaseModel):
  purchaseRequestId: "PurchaseRequestId"
  productId: "ProductId"
  paidUnits: "NumberOfUnits"
  freeUnits: typing.Union["NumberOfUnits", None]
  priceDiscount: typing.Union[common.BigDecimal, None]
  # The price per unit - this can be inclusive or exclusive of duty depending on how the pr was created
  unitPrice: common.BigDecimal
  # Will be ZERO if in bond sale
  dutyPerUnit: common.BigDecimal
  # Additional monthly fees that are charged to the organisation, incured
  # during the period that Fero was waiting for payment from the purchaser. 
  monthlyFeeCreditTotal: typing.Union[common.BigDecimal, None]
  saleProductId: typing.Union["SaleProductId", None]

type PurchaseRequestLineItemId = common_db.DbKey["PurchaseRequestLineItem"]

class NominatedPurchaserTerms(pydantic.BaseModel):
  terms: "Term"
  createdAt: common.Instant

type NominatedPurchaserTermsId = common_db.DbKey["NominatedPurchaserTerms"]

class PurchaseRequestAdditionalCost(pydantic.BaseModel):
  purchaseRequestId: "PurchaseRequestId"
  amount: common.BigDecimal
  description: common_strings.StringNE
  costType: "PurchaseRequestAdditionalCostType"

type PurchaseRequestAdditionalCostId = common_db.DbKey["PurchaseRequestAdditionalCost"]

class PurchaseRequestAdditionalCostType(str, enum.Enum):
  additionalDeliveryCost: str = "additionalDeliveryCost"
  customDeliveryCost: str = "customDeliveryCost"

class Incoterms(str, enum.Enum):
  EXW: str = "EXW"
  FCA: str = "FCA"
  DAP: str = "DAP"
  FOB: str = "FOB"
  DDP: str = "DDP"
  CFR: str = "CFR"

class OrganisationPaymentInfo(pydantic.BaseModel):
  organisationId: "OrganisationId"
  paymentDescriptionPrefix: common_strings.StringNE

class NegativeStockAdjustment(pydantic.BaseModel):
  # The ID of the storage location that reported this stock adjustment
  storageLocationId: "StorageLocationId"
  # A reference number provided by the storage location that raised this stock adjustment
  referenceNumber: str
  # A free text comment provided by the storage location, could be used to
  # describe what happened to the stock
  comment: typing.Union[str, None]
  # True if the storage location is going to be reimbursing the costs for
  # this stock adjustment
  reimbursingCosts: bool
  # Present if the storage location is going to reimburse costs and they have
  # been defined by a Fero admin
  additionalCosts: typing.Union["NegativeStockAdjustmentAdditionalCosts", None]
  createdAt: common.Instant
  state: "NegativeStockAdjustmentState"
  stateEvents: list["NegativeStockAdjustmentStateEvent"]

type NegativeStockAdjustmentId = common_db.DbKey["NegativeStockAdjustment"]

# Links a negative stock adjustment to multiple sale orders, so that a single
# negative stock adjustment can create loss records for multiple organisations
class NegativeStockAdjustmentSaleOrder(pydantic.BaseModel):
  negativeStockAdjustmentId: "NegativeStockAdjustmentId"
  saleOrderId: "SaleOrderId"

class NegativeStockAdjustmentAdditionalCosts(pydantic.BaseModel):
  storage: common.BigDecimal
  landing: common.BigDecimal

class NegativeStockAdjustmentStateEvent(pydantic.BaseModel):
  timestamp: common.Instant
  status: "NegativeStockAdjustmentState"

class NegativeStockAdjustmentState(str, enum.Enum):
  # The stock adjustment is created
  created: str = "created"
  # An admin user needs to review the stock adjustment and add landing /
  # storage costs. Only applicable to stock adjustments where the storage
  # location is going to reimburse the costs. 
  awaitingAdminReview: str = "awaitingAdminReview"
  # No further modifications can be made to this stock adjustment
  complete: str = "complete"

class PositiveStockAdjustment(pydantic.BaseModel):
  # The ID of the storage location that reported this stock adjustment
  storageLocationId: "StorageLocationId"
  # A reference number provided by the storage location that raised this stock adjustment
  referenceNumber: str
  # A free text comment provided by the storage location, could be used to
  # describe what happened to the stock
  comment: typing.Union[str, None]
  createdAt: common.Instant

type PositiveStockAdjustmentId = common_db.DbKey["PositiveStockAdjustment"]

# Links a positive stock adjustment to multiple new deal requests, so that a single
# positive stock adjustment can create new deal requests for multiple organisations
class PositiveStockAdjustmentNewDealRequest(pydantic.BaseModel):
  positiveStockAdjustmentId: "PositiveStockAdjustmentId"
  # Existing stock new deal request attached to this positive stock adjustment
  newDealRequestId: "NewDealRequestId"

# Returned from the vision API held orders endpoint. Either the request was successful
# and held orders were returned, or it was unsuccessful and a failure reason is provided.
# For more information on the endpoint, see https://visionsoftware.atlassian.net/wiki/spaces/VEDI/pages/924614693/2321+API+Get
class StorageLocationLcbHeldOrders_Success(pydantic.BaseModel):
  success: list["LcbHeldOrder"]

class StorageLocationLcbHeldOrders_Failure(pydantic.BaseModel):
  failure: "VisionEdiApiErrorDetails"

class StorageLocationLcbHeldOrders(pydantic.RootModel[typing.Union[StorageLocationLcbHeldOrders_Success | StorageLocationLcbHeldOrders_Failure]]):
  pass

# If the account code and site code are invalid, a failure reason is provided as well
# as the invalid account and site code.
class VisionEdiApiErrorDetails(pydantic.BaseModel):
  failureReason: "VisionEdiApiFailureReason"
  errorMessage: str
  ferovinumAccountCode: str
  siteCode: str

class VisionEdiApiFailureReason(str, enum.Enum):
  authenticationFailed: str = "authenticationFailed"
  permissionDeniedForParameters: str = "permissionDeniedForParameters"
  invalidParameters: str = "invalidParameters"
  serviceError: str = "serviceError"
  unauthorised: str = "unauthorised"
  apiAccessRestricted: str = "apiAccessRestricted"
  internalServerError: str = "internalServerError"
  requestProcessingFailed: str = "requestProcessingFailed"
  inputXmlValidationFailed: str = "inputXmlValidationFailed"
  multipleErrors: str = "multipleErrors"
  errorParsingContent: str = "errorParsingContent"
  unknownFailure: str = "unknownFailure"
  apiNotEnabled: str = "apiNotEnabled"
  missingParameters: str = "missingParameters"
  notFound: str = "notFound"

# Each held order will either have a matching sale order in the app db, or it won't.
# Unidentifiable held orders will be grouped together.
class LcbHeldOrder_IdentifiedInvoiceNumbers(pydantic.BaseModel):
  identifiedInvoiceNumbers: "IdentifiedLcbHeldOrders"

class LcbHeldOrder_UnidentifiedInvoiceNumbers(pydantic.BaseModel):
  unidentifiedInvoiceNumbers: list[str]

class LcbHeldOrder(pydantic.RootModel[typing.Union[LcbHeldOrder_IdentifiedInvoiceNumbers | LcbHeldOrder_UnidentifiedInvoiceNumbers]]):
  pass

# Held orders with matching sale orders will be grouped by the organisation.
class IdentifiedLcbHeldOrders(pydantic.BaseModel):
  organisationName: str
  invoiceNumbers: list[str]

class CacheType(str, enum.Enum):
  heldOrders: str = "heldOrders"
  visionGoodsReceived: str = "visionGoodsReceived"

class VisionGoodsReceivedCacheFlag(str, enum.Enum):
  pending: str = "pending"
  processed: str = "processed"
  failed: str = "failed"

class VisionGoodsReceivedCacheContent(pydantic.BaseModel):
  flag: "VisionGoodsReceivedCacheFlag"
  request: sys_types.Map[str, str]
  responseContent: str

# Stores the data that was cached.
class CacheContent_HeldOrders(pydantic.BaseModel):
  heldOrders: typing.Union["StorageLocationLcbHeldOrders", None]

class CacheContent_VisionGoodsReceived(pydantic.BaseModel):
  visionGoodsReceived: typing.Union["VisionGoodsReceivedCacheContent", None]

class CacheContent(pydantic.RootModel[typing.Union[CacheContent_HeldOrders | CacheContent_VisionGoodsReceived]]):
  pass

# This table is used for caching data that is only available via an external API that is rate-limited.
# E.g. Vision EDI APIs that can only be called 4 times/hr
# It can be deleted in the future when we have enough data that we want to cache to justify a
# centralised AWS cache like ElastiCache.
class DataCache(pydantic.BaseModel):
  # A unique identifier for each cached data
  identifier: str
  # The type of data being cache e.g. heldOrders
  # Having a separate row for the type will enable us to easily query the table for
  # objects of this type.
  cacheType: "CacheType"
  # The last time that this row was updated
  lastUpdated: common.Instant
  # The data being cached
  content: "CacheContent"

class OrgSettlementCreditLimit(pydantic.BaseModel):
  currency: "Currency"
  creditLimit: common.BigDecimal

class DealExtensionRequestState(str, enum.Enum):
  # initial new deal extension request state
  newDeal: str = "newDeal"
  # Ferovinum HQ has priced the deal
  newDealPriced: str = "newDealPriced"
  # Client organisation has accepted the deal
  orgAccepted: str = "orgAccepted"
  # Client organisation has rejected the deal pricing set by ferovinum
  orgRejected: str = "orgRejected"
  # A new deal extension has been created for the approved Deal Leg Extension Line Items
  newDealCreated: str = "newDealCreated"
  # When everything that the client has requested to renew has been repurchased before the roll
  nothingToExtend: str = "nothingToExtend"
  # The invoice for this deal extension has been sent to the client organisation
  invoiced: str = "invoiced"
  # The invoice has been marked as paid by the client organisation
  invoicePaid: str = "invoicePaid"
  # The payment has been acknowledged as received by Ferovinum HQ
  paymentReceived: str = "paymentReceived"

class DealExtensionRequestStateEvent(pydantic.BaseModel):
  time: common.Instant
  state: "DealExtensionRequestState"

class DealExtensionRequest(pydantic.BaseModel):
  # A human-readable identifier
  dealNumber: common_strings.StringNE
  # New created deal id
  newDealId: typing.Union["DealId", None]
  # Organisation that has requested this deal extension request
  organisationId: "OrganisationId"
  # The deal terms linked to this deal extension request
  dealTermsId: typing.Union["DealTermsId", None]
  # Additional deal terms set by Ferovinum
  additionalTermsText: typing.Union[common_strings.StringMD, None]
  # Current state of this deal extension request
  currentState: "DealExtensionRequestState"
  # A history of the state transitions for this agreement
  stateEvents: list["DealExtensionRequestStateEvent"]
  contractNoteId: typing.Union["ContractNoteId", None]
  invoiceId: typing.Union["InvoiceId", None]

type DealExtensionRequestId = common_db.DbKey["DealExtensionRequest"]

class DealLegExpiryStageType(str, enum.Enum):
  # Compulsory sale date stage of a deal leg
  CSD: str = "CSD"
  # Final sale date stage of a deal leg
  FSD: str = "FSD"

class DealLegExtensionLineItem(pydantic.BaseModel):
  # The ID of the deal leg that this extension applies to
  dealLegId: "DealLegId"
  # Deal extension request that this line item is attached to
  dealExtensionRequestId: "DealExtensionRequestId"
  # Max number of units that can be rolled over from the referenced deal leg
  quantityToRoll: "NumberOfUnits"
  # Mandatory approval reason for this deal leg extension line item
  approvalReason: common_strings.StringNE
  # compulsory sale date for the new deal
  compulsorySaleDate: typing.Union[common.LocalDate, None]
  # percentage (0.0-100.0)
  compulsorySalePc: typing.Union[common.BigDecimal, None]
  # final date for the new deal
  finalDate: typing.Union[common.LocalDate, None]
  # Purchase price per unit in the new deal
  purchasePricePerUnit: typing.Union[common.BigDecimal, None]
  # Deposit price in the new deal
  depositPrice: typing.Union[common.BigDecimal, None]
  # Monthly fee specification
  monthlyFee: typing.Union["MonthlyFee", None]
  # percentage (0.0-100.0)
  throughputFeePc: typing.Union[common.BigDecimal, None]

# Stock Sync
class StorageLocationStockSync(pydantic.BaseModel):
  locId: "StorageLocationId"
  createdAt: common.Instant

type StorageLocationStockSyncId = common_db.DbKey["StorageLocationStockSync"]

class S3Bucket(str, enum.Enum):
  assets: str = "assets"
  documents: str = "documents"

class FileEntityRef_SaleOrderId(pydantic.BaseModel):
  saleOrderId: common_db.DbKey["SaleOrder"]

class FileEntityRef_PurchaseRequestId(pydantic.BaseModel):
  purchaseRequestId: common_db.DbKey["PurchaseRequest"]

class FileEntityRef_NewDealRequestId(pydantic.BaseModel):
  newDealRequestId: common_db.DbKey["NewDealRequest"]

class FileEntityRef_RevaluationRequestId(pydantic.BaseModel):
  revaluationRequestId: common_db.DbKey["RevaluationRequest"]

class FileEntityRef_DealExtensionRequestId(pydantic.BaseModel):
  dealExtensionRequestId: common_db.DbKey["DealExtensionRequest"]

class FileEntityRef_OrganisationId(pydantic.BaseModel):
  organisationId: common_db.DbKey["Organisation"]

class FileEntityRef_ProductionOrderId(pydantic.BaseModel):
  productionOrderId: common_db.DbKey["ProductionOrder"]

class FileEntityRef_ContractNoteId(pydantic.BaseModel):
  contractNoteId: common_db.DbKey["ContractNote"]

class FileEntityRef_NominatedPurchaserId(pydantic.BaseModel):
  nominatedPurchaserId: common_db.DbKey["NominatedPurchaser"]

class FileEntityRef_Pending(pydantic.BaseModel):
  pending: None

class FileEntityRef(pydantic.RootModel[typing.Union[FileEntityRef_SaleOrderId | FileEntityRef_PurchaseRequestId | FileEntityRef_NewDealRequestId | FileEntityRef_RevaluationRequestId | FileEntityRef_DealExtensionRequestId | FileEntityRef_OrganisationId | FileEntityRef_ProductionOrderId | FileEntityRef_ContractNoteId | FileEntityRef_NominatedPurchaserId | FileEntityRef_Pending]]):
  pass

class StoredFile(pydantic.BaseModel):
  # S3 info
  s3Key: str
  s3Bucket: "S3Bucket"
  # General file metadata
  fileName: str
  fileType: str
  mimeType: str
  createdAt: common.Instant
  # The ID of the object that this file is linked to
  entityRef: "FileEntityRef"
  replacedBy: typing.Union["StoredFileId", None]

type StoredFileId = common_db.DbKey["StoredFile"]

# Revaluation Roll
class RevaluationRequest(pydantic.BaseModel):
  dealNumber: common_strings.StringNE
  organisationId: "OrganisationId"
  dealTermsId: "DealTermsId"
  dateOfRequest: common.LocalDate
  state: "RevaluationRequestState"
  stateEvents: list["RevaluationRequestStateEvent"]
  contractNoteId: typing.Union["ContractNoteId", None]
  invoiceId: typing.Union["InvoiceId", None]

type RevaluationRequestId = common_db.DbKey["RevaluationRequest"]

class RevaluationRequestLineItem(pydantic.BaseModel):
  revaluationRequestId: "RevaluationRequestId"
  sourceDealLegId: "DealLegId"
  purchasePricePerUnit: ferovinum_app_types.Decimal
  monthlyFee: "MonthlyFee"
  throughputFeePct: ferovinum_app_types.Decimal
  depositPct: ferovinum_app_types.Decimal
  depositPrice: ferovinum_app_types.Decimal
  numberOfUnits: typing.Union["NumberOfUnits", None]

type RevaluationRequestLineItemId = common_db.DbKey["RevaluationRequestLineItem"]

class RevaluationRequestState(str, enum.Enum):
  created: str = "created"
  orgAccepted: str = "orgAccepted"
  orgRejected: str = "orgRejected"

class RevaluationRequestStateEvent(pydantic.BaseModel):
  time: common.Instant
  state: "RevaluationRequestState"

class ContractNoteLineItem(pydantic.BaseModel):
  productId: "ProductId"
  purchasePrice: ferovinum_app_types.Decimal
  depositPrice: ferovinum_app_types.Decimal
  totalNumberOfUnits: "NumberOfUnits"
  numberOfFreeUnits: "NumberOfUnits"
  compulsorySaleDate: common.LocalDate
  compulsorySalePc: ferovinum_app_types.Decimal
  finalDate: common.LocalDate

class ContractNote(pydantic.BaseModel):
  dealNumber: str
  organisationId: "OrganisationId"
  dealCurrency: "Currency"
  throughputFeePc: ferovinum_app_types.Decimal
  monthlyFee: "MonthlyFee"
  executionDate: common.LocalDate
  startDate: common.LocalDate
  endDate: common.LocalDate
  masterAgreementSignedDate: common.LocalDate
  dealTerms: "DealTerms"
  additionalTermsText: common_strings.StringMD
  grossTotal: ferovinum_app_types.Decimal
  grossDepositTotal: ferovinum_app_types.Decimal
  depositCreditTotal: ferovinum_app_types.Decimal
  netAdvanceAmount: ferovinum_app_types.Decimal
  pdfFileId: typing.Union["StoredFileId", None]
  lineItems: list["ContractNoteLineItem"]

type ContractNoteId = common_db.DbKey["ContractNote"]

class ProductSalePrice(pydantic.BaseModel):
  productId: "ProductId"
  productCode: common_strings.StringNE
  organisationId: "OrganisationId"
  purchaserId: typing.Union["PurchaserId", None]
  currency: "Currency"
  incDutyExVatPrice: typing.Union[common.BigDecimal, None]
  incDutyAndVatPrice: typing.Union[common.BigDecimal, None]
  exDutyAndVatPrice: typing.Union[common.BigDecimal, None]

class Invoice(pydantic.BaseModel):
  invoiceNumber: str
  title: str
  itemDescription: str
  orgName: str
  orgAddress: str
  invoiceDate: common.LocalDate
  dueDate: common.LocalDate
  vatNumber: str
  currency: "Currency"
  lineItems: list["InvoiceLineItem"]
  netSubtotal: float
  dutySubtotal: typing.Union[float, None] = pydantic.Field(default=None)
  vatSubtotal: typing.Union[float, None] = pydantic.Field(default=None)
  depositSubtotal: typing.Union[float, None] = pydantic.Field(default=None)
  discountSubtotal: typing.Union[float, None] = pydantic.Field(default=None)
  paidSubtotal: typing.Union[float, None] = pydantic.Field(default=None)
  settlementCreditUsageFee: typing.Union[float, None] = pydantic.Field(default=None)
  total: float
  amountDue: float
  reference: typing.Union[str, None] = pydantic.Field(default=None)
  note: str = pydantic.Field(default="")
  terms: str = pydantic.Field(default="")
  locale: "Locale" = pydantic.Field(default="London")
  timezoneOffset: int = pydantic.Field(default=0)
  storedFileId: "StoredFileId"
  source: "InvoiceSource"
  emailRecipient: "InvoiceEmailRecipient"
  customType: "InvoiceCustomType" = pydantic.Field(default="none")
  customerPoRef: typing.Union[str, None] = pydantic.Field(default=None)
  proforma: bool = pydantic.Field(default=False)
  internationalShippingDetails: typing.Union["ShippingDelivery", None] = pydantic.Field(default=None)

type InvoiceId = common_db.DbKey["Invoice"]

class InvoiceLineItem(pydantic.BaseModel):
  code: str
  description: str
  quantity: float
  quantitySuffix: typing.Union[str, None] = pydantic.Field(default=None)
  unitPrice: float
  vatPc: typing.Union[float, None] = pydantic.Field(default=None)
  subtotal: float
  discountAmount: typing.Union[float, None] = pydantic.Field(default=None)

class InvoiceSource_NewDealRequest(pydantic.BaseModel):
  newDealRequest: "NewDealRequestId"

class InvoiceSource_SaleOrder(pydantic.BaseModel):
  saleOrder: "SaleOrderId"

class InvoiceSource_DealRollSaleOrder(pydantic.BaseModel):
  dealRollSaleOrder: "SaleOrderId"

class InvoiceSource_PurchaseRequest(pydantic.BaseModel):
  purchaseRequest: "PurchaseRequestId"

class InvoiceSource_NominatedPurchaser(pydantic.BaseModel):
  nominatedPurchaser: "NominatedPurchaserId"

class InvoiceSource(pydantic.RootModel[typing.Union[InvoiceSource_NewDealRequest | InvoiceSource_SaleOrder | InvoiceSource_DealRollSaleOrder | InvoiceSource_PurchaseRequest | InvoiceSource_NominatedPurchaser]]):
  pass

class InvoiceEmailRecipient_Organisation(pydantic.BaseModel):
  organisation: "OrganisationId"

class InvoiceEmailRecipient_Purchaser(pydantic.BaseModel):
  purchaser: "PurchaserId"

class InvoiceEmailRecipient(pydantic.RootModel[typing.Union[InvoiceEmailRecipient_Organisation | InvoiceEmailRecipient_Purchaser]]):
  pass

class InvoiceCustomReplaceLogoWithOrgLogo(pydantic.BaseModel):
  organisation: "OrganisationId"

class InvoiceCustomType_None(pydantic.BaseModel):
  none: None

class InvoiceCustomType_Simple_logo_replacement_with_org_logo(pydantic.BaseModel):
  simple_logo_replacement_with_org_logo: "InvoiceCustomReplaceLogoWithOrgLogo"

class InvoiceCustomType(pydantic.RootModel[typing.Union[InvoiceCustomType_None | InvoiceCustomType_Simple_logo_replacement_with_org_logo]]):
  pass

class StorefrontConfig(pydantic.BaseModel):
  nominatedPurchaserId: common_db.DbKey["NominatedPurchaser"]
  storefrontEnabled: bool
  storageLocationId: common_db.DbKey["StorageLocation"]
  paymentTerm: "PaymentTermsPeriod"
  minimumOrderValue: common.BigDecimal

type StorefrontConfigId = common_db.DbKey["StorefrontConfig"]

class StorefrontProductAllocation(pydantic.BaseModel):
  storefrontConfigId: common_db.DbKey["StorefrontConfig"]
  productId: "ProductId"
  price: common.BigDecimal
  remainingQuantity: int

class SettlementCreditClaim(pydantic.BaseModel):
  counterPartyId: common_strings.StringNE
  sourceId: common_strings.StringNE
  amount: float
  currency: "Currency"
  settledAt: typing.Union[common.Instant, None]
  cancelledAt: typing.Union[common.Instant, None]
  createdAt: common.Instant

class CreditEntityRef_Organisation(pydantic.BaseModel):
  organisation: "OrganisationId"

class CreditEntityRef_Purchaser(pydantic.BaseModel):
  purchaser: "PurchaserId"

class CreditEntityRef_Purchaser_np(pydantic.BaseModel):
  purchaser_np: "NominatedPurchaserId"

class CreditEntityRef(pydantic.RootModel[typing.Union[CreditEntityRef_Organisation | CreditEntityRef_Purchaser | CreditEntityRef_Purchaser_np]]):
  pass

class SettlementCreditLimit(pydantic.BaseModel):
  entityRef: "CreditEntityRef"
  currency: "Currency"
  creditLimit: float
  paymentTerms: "PaymentTermsPeriod"
  creditFeesPct: ferovinum_app_types.Decimal

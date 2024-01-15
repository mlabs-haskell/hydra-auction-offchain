// AnnounceAuction -------------------------------------------------------------

export type AnnounceAuctionContractParams = {
  auctionTerms: AuctionTermsInput;
  additionalAuctionLotOrefs: Array<TransactionInput>;
};

export type StartBiddingContractParams = {
  auctionInfo: AuctionInfo;
};

export type AuctionInfo = {
  auctionId: CurrencySymbol;
  auctionTerms: AuctionTerms;
  auctionEscrowAddr: Address;
  bidderDepositAddr: Address;
  feeEscrowAddr: Address;
  standingBidAddr: Address;
};

export type PlaceBidContractParams = {
  auctionInfo: AuctionInfo;
  sellerSignature: ByteArray;
  bidAmount: BigInt;
};

export interface AuctionTermsInput {
  auctionLot: Value;
  delegates: Array<PubKeyHash>;
  biddingStart: POSIXTime;
  biddingEnd: POSIXTime;
  purchaseDeadline: POSIXTime;
  cleanup: POSIXTime;
  auctionFeePerDelegate: BigInt;
  startingBid: BigInt;
  minBidIncrement: BigInt;
  minDepositAmount: BigInt;
}

export interface AuctionTerms extends AuctionTermsInput {
  sellerPkh: PubKeyHash;
  sellerVk: ByteArray;
}

// Common ----------------------------------------------------------------------

export type WalletApp =
  | "Nami"
  | "Gero"
  | "Flint"
  | "Eternl"
  | "Lode"
  | "NuFi"
  | "Lace"
  | "Plutip";

export type ContractOutput<T> = ContractOutputResult<T> | ContractOutputError;

export type ContractOutputResult<T> = {
  tag: "result";
  value: T;
};

export type ContractOutputError = {
  tag: "error";
  value: ContractError;
};

export type ContractError = {
  errorCode: string;
  message: string;
};

// Cardano ---------------------------------------------------------------------

export type Address = string;

export type BigInt = string;

export type ByteArray = string;

export type CurrencySymbol = string;

export type POSIXTime = string;

export type PubKeyHash = string;

export type TokenName = string;

export type TransactionInput = {
  transactionId: TransactionHash;
  index: UInt;
};

export type TransactionHash = string;

export type TxCbor = string;

export type UInt = string;

export type Value = Array<ValueEntry>;

export type ValueEntry = {
  cs: CurrencySymbol;
  tn: TokenName;
  quantity: BigInt;
};

export type VerificationKey = string;

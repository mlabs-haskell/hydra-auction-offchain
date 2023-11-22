// AnnounceAuction -------------------------------------------------------------

export type AnnounceAuctionContractParams = {
  auctionTerms: AuctionTerms;
  additionalAuctionLotOrefs: Array<TransactionInput>;
};

export type AuctionTerms = {
  auctionLot: Value;
  sellerPkh: PubKeyHash;
  sellerVk: ByteArray;
  delegates: Array<PubKeyHash>;
  biddingStart: POSIXTime;
  biddingEnd: POSIXTime;
  purchaseDeadline: POSIXTime;
  cleanup: POSIXTime;
  auctionFeePerDelegate: BigInt;
  startingBid: BigInt;
  minBidIncrement: BigInt;
  minDepositAmount: BigInt;
};

// Common ----------------------------------------------------------------------

export type WalletApp = "Nami" | "Gero" | "Flint" | "Eternl" | "Lode" | "NuFi" | "Lace";

export type ContractOutput<T> = T | ContractError;

export type ContractError = {
  errorCode: string;
  message: string;
};

// Cardano ---------------------------------------------------------------------

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

export type UInt = string;

export type Value = Array<ValueEntry>;

export type ValueEntry = {
  cs: CurrencySymbol;
  tn: TokenName;
  quantity: BigInt;
};

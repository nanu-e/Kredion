# Kredion

**Kredion** is a decentralized lending and borrowing smart contract built on the **Clarity** language for the Stacks blockchain. It enables peer-to-peer microloans with transparent tracking of deposits, loans, repayments, and interest accruals — all without intermediaries.

## Overview

Kredion allows users to deposit STX to earn interest, while borrowers can request and repay loans directly on-chain. The contract ensures fair enforcement of loan terms, automatic repayment tracking, and balance visibility for all participants.

## Features

* **Decentralized Lending:** Lenders can deposit STX and earn returns based on loan repayments.
* **Borrower Access:** Borrowers can request funds within defined limits and repay incrementally.
* **Automated Tracking:** All deposits, loans, and repayments are transparently recorded on-chain.
* **Interest Mechanism:** Supports defining interest rates for calculating repayment totals.
* **Security Controls:** Prevents over-borrowing and double repayment through built-in assertions.

## Data Structures

* **Deposits Map:** Tracks each user’s deposited balance.
* **Loans Map:** Maintains borrower information, including principal, interest, and repayment status.
* **Admin Constant:** Identifies the system administrator authorized for specific configurations.

## Key Functions

* `deposit`: Allows users to deposit STX for lending.
* `request-loan`: Lets borrowers request funds, validated against available liquidity.
* `repay-loan`: Enables borrowers to repay outstanding balances with interest.
* `get-deposit-balance`: Returns the STX balance deposited by a user.
* `get-loan-details`: Retrieves details of a borrower’s active loan.
* `withdraw`: Allows lenders to withdraw available funds if not tied to active loans.

## Error Codes

* **u100:** Unauthorized action — caller is not admin.
* **u101:** Insufficient balance or liquidity.
* **u102:** Loan cap exceeded or invalid loan amount.
* **u103:** Repayment exceeds outstanding balance.
* **u104:** Transfer or repayment operation failed.

## Usage Flow

1. **Lenders** deposit STX into the pool using `deposit`.
2. **Borrowers** request loans through `request-loan`, specifying the desired amount.
3. Upon approval, funds are transferred, and loan terms are set.
4. Borrowers make repayments using `repay-loan`.
5. Lenders can later withdraw available funds with accrued returns via `withdraw`.

## Security Notes

* All transactions are recorded immutably on-chain.
* Assertions prevent unauthorized actions and invalid transfers.
* Administrators can configure contract-level parameters but cannot alter loan records.

## License

This contract is released under the **MIT License** and is free to use, modify, and extend within decentralized finance ecosystems.

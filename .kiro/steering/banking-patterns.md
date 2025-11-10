# Banking Patterns in COBOL

This document provides guidance for recognizing and understanding common COBOL patterns in banking systems, helping you analyze legacy code and translate business logic to modern architectures.

## Common Banking Patterns

### 1. Interest Accrual

**Business Purpose**: Calculate interest on accounts based on principal, rate, and time period.

**COBOL Pattern**:

```cobol
COMPUTE INTEREST-AMOUNT = PRINCIPAL-BALANCE *
    (INTEREST-RATE / 100) * (DAYS-ELAPSED / 365).

ADD INTEREST-AMOUNT TO ACCOUNT-BALANCE.
```

**Key Indicators**:

- COMPUTE statements with multiplication and division
- Variables named INTEREST, RATE, PRINCIPAL, BALANCE
- Date arithmetic (DAYS-ELAPSED, DAYS-IN-PERIOD)
- Division by 365, 360, or 12 (day-count conventions)

**Business Meaning**: Simple interest calculation using actual/365 day-count convention. The pattern multiplies principal by annual rate (converted from percentage) by time fraction.

**AWS Translation**: Lambda function with business logic, DynamoDB for account state, EventBridge for scheduled daily accrual.

### 2. Loan Amortization

**Business Purpose**: Calculate periodic loan payments and principal/interest breakdown.

**COBOL Pattern**:

```cobol
COMPUTE MONTHLY-RATE = ANNUAL-RATE / 1200.
COMPUTE PAYMENT-AMOUNT = LOAN-PRINCIPAL *
    (MONTHLY-RATE * (1 + MONTHLY-RATE) ** LOAN-TERM) /
    ((1 + MONTHLY-RATE) ** LOAN-TERM - 1).

COMPUTE INTEREST-PORTION = REMAINING-BALANCE * MONTHLY-RATE.
COMPUTE PRINCIPAL-PORTION = PAYMENT-AMOUNT - INTEREST-PORTION.
```

**Key Indicators**:

- Exponentiation operator (\*\*)
- Variables named PAYMENT, PRINCIPAL, TERM, AMORTIZATION
- Iterative calculations in PERFORM loops
- Running balance updates

**Business Meaning**: Standard amortization formula calculating fixed payment amount, then splitting each payment into interest and principal components.

**AWS Translation**: Lambda function for calculation, Step Functions for payment schedule generation, S3 for amortization tables.

### 3. Transaction Posting

**Business Purpose**: Validate and post financial transactions to accounts with double-entry bookkeeping.

**COBOL Pattern**:

```cobol
IF TRANSACTION-TYPE = 'DEBIT'
    SUBTRACT TRANSACTION-AMOUNT FROM ACCOUNT-BALANCE
    IF ACCOUNT-BALANCE < MINIMUM-BALANCE
        MOVE 'INSUFFICIENT-FUNDS' TO ERROR-CODE
        PERFORM ROLLBACK-TRANSACTION
    ELSE
        ADD TRANSACTION-AMOUNT TO DEBIT-TOTAL
        PERFORM UPDATE-ACCOUNT-RECORD
    END-IF
ELSE IF TRANSACTION-TYPE = 'CREDIT'
    ADD TRANSACTION-AMOUNT TO ACCOUNT-BALANCE
    ADD TRANSACTION-AMOUNT TO CREDIT-TOTAL
    PERFORM UPDATE-ACCOUNT-RECORD
END-IF.
```

**Key Indicators**:

- IF-THEN-ELSE logic for transaction types
- Balance validation checks
- ROLLBACK or error handling procedures
- Audit trail creation (PERFORM LOG-TRANSACTION)
- Double-entry accounting (debit/credit pairs)

**Business Meaning**: Atomic transaction processing with validation, balance updates, and error handling. Ensures account integrity.

**AWS Translation**: Lambda function with DynamoDB transactions, SQS for async processing, CloudWatch for audit logs.

### 4. Batch Reconciliation

**Business Purpose**: Match and reconcile transactions between systems or against external files.

**COBOL Pattern**:

```cobol
PERFORM UNTIL END-OF-FILE-1 OR END-OF-FILE-2
    IF KEY-1 = KEY-2
        IF AMOUNT-1 = AMOUNT-2
            ADD 1 TO MATCHED-COUNT
            PERFORM WRITE-MATCHED-RECORD
        ELSE
            ADD 1 TO DISCREPANCY-COUNT
            PERFORM WRITE-EXCEPTION-RECORD
        END-IF
        PERFORM READ-FILE-1
        PERFORM READ-FILE-2
    ELSE IF KEY-1 < KEY-2
        ADD 1 TO UNMATCHED-COUNT-1
        PERFORM WRITE-UNMATCHED-RECORD-1
        PERFORM READ-FILE-1
    ELSE
        ADD 1 TO UNMATCHED-COUNT-2
        PERFORM WRITE-UNMATCHED-RECORD-2
        PERFORM READ-FILE-2
    END-IF
END-PERFORM.
```

**Key Indicators**:

- Sequential file processing with READ statements
- Key comparison logic (merge algorithm)
- Counter variables (MATCHED-COUNT, EXCEPTION-COUNT)
- Multiple output files (matched, unmatched, exceptions)
- END-OF-FILE conditions

**Business Meaning**: Two-way merge reconciliation comparing sorted files by key, identifying matches, discrepancies, and orphaned records.

**AWS Translation**: Step Functions workflow, Lambda for comparison logic, S3 for input/output files, SNS for exception alerts.

### 5. Balance Inquiry

**Business Purpose**: Retrieve and display account balance with available credit calculation.

**COBOL Pattern**:

```cobol
EXEC SQL
    SELECT CURRENT_BALANCE, CREDIT_LIMIT, HOLD_AMOUNT
    INTO :WS-BALANCE, :WS-LIMIT, :WS-HOLDS
    FROM ACCOUNTS
    WHERE ACCOUNT_NUMBER = :WS-ACCOUNT-NUM
END-EXEC.

COMPUTE AVAILABLE-BALANCE = WS-BALANCE + WS-LIMIT - WS-HOLDS.

IF AVAILABLE-BALANCE < 0
    MOVE 'OVERLIMIT' TO STATUS-CODE
ELSE
    MOVE 'OK' TO STATUS-CODE
END-IF.
```

**Key Indicators**:

- EXEC SQL embedded SQL statements
- Host variables (prefixed with :)
- Balance calculations (available = current + limit - holds)
- Status code assignments

**Business Meaning**: Database query to retrieve account data, calculate available balance considering credit limit and holds, return status.

**AWS Translation**: Lambda function, RDS or DynamoDB for account data, API Gateway for REST endpoint.

### 6. Payment Validation

**Business Purpose**: Validate payment requests against business rules before processing.

**COBOL Pattern**:

```cobol
PERFORM VALIDATE-PAYMENT-AMOUNT.
PERFORM VALIDATE-ACCOUNT-STATUS.
PERFORM VALIDATE-DAILY-LIMIT.
PERFORM CHECK-FRAUD-RULES.

IF ERROR-COUNT > 0
    MOVE 'VALIDATION-FAILED' TO RESPONSE-CODE
    PERFORM RETURN-ERROR-DETAILS
ELSE
    PERFORM PROCESS-PAYMENT
END-IF.

VALIDATE-PAYMENT-AMOUNT.
    IF PAYMENT-AMOUNT <= 0 OR PAYMENT-AMOUNT > 999999.99
        ADD 1 TO ERROR-COUNT
        MOVE 'INVALID-AMOUNT' TO ERROR-TABLE(ERROR-COUNT)
    END-IF.
```

**Key Indicators**:

- Multiple PERFORM statements for validation steps
- Error accumulation pattern (ERROR-COUNT)
- Range checks and business rule validations
- Error table or array for multiple errors
- Conditional processing based on validation results

**Business Meaning**: Multi-step validation pipeline checking amount ranges, account status, limits, and fraud rules before payment processing.

**AWS Translation**: Lambda function with validation chain, DynamoDB for rules, Step Functions for complex workflows.

## COBOL Keywords and Banking Context

### Data Division Keywords

- **PIC 9(n)V99**: Numeric field with 2 decimal places (currency amounts)
- **PIC X(n)**: Alphanumeric field (account numbers, names)
- **PIC S9(n) COMP-3**: Packed decimal (efficient storage for calculations)
- **OCCURS n TIMES**: Array definition (transaction lists, payment schedules)
- **REDEFINES**: Alternate view of same memory (union types)

### Procedure Division Keywords

- **COMPUTE**: Arithmetic expression evaluation (interest, payments)
- **PERFORM**: Subroutine call or loop (batch processing)
- **EXEC SQL**: Embedded SQL (database operations)
- **CALL**: External program invocation (shared utilities)
- **EVALUATE**: Multi-way branch (transaction type routing)

### File Processing Keywords

- **READ**: Sequential or random file access (batch files)
- **WRITE**: Output record creation (reports, extracts)
- **REWRITE**: Update existing record (account updates)
- **START**: Position file pointer (indexed file access)
- **OPEN/CLOSE**: File lifecycle management

## Banking Terms Glossary

- **Principal**: Original loan or deposit amount
- **Interest Rate**: Percentage charged or earned on principal
- **Amortization**: Gradual loan repayment through scheduled payments
- **Day-Count Convention**: Method for calculating interest periods (actual/365, 30/360)
- **Double-Entry Bookkeeping**: Every transaction has equal debit and credit entries
- **Reconciliation**: Process of matching transactions between systems
- **Hold Amount**: Funds reserved but not yet debited (pending transactions)
- **Credit Limit**: Maximum negative balance allowed (overdraft protection)
- **Batch Processing**: Processing groups of transactions together (end-of-day)
- **Posting**: Recording transaction to account ledger
- **Clearing**: Settlement process between financial institutions
- **Nostro/Vostro**: Correspondent banking accounts (our account at their bank / their account at our bank)

## Regulatory Compliance Indicators

### Audit Trail Requirements

**Pattern Recognition**:

```cobol
MOVE FUNCTION CURRENT-DATE TO AUDIT-TIMESTAMP.
MOVE USER-ID TO AUDIT-USER.
MOVE TRANSACTION-AMOUNT TO AUDIT-AMOUNT.
WRITE AUDIT-RECORD.
```

**Compliance Purpose**: SOX, PCI-DSS, GLBA require complete audit trails of financial transactions.

**AWS Implementation**: CloudWatch Logs, CloudTrail, DynamoDB with point-in-time recovery.

### Data Retention

**Pattern Recognition**:

```cobol
IF TRANSACTION-DATE < RETENTION-CUTOFF-DATE
    PERFORM ARCHIVE-TRANSACTION
ELSE
    PERFORM PROCESS-TRANSACTION
END-IF.
```

**Compliance Purpose**: Regulations require 7-year retention for financial records.

**AWS Implementation**: S3 with lifecycle policies, Glacier for long-term storage.

### Access Control

**Pattern Recognition**:

```cobol
IF USER-ROLE NOT = 'TELLER' AND USER-ROLE NOT = 'MANAGER'
    MOVE 'UNAUTHORIZED' TO ERROR-CODE
    PERFORM SECURITY-VIOLATION-LOG
    STOP RUN
END-IF.
```

**Compliance Purpose**: Least-privilege access, separation of duties (SOX, GLBA).

**AWS Implementation**: IAM roles, Cognito user pools, API Gateway authorizers.

### Encryption Requirements

**Pattern Recognition**:

```cobol
CALL 'ENCRYPT' USING ACCOUNT-NUMBER, ENCRYPTION-KEY, ENCRYPTED-VALUE.
MOVE ENCRYPTED-VALUE TO OUTPUT-FIELD.
```

**Compliance Purpose**: PCI-DSS requires encryption of cardholder data, GLBA for customer information.

**AWS Implementation**: KMS for encryption keys, encrypted EBS volumes, TLS for transit.

### Transaction Limits

**Pattern Recognition**:

```cobol
IF DAILY-TRANSACTION-TOTAL + TRANSACTION-AMOUNT > DAILY-LIMIT
    MOVE 'LIMIT-EXCEEDED' TO ERROR-CODE
    PERFORM ALERT-COMPLIANCE-TEAM
END-IF.
```

**Compliance Purpose**: Anti-money laundering (AML), Bank Secrecy Act (BSA) reporting thresholds.

**AWS Implementation**: Lambda with DynamoDB for limit tracking, SNS for alerts, Step Functions for review workflows.

## Pattern Recognition Guidelines

### High-Priority Modernization Candidates

1. **Stateless Calculations**: Interest, payments, validations (easy Lambda conversion)
2. **API-Ready Logic**: Balance inquiry, transaction status (direct API Gateway mapping)
3. **Isolated Modules**: Low coupling to other programs (reduced risk)

### Complex Patterns Requiring Careful Analysis

1. **File-Based Batch Processing**: Requires Step Functions orchestration
2. **Mainframe Database Access**: May need hybrid architecture with MQ
3. **CICS Transaction Processing**: Complex state management, consider ECS containers
4. **Tightly Coupled Programs**: Modernize as a group or use strangler pattern

### Red Flags for Manual Review

1. **Undocumented Business Logic**: Complex COMPUTE statements without comments
2. **Hard-Coded Values**: Magic numbers that may be regulatory thresholds
3. **Error Handling Gaps**: Missing validation or rollback logic
4. **Performance-Critical Code**: Sub-second response requirements
5. **Regulatory Calculations**: Interest, fees, penalties (require exact replication)

## Usage in COBRA

When analyzing COBOL code, COBRA uses these patterns to:

1. **Identify Business Logic**: Match code structures to known banking patterns
2. **Generate Explanations**: Provide business context for technical code
3. **Suggest AWS Services**: Map patterns to appropriate cloud services
4. **Assess Modernization Risk**: Evaluate complexity and coupling
5. **Create Specifications**: Generate requirements based on recognized patterns

## Examples for Testing

Use these COBOL snippets to test pattern recognition:

- `examples/interest-calculation.cbl`: Interest accrual pattern
- `examples/transaction-posting.cbl`: Transaction validation and posting
- `examples/batch-reconciliation.cbl`: File reconciliation pattern

When COBRA encounters these patterns, it should automatically recognize the banking operation and provide appropriate modernization recommendations.

import { useState } from 'react'
import './ExampleSnippets.css'

interface Example {
  id: string
  name: string
  description: string
  code: string
}

const examples: Example[] = [
  {
    id: 'interest',
    name: 'Interest Calculation',
    description: 'Calculate simple interest on account balance',
    code: `       IDENTIFICATION DIVISION.
       PROGRAM-ID. INTEREST-CALC.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  PRINCIPAL-BALANCE      PIC 9(7)V99 VALUE 10000.00.
       01  INTEREST-RATE          PIC 9V9999  VALUE 0.0525.
       01  DAYS-ELAPSED           PIC 9(3)    VALUE 365.
       01  INTEREST-AMOUNT        PIC 9(7)V99.
       01  NEW-BALANCE            PIC 9(7)V99.
       
       PROCEDURE DIVISION.
       MAIN-LOGIC.
           COMPUTE INTEREST-AMOUNT = PRINCIPAL-BALANCE *
               INTEREST-RATE * (DAYS-ELAPSED / 365).
           
           ADD INTEREST-AMOUNT TO PRINCIPAL-BALANCE 
               GIVING NEW-BALANCE.
           
           DISPLAY "Principal: " PRINCIPAL-BALANCE.
           DISPLAY "Interest Rate: " INTEREST-RATE.
           DISPLAY "Interest Earned: " INTEREST-AMOUNT.
           DISPLAY "New Balance: " NEW-BALANCE.
           
           STOP RUN.`
  },
  {
    id: 'balance',
    name: 'Balance Inquiry',
    description: 'Query account balance with available credit calculation',
    code: `       IDENTIFICATION DIVISION.
       PROGRAM-ID. BALANCE-INQ.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-ACCOUNT-NUM         PIC 9(10).
       01  WS-BALANCE             PIC S9(9)V99.
       01  WS-LIMIT               PIC 9(7)V99.
       01  WS-HOLDS               PIC 9(7)V99.
       01  AVAILABLE-BALANCE      PIC S9(9)V99.
       01  STATUS-CODE            PIC X(10).
       
       PROCEDURE DIVISION.
       MAIN-LOGIC.
           ACCEPT WS-ACCOUNT-NUM.
           
           EXEC SQL
               SELECT CURRENT_BALANCE, CREDIT_LIMIT, HOLD_AMOUNT
               INTO :WS-BALANCE, :WS-LIMIT, :WS-HOLDS
               FROM ACCOUNTS
               WHERE ACCOUNT_NUMBER = :WS-ACCOUNT-NUM
           END-EXEC.
           
           COMPUTE AVAILABLE-BALANCE = 
               WS-BALANCE + WS-LIMIT - WS-HOLDS.
           
           IF AVAILABLE-BALANCE < 0
               MOVE 'OVERLIMIT' TO STATUS-CODE
           ELSE
               MOVE 'OK' TO STATUS-CODE
           END-IF.
           
           DISPLAY "Account: " WS-ACCOUNT-NUM.
           DISPLAY "Current Balance: " WS-BALANCE.
           DISPLAY "Available: " AVAILABLE-BALANCE.
           DISPLAY "Status: " STATUS-CODE.
           
           STOP RUN.`
  },
  {
    id: 'transaction',
    name: 'Transaction Posting',
    description: 'Validate and post financial transactions with error handling',
    code: `       IDENTIFICATION DIVISION.
       PROGRAM-ID. TXN-POST.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  TRANSACTION-TYPE       PIC X(6).
       01  TRANSACTION-AMOUNT     PIC 9(7)V99.
       01  ACCOUNT-BALANCE        PIC S9(9)V99.
       01  MINIMUM-BALANCE        PIC 9(7)V99 VALUE 100.00.
       01  ERROR-CODE             PIC X(20).
       01  DEBIT-TOTAL            PIC 9(9)V99 VALUE 0.
       01  CREDIT-TOTAL           PIC 9(9)V99 VALUE 0.
       
       PROCEDURE DIVISION.
       MAIN-LOGIC.
           ACCEPT TRANSACTION-TYPE.
           ACCEPT TRANSACTION-AMOUNT.
           ACCEPT ACCOUNT-BALANCE.
           
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
           
           DISPLAY "Transaction: " TRANSACTION-TYPE.
           DISPLAY "Amount: " TRANSACTION-AMOUNT.
           DISPLAY "New Balance: " ACCOUNT-BALANCE.
           DISPLAY "Status: " ERROR-CODE.
           
           STOP RUN.
       
       UPDATE-ACCOUNT-RECORD.
           DISPLAY "Updating account record...".
           MOVE 'SUCCESS' TO ERROR-CODE.
       
       ROLLBACK-TRANSACTION.
           DISPLAY "Rolling back transaction...".`
  }
]

interface ExampleSnippetsProps {
  onSelectExample: (code: string) => void
}

function ExampleSnippets ({ onSelectExample }: ExampleSnippetsProps) {
  const [isOpen, setIsOpen] = useState(false)

  return (
    <div className='example-snippets'>
      <button className='toggle-btn' onClick={() => setIsOpen(!isOpen)}>
        {isOpen ? '▼' : '▶'} Example COBOL Programs
      </button>

      {isOpen && (
        <div className='examples-list'>
          {examples.map(example => (
            <div key={example.id} className='example-card'>
              <div className='example-header'>
                <h4>{example.name}</h4>
                <button
                  className='load-btn'
                  onClick={() => {
                    onSelectExample(example.code)
                    setIsOpen(false)
                  }}
                >
                  Load
                </button>
              </div>
              <p className='example-description'>{example.description}</p>
            </div>
          ))}
        </div>
      )}
    </div>
  )
}

export default ExampleSnippets

      *****************************************************************
      * Program     : ATM Simulator
      *
      * Description : A program is a tool that replicates the functions
      *               of an ATM.
      *
      * History     : Ghazi Dhouafli 15-JUN-2026 Original Version.
      *
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ATM-SIMULATOR.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-ACCOUNT-HOLDER    PIC X(20)   VALUE SPACES.
       01  WS-BALANCE           PIC 9(6)V99 VALUE 1000.00.
       01  WS-CHOICE            PIC X(1)    VALUE SPACES.
       01  WS-AMOUNT            PIC 9(6)V99 VALUE 0.00.

       01  WS-DISP-BALANCE      PIC $ZZZ,ZZ9.99.
       01  WS-DISP-AMOUNT       PIC $ZZZ,ZZ9.99.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           DISPLAY "======================================="
           DISPLAY "     AUTOMATED TELLER MACHINE          "
           DISPLAY "======================================="
           DISPLAY "Please enter your name: " WITH NO ADVANCING.
           ACCEPT WS-ACCOUNT-HOLDER.
           
           PERFORM PROCESS-MENU UNTIL WS-CHOICE = "4".
           
           DISPLAY " "
           DISPLAY "Thank you for banking with us, " 
                   FUNCTION TRIM(WS-ACCOUNT-HOLDER) "!".
           DISPLAY "Goodbye!".
           STOP RUN.
       PROCESS-MENU.
           DISPLAY " "
           DISPLAY "---------------------------------------"
           DISPLAY "               MENU                    "
           DISPLAY "---------------------------------------"
           DISPLAY "Account Holder: " FUNCTION TRIM(WS-ACCOUNT-HOLDER)
           MOVE WS-BALANCE TO WS-DISP-BALANCE
           DISPLAY "Current Balance: " WS-DISP-BALANCE
           DISPLAY "---------------------------------------"
           DISPLAY "1. Deposit Funds"
           DISPLAY "2. Withdraw Funds"
           DISPLAY "3. Balance Inquiry"
           DISPLAY "4. Exit"
           DISPLAY "---------------------------------------"
           DISPLAY "Enter option (1-4): " WITH NO ADVANCING.
           ACCEPT WS-CHOICE.
           
           EVALUATE WS-CHOICE
               WHEN "1" PERFORM DEPOSIT-PARA
               WHEN "2" PERFORM WITHDRAW-PARA
               WHEN "3" PERFORM INQUIRY-PARA
               WHEN "4" CONTINUE
               WHEN OTHER DISPLAY "Invalid option! Enter 1, 2, 3, or 4."
           END-EVALUATE.

       DEPOSIT-PARA.
           DISPLAY " "
           DISPLAY "--- DEPOSIT FUNDS ---"
           DISPLAY "Enter deposit amount: " WITH NO ADVANCING.
           ACCEPT WS-AMOUNT.
           
           IF WS-AMOUNT <= 0
              DISPLAY "Error: Deposit amount must be greater than zero."
           ELSE
               ADD WS-AMOUNT TO WS-BALANCE
               MOVE WS-AMOUNT TO WS-DISP-AMOUNT
               MOVE WS-BALANCE TO WS-DISP-BALANCE
               DISPLAY "Successfully deposited: " WS-DISP-AMOUNT
               DISPLAY "New Balance:            " WS-DISP-BALANCE
           END-IF.

       WITHDRAW-PARA.
           DISPLAY " "
           DISPLAY "--- WITHDRAW FUNDS ---"
           DISPLAY "Enter withdrawal amount: " WITH NO ADVANCING.
           ACCEPT WS-AMOUNT.
           
           IF WS-AMOUNT <= 0
               DISPLAY "Error:Withdrawal amount must be greater than 0."
           ELSE
               IF WS-AMOUNT > WS-BALANCE
                   DISPLAY "Error: Insufficient funds!"
                   MOVE WS-BALANCE TO WS-DISP-BALANCE
                   DISPLAY "Your balance is: " WS-DISP-BALANCE
               ELSE
                   SUBTRACT WS-AMOUNT FROM WS-BALANCE
                   MOVE WS-AMOUNT TO WS-DISP-AMOUNT
                   MOVE WS-BALANCE TO WS-DISP-BALANCE
                   DISPLAY "Successfully withdrew: " WS-DISP-AMOUNT
                   DISPLAY "New Balance:           " WS-DISP-BALANCE
               END-IF
           END-IF.

       INQUIRY-PARA.
           DISPLAY " "
           DISPLAY "--- BALANCE INQUIRY ---"
           MOVE WS-BALANCE TO WS-DISP-BALANCE
           DISPLAY "Account Holder:  " FUNCTION TRIM(WS-ACCOUNT-HOLDER)
           DISPLAY "Current Balance: " WS-DISP-BALANCE.

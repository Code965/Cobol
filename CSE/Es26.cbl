       IDENTIFICATION DIVISION.
       PROGRAM-ID. YOUR-PROGRAM-NAME.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       EXEC SQL CONNECT TO "MySQLODBCDataSource" USER mysqluser
           USING mysqlpassword
           END-EXEC.
       01 TABELLA.
           05 LISTA_UTENTI OCCURS 1 TO 100 TIMES DEPENDING ON SIZE-ARRAY
           INDEXED BY I.
               10 EMAIL PIC X(256).
               10 PASSWORD PIC X(25).
               10 NOME PIC A(30).
               10 COGNOME PIC A(30).
               10 NUMERO_TELEFONO PIC 9(10).
               10 LOG PIC 9(1) VALUES 0.

      *PER OGNI UTENTE ABBIAMO UN TOKEN RANDOM
               10 TOKEN_UTENTE PIC X(20).

       01 TABELLA3.
           05 LISTA_UTENTI_HISTORY OCCURS 1 TO 100 TIMES
           DEPENDING ON SIZE-ARRAY2
           INDEXED BY J.
               10 EMAIL_HISTORY PIC X(256).
               10 PASSWORD_HISTORY PIC X(25).
               10 NOME_HISTORY PIC A(30).
               10 COGNOME_HISTORY PIC A(30).
               10 NUMERO_TELEFONO_HISTORY PIC 9(10).


       01 SIZE-ARRAY USAGE IS INDEX.
       01 SIZE-ARRAY2 USAGE IS INDEX.
       01 SCELTA PIC 9(1).
       01 SCELTA2 PIC 9(1).
       01 TOKEN PIC 9(20).
       01 NOME-LOGIN PIC A(30).
       01 PASSWORD-LOGIN PIC X(25).


       PROCEDURE DIVISION.
       MAIN-PROCEDURE.

           MOVE 2 TO SIZE-ARRAY.
           MOVE 100 TO SIZE-ARRAY2.

           PERFORM UNTIL SCELTA2 = 9

                DISPLAY "COSA VUOI FARE?"
                   DISPLAY "1 LOGIN 2 REGISTRAZIONE 3 LOGUT 9 FINE"
                   ACCEPT SCELTA2


                  EVALUATE TRUE
                   WHEN SCELTA2 = 1
                       PERFORM LOGIN
                   WHEN SCELTA2 = 2
                       PERFORM REGISTRAZIONE
                   WHEN SCELTA2 = 3
                       PERFORM LOGOUT
                   WHEN OTHER
                       DISPLAY "FINE"
                       COMPUTE SCELTA = 9
                  END-EVALUATE

           END-PERFORM.


           STOP RUN.
      *------------------------------------ FUNCTION AA ----------------
       LOGIN.

           DISPLAY "NOME:".
           ACCEPT NOME-LOGIN.
           DISPLAY "PASSWORD-LOGIN:".
           ACCEPT PASSWORD-LOGIN.

           PERFORM VARYING I FROM 1 BY 1 UNTIL I > SIZE-ARRAY

               IF NOME-LOGIN = NOME(I) AND PASSWORD-LOGIN = PASSWORD(I)
                   THEN
                        DISPLAY "LOGIN VERIFICATO"
                        MOVE 1 TO LOG(I)
                        COMPUTE TOKEN = FUNCTION RANDOM * 20
                        MOVE TOKEN TO TOKEN_UTENTE(I)
                        PERFORM MODIFICA-UTENTE
               ELSE
                   DISPLAY "LOGIN NON VERIFICATO"
               END-IF

           END-PERFORM.

      *------------------------------------ FUNCTION AB ----------------
       MODIFICA-PASSWORD.

           PERFORM VARYING I FROM 1 BY 1 UNTIL I > SIZE-ARRAY

              IF TOKEN = TOKEN_UTENTE(I) THEN
                  MOVE PASSWORD(I) TO PASSWORD_HISTORY(I)
                  DISPLAY "INSERISCI UNA NUOVA PASSWORD"
                  ACCEPT PASSWORD(I)
               ELSE
                   DISPLAY "ERRORE TOKEN"

              END-IF

           END-PERFORM.

      *---------------------------------- FUNCTION AD ------------------
       MODIFICA-UTENTE.

           PERFORM VARYING I FROM 1 BY 1 UNTIL I > SIZE-ARRAY

              IF TOKEN = TOKEN_UTENTE(I) THEN

                   DISPLAY "COSA VUOI MODIFICARE?"
                   DISPLAY "1 - EMAIL 2- PASSWORD"
                   ACCEPT SCELTA

                   IF SCELTA = 1 THEN

                       MOVE EMAIL(I) TO EMAIL_HISTORY(I)
                       DISPLAY "INSERISCI LA NUOVA EMAIL"
                       ACCEPT EMAIL(I)
                   ELSE
                       IF SCELTA = 2 THEN
                           PERFORM MODIFICA-PASSWORD
                       END-IF
                   END-IF
               ELSE

                   DISPLAY "ERRORE TOKEN"

              END-IF


           END-PERFORM.

      *---------------------------------- FUNCTION AE ------------------
       REGISTRAZIONE.

           PERFORM VARYING I FROM 1 BY 1 UNTIL I > SIZE-ARRAY

               DISPLAY "EMAIL: "
               ACCEPT EMAIL(I)
               DISPLAY "PASSWORD:"
               ACCEPT PASSWORD(I)
               DISPLAY "NOME:"
               ACCEPT NOME(I)
               DISPLAY "COGNOME:"
               ACCEPT COGNOME(I)
               DISPLAY "NUMERO TELEFONO:"
               ACCEPT NUMERO_TELEFONO(I)

           END-PERFORM.

      *----------------------------------- FUNCTION AF -----------------
       LOGOUT.

           PERFORM VARYING I FROM 1 BY 1 UNTIL I > SIZE-ARRAY

               IF TOKEN_UTENTE(I) = TOKEN
                   THEN
                        DISPLAY "LOGIN VERIFICATO"
                        MOVE 0 TO LOG(I)
                        MOVE ZEROES TO TOKEN_UTENTE(I)
               ELSE
                   DISPLAY "LOGIN NON VERIFICATO"
               END-IF

           END-PERFORM.

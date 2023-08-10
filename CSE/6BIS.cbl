
       IDENTIFICATION DIVISION.
       PROGRAM-ID. YOUR-PROGRAM-NAME.
       DATA DIVISION.

       WORKING-STORAGE SECTION.


      *DIMENSIONE
       01 SIZE-VAL PIC 9(3) VALUES 3.

       01 TABELLA.
           05 ARRAY PIC 9(3) OCCURS 5 TIMES INDEXED BY I .

       01 TEMP PIC 9(3).
       01 NUMERO PIC 9(3).


       PROCEDURE DIVISION.
       MAIN-PROCEDURE.


           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 5

               DISPLAY "INSERISCI IL NUMERO: "
               ACCEPT NUMERO
               MOVE NUMERO TO ARRAY(I)
           END-PERFORM.




           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 5 - 1

               IF ARRAY(I) > ARRAY( I + 1 ) THEN

                   MOVE ARRAY(I) TO TEMP
                   MOVE ARRAY(I + 1) TO ARRAY(I)
                   MOVE TEMP TO ARRAY( I + 1)

               END-IF
           END-PERFORM.


           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 5
               DISPLAY "VAL: " ARRAY(I)
           END-PERFORM.


           STOP RUN.

      ******************************************************************
      *stampa istogrammi
      *per ogni valore inserito stampa un numero di instogrammi
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. YOUR-PROGRAM-NAME.
       DATA DIVISION.
       WORKING-STORAGE SECTION.

           01 TABELLA.
              05 ARRAY OCCURS 30 TIMES INDEXED BY I,J.
                 10 VALORE PIC 9(2).

           01 DATO PIC 9(2).
           01 DATO2 PIC 9(2).
           01 DATO3 PIC 9(2).
           01 DATO4 PIC 9(2).
           01 ELEMENTO PIC 9(2).


       PROCEDURE DIVISION.
       MAIN-PROCEDURE.

            PERFORM RIEMPI-ARRAY.
      *      PERFORM STAMPA-ARRAY.
            PERFORM INSTOGRAMMI.

            STOP RUN.

      *paragrafi/funzioni
       RIEMPI-ARRAY.
      *Inserimento valori
           PERFORM VARYING I FROM 1 BY 1 UNTIL I >10
               DISPLAY "INSERISCI IL VALORE: "
               ACCEPT DATO
               MOVE DATO TO ARRAY(I)
           END-PERFORM.

       STAMPA-ARRAY.
      *stampa i valori
           PERFORM VARYING I FROM 1 BY 1 UNTIL I >10
               DISPLAY "VALORE: " ARRAY(I)
           END-PERFORM.

       INSTOGRAMMI.

           SET I TO 0.
           SET J TO 0.

           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 10
               DISPLAY "ELEMENTO: " ARRAY(I)
               MOVE ARRAY(I) TO ELEMENTO
               PERFORM VARYING J FROM 1 BY 1 UNTIL J > ELEMENTO
               DISPLAY "*"
               END-PERFORM
               DISPLAY " "
           END-PERFORM.

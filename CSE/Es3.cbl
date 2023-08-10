      ******************************************************************
      * verificare se i valori in un array sono ordinati in modo
      * strettamente crescente
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
           01 DIMENSIONE PIC 9(2).


       PROCEDURE DIVISION.
       MAIN-PROCEDURE.

           PERFORM RIEMPI-ARRAY.
           PERFORM STAMPA-ARRAY.
           PERFORM ORDINAMENTO-CRESCENTE.


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

       ORDINAMENTO-CRESCENTE.
      *verifica se gli elementi sono tutti uguali

           SET I TO 1.
           SET J TO 2.
           SEARCH ARRAY AT END DISPLAY " NON UGUALI"
           WHEN ARRAY(I) < ARRAY(J) DISPLAY "CRESCENTE "
           WHEN ARRAY(I) > ARRAY(J) DISPLAY "DECRESCENTE"
           WHEN ARRAY(I) NOT = ARRAY(J)
           DISPLAY "NON CRESCENTE E NON DECRESCENTE "
           END-SEARCH.

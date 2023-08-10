      ******************************************************************
      * caricare un vettore di N posizioni, stampando la somma dei suoi
      * elementi
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. YOUR-PROGRAM-NAME.
       DATA DIVISION.

       WORKING-STORAGE SECTION.

       01 TABELLA.
           05 ARRAY PIC 9(3) OCCURS 1 TO 100 TIMES
           DEPENDING ON SIZE-ARRAY
           INDEXED BY I.

       01 SIZE-ARRAY USAGE IS INDEX.
       01 VAL PIC 9(3).
       01 SOMMA-ARRAY PIC 9(3).

       01 MEDIE.
           05 SOMMA-MEDIA-INTERNA PIC 9(3).
           05 SOMMA-MEDIA-ESTERNA PIC 9(3).
           05 MEDIA-ESTERNA-VAL PIC 9(3).
           05 MEDIA-INTERNA-VAL PIC 9(3).
           05 CONT-MEDIA-INTERNA PIC 9(3).
           05 CONT-MEDIA-ESTERNA PIC 9(3).

       01 VALORI.
           05 SX PIC 9(3).
           05 DX PIC 9(3).

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
              DISPLAY "INSERISCI LA LUNGHEZZA DEL TUO ARRAY".
              ACCEPT SIZE-ARRAY.

              PERFORM RIEMPI-ARRAY.
              PERFORM STAMPA-ARRAY.
              PERFORM SET-INTERVALLO.
              PERFORM MEDIA-INTERNA.
              PERFORM MEDIA-ESTERNA.

           STOP RUN.

      * ------------------------------------ FUNCION AA ----------------
       RIEMPI-ARRAY.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > SIZE-ARRAY
               DISPLAY "INSERISCI IL VALORE: "
               ACCEPT VAL
               MOVE VAL TO ARRAY(I)
           END-PERFORM.
      *------------------------------------ FUNCTION AB ----------------
       STAMPA-ARRAY.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > SIZE-ARRAY
               DISPLAY "VALORE: " ARRAY(I)
           END-PERFORM.
      *------------------------------------ FUNCTION AC ----------------
       MEDIA-INTERNA.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > SIZE-ARRAY

               IF I >= SX AND I <= DX THEN

               COMPUTE SOMMA-MEDIA-INTERNA =
               SOMMA-MEDIA-INTERNA + ARRAY(I)

               COMPUTE CONT-MEDIA-INTERNA = CONT-MEDIA-INTERNA + 1

               END-IF

           END-PERFORM.

           COMPUTE MEDIA-INTERNA-VAL = SOMMA-MEDIA-INTERNA /
                       CONT-MEDIA-INTERNA.
           DISPLAY "MEDIA INTERNA: " MEDIA-INTERNA-VAL.
      *------------------------------------ FUNCTION AD ----------------
       MEDIA-ESTERNA.

           PERFORM VARYING I FROM 1 BY 1 UNTIL I > SIZE-ARRAY
               IF I >= DX THEN

                   COMPUTE SOMMA-MEDIA-ESTERNA =
                   SOMMA-MEDIA-ESTERNA + ARRAY(I)

                   COMPUTE CONT-MEDIA-ESTERNA =
                           CONT-MEDIA-ESTERNA + 1
              END-IF

               IF I <= SX THEN

                   COMPUTE SOMMA-MEDIA-ESTERNA =
                   SOMMA-MEDIA-ESTERNA + ARRAY(I)

                   COMPUTE CONT-MEDIA-ESTERNA =
                           CONT-MEDIA-ESTERNA + 1
              END-IF

           END-PERFORM.

           COMPUTE MEDIA-ESTERNA-VAL = SOMMA-MEDIA-ESTERNA /
                       CONT-MEDIA-ESTERNA.

           DISPLAY "MEDIA ESTERNA: " MEDIA-ESTERNA-VAL.

      *------------------------------------ FUNCTION AE ----------------
       SET-INTERVALLO.

           DISPLAY "INSERISCI L'INTERVALLO SX".
           ACCEPT SX.
           DISPLAY "INSERISCI L'INTERVALLO DX".
           ACCEPT DX.

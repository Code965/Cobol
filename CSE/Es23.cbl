       IDENTIFICATION DIVISION.
       PROGRAM-ID. YOUR-PROGRAM-NAME.
       DATA DIVISION.
       WORKING-STORAGE SECTION.

         01 STUDENTI.
             05 ELENCO-STUDENTI OCCURS 1 TO 100 TIMES DEPENDING ON
             SIZE-ARRAY INDEXED BY I.
               10 NOME PIC A(20).
               10 CLASSE PIC X(3).
               10 VOTO PIC 9(2).

         01 SIZE-ARRAY USAGE IS INDEX.
         01 MAX-VOTO PIC 9(2).
         01 MIN-VOTO PIC 9(2).
         01 CLASSE-T PIC X(3).

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.

           DISPLAY "QUANTI STUDENTI CI SONO? "
           ACCEPT SIZE-ARRAY.
           PERFORM RIEMPI-ARRAY.
           PERFORM PRINT-ARRAY.
           PERFORM TROVA-CLASSE.

           STOP RUN.
      *------------------------------------- FUNCTION AA ---------------
       RIEMPI-ARRAY.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > SIZE-ARRAY
               DISPLAY "INSERISCI IL NOME: "
               ACCEPT NOME(I)
               DISPLAY "INSERISCI LA CLASSE: "
               ACCEPT CLASSE(I)
               DISPLAY "INSERISCI IL VOTO: "
               ACCEPT VOTO(I)
           END-PERFORM.
      *----------------------------------- FUNCTION AB -----------------
       PRINT-ARRAY.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > SIZE-ARRAY
               DISPLAY "NOME: " NOME(I)
                       "CLASSE: " CLASSE(I)
                       "VOTO: " VOTO(I)
           END-PERFORM.
      *--------------------------------- FUNCTION AC -------------------
       VOTO-BASSO.
           INITIALIZE MIN-VOTO.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > SIZE-ARRAY
               MOVE VOTO(1) TO MIN-VOTO
               IF MIN-VOTO > VOTO(I) THEN
                    MOVE VOTO(I) TO MIN-VOTO
               END-IF
           END-PERFORM.
           DISPLAY "VOTO-BASSO: " MIN-VOTO.
      *------------------------------ FUNCTION AD ----------------------
       VOTO-MAX.
           INITIALIZE MAX-VOTO.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > SIZE-ARRAY
               IF MAX-VOTO < VOTO(I) THEN
                     MOVE VOTO(I) TO MAX-VOTO
               END-IF
           END-PERFORM.
           DISPLAY "VOTO-MAX: " MAX-VOTO.
      *--------------------------------------- FUNCTION AE -------------
       TROVA-CLASSE.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > SIZE-ARRAY
               DISPLAY "PER QUALE CLASSE CERCHI I VOTI? "
               ACCEPT CLASSE-T
               IF CLASSE-T = CLASSE(I) THEN
                       PERFORM VOTO-BASSO
                       PERFORM VOTO-MAX
                END-IF
           END-PERFORM.
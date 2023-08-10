      ******************************************************************
      *Abbiamo un magazzino con un elenco di articoli:
      *descrizione, giacenza, prezzo acquisto e prezzo di vendita iva esclusa.

      *determinare
      *valorizzazione di magazzino
      *stampa listino alfabetico riportante descrizione, prezzo di vendita
      * senza iva e con iva al 20%
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. YOUR-PROGRAM-NAME.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01 MAGAZZINO.
           05 ELENCO-ARTICOLI OCCURS 1 TO 100 TIMES DEPENDING ON
              SIZE-ARRAY INDEXED BY I.
                10 DESCRIZIONE PIC X(200).
                10 GIACENZA PIC 9(4).
                10 PREZZO_ACQUISTO PIC S9(3)V9(2).
                10 PREZZO_VENDITA PIC S9(3)V9(2).
                10 PREZZO_VENDITA_IVA PIC S9(3)V9(2).
                10 PREZZO-DEFINITIVO PIC S9(3)V9(2).

       01 IVA-CALCOLATA PIC S9(3)V9(3).
       01 CALCOLO-PREZZO-INTERO PIC S9(3)V9(3).

       01 SIZE-ARRAY USAGE IS INDEX.



       PROCEDURE DIVISION.
       MAIN-PROCEDURE.

           DISPLAY "QUANTI ARTICOLI CI SONO NEL MAGAZZINO?".
           ACCEPT SIZE-ARRAY.

           PERFORM RIEMPI-MAGAZZINO.
           PERFORM IVA.
           PERFORM STAMPA-ELENCO-PRODOTTI.


           STOP RUN.

      *------------------------------------ FUNCTION AA ----------------
       RIEMPI-MAGAZZINO.

           PERFORM VARYING I FROM 1 BY 1 UNTIL I > SIZE-ARRAY
                  DISPLAY "INSERISCI LA DESCRIZIONE"
                  ACCEPT DESCRIZIONE(I)
                  DISPLAY "INSERISCI LA GIACENZA"
                  ACCEPT GIACENZA(I)
                  DISPLAY "INSERISCI IL PREZZO ACQUISTO"
                  ACCEPT PREZZO_ACQUISTO(I)
                  DISPLAY "INSERISCI IL PREZZO VENDITA"
                  ACCEPT PREZZO_VENDITA(I)

                  MOVE 0 TO PREZZO_VENDITA_IVA(I)
                  MOVE 0 TO PREZZO-DEFINITIVO(I)

           END-PERFORM.

      *------------------------------------ FUNCTION AB ----------------
       STAMPA-ELENCO-PRODOTTI.

            PERFORM VARYING I FROM 1 BY 1 UNTIL I > SIZE-ARRAY

                  DISPLAY "DESCRIZIONE: " DESCRIZIONE(I)
                  DISPLAY  "PREZZO_VENDITA: "  PREZZO_VENDITA(I)
                  DISPLAY  "PREZZO_VENDITA_IVA: " PREZZO_VENDITA_IVA(I)
                  DISPLAY  "PREZZO_DEFINITIVO: " PREZZO-DEFINITIVO(I)
           END-PERFORM.

      *------------------------------------ FUNCTION AC ----------------
      * VALORIZZAZIONE-MAGAZZINO.

      *------------------------------------ FUNCTION AD ----------------
       IVA.

           PERFORM VARYING I FROM 1 BY 1 UNTIL I > SIZE-ARRAY

               COMPUTE IVA-CALCOLATA = (PREZZO_VENDITA(I) * 22) / 100

               MOVE IVA-CALCOLATA TO PREZZO_VENDITA_IVA(I)

               COMPUTE CALCOLO-PREZZO-INTERO = PREZZO_VENDITA(I) +
                       IVA-CALCOLATA

               MOVE CALCOLO-PREZZO-INTERO TO PREZZO-DEFINITIVO(I)
           END-PERFORM.

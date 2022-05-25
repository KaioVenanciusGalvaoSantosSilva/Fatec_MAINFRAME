       IDENTIFICATION DIVISION.
       PROGRAM-ID. SMP029.
       AUTHOR. ANGELO LOTIERZO FILHO - REFERENCIA
      ***********************************************
      * CONVERCAO DO ARQUIVO CADCEP PARA TXT        *
      ***********************************************
      *----------------------------------------------------------------
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
                     DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CADCEP ASSIGN TO DISK
                    ORGANIZATION IS INDEXED
                    ACCESS MODE  IS DYNAMIC
                    RECORD KEY   IS CEP-NUMCEP
                    FILE STATUS  IS ST-ERRO
                    ALTERNATE RECORD KEY IS CEP-LOGRA
                                   WITH DUPLICATES.
           SELECT CADCEPTX ASSIGN TO DISK
                  ORGANIZATION IS LINE SEQUENTIAL
                  ACCESS MODE IS SEQUENTIAL
                  FILE STATUS  IS ST-ERRO.
      *
      *-----------------------------------------------------------------
       DATA DIVISION.
       FILE SECTION.
       FD CADCEP
               LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "CADCEP.DAT".
       01 REGCEP.
                03 CEP-NUMCEP        PIC 9(08).
                03 CEP-LOGRA         PIC X(30).
                03 CEP-BAIRRO        PIC X(20).
                03 CEP-CIDADE        PIC X(20).
                03 CEP-UF            PIC X(02).
      *
       FD CADCEPTX
               LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "CADCEPRR.TXT".
       01 REGCEPTX    PIC X(100).
      *-----------------------------------------------------------------
       WORKING-STORAGE SECTION.
       77 W-SEL      PIC 9(01) VALUE ZEROS.
       77 W-CONT     PIC 9(06) VALUE ZEROS.
       77 W-OPCAO    PIC X(01) VALUE SPACES.
       77 ST-ERRO    PIC X(02) VALUE "00". 
       77 W-ACT      PIC 9(02) VALUE ZEROS.
       77 MENS       PIC X(50) VALUE SPACES.
       77 LIMPA      PIC X(50) VALUE SPACES.
      *
       01 CEPTX.
                03 CEP-NUMCEPTX      PIC 9(08).
                03 FILLER            PIC X(01) VALUE " ".
                03 CEP-LOGRATX       PIC X(30).
                03 FILLER            PIC X(01) VALUE " ".
                03 CEP-BAIRROTX      PIC X(20).
                03 FILLER            PIC X(01) VALUE " ".
                03 CEP-CIDADETX      PIC X(20).
                03 FILLER            PIC X(01) VALUE " ".
                03 CEP-UFTX          PIC X(02).
       01 CAB0.
                03 FILLER      PIC X(20) VALUE "FATEC ZL".
                03 FILLER      PIC X(30) VALUE
                        "   *** RELACAO DE CEP ***".

       01 CAB1.
                03 CAB-NUMCEPTX      PIC X(08) VALUE "  CEP   ".
                03 FILLER            PIC X(01) VALUE " ".
                03 CAB-LOGRATX       PIC X(30) VALUE "   LOGRADOURO".
                03 FILLER            PIC X(01) VALUE " ".
                03 CAB-BAIRROTX      PIC X(20) VALUE "   BAIRRO".
                03 FILLER            PIC X(01) VALUE " ".
                03 CAB-CIDADETX      PIC X(20) VALUE "   CIDADE".
                03 FILLER            PIC X(01) VALUE " ".
                03 CAB-UFTX          PIC X(02) VALUE "UF".
       01 CAB2.
                03 CAB-NUMCEPTX      PIC X(08) VALUE "  ---   ".
                03 FILLER            PIC X(01) VALUE " ".
                03 CAB-LOGRATX       PIC X(30) VALUE "   ----------".
                03 FILLER            PIC X(01) VALUE " ".
                03 CAB-BAIRROTX      PIC X(20) VALUE "   ------".
                03 FILLER            PIC X(01) VALUE " ".
                03 CAB-CIDADETX      PIC X(20) VALUE "   ------".
                03 FILLER            PIC X(01) VALUE " ".
                03 CAB-UFTX          PIC X(02) VALUE "--".

      *-----------------------------------------------------------------
       SCREEN SECTION.
       01  SMT025.
           05  BLANK SCREEN.
           05  LINE 02  COLUMN 01 
               VALUE  "                *** CONVERSAO DE ARQUIVO".
           05  LINE 02  COLUMN 41 
               VALUE  " DE CEP PARA TXT ***".
           05  LINE 12  COLUMN 01 
               VALUE  "                         CONFIRMA CONVER".
           05  LINE 12  COLUMN 41 
               VALUE  "SAO (S ou N ) :".
           05  LINE 24  COLUMN 01 
               VALUE  " MENSAGEM :".
           05  TW-OPCAO
               LINE 12  COLUMN 57  PIC X(01)
               USING  W-OPCAO
               HIGHLIGHT.

      *-----------------------------------------------------------------
       PROCEDURE DIVISION.
       INICIO. 
      *
       INC-001.
                MOVE SPACES TO CEP-UF CEP-LOGRA CEP-BAIRRO.
                MOVE SPACES TO CEP-CIDADE. 
                MOVE ZEROS  TO CEP-NUMCEP.
                DISPLAY SMT025.
       INC-OPC.
                ACCEPT TW-OPCAO
                IF W-OPCAO = "N" OR "n"
                   MOVE "* DADOS RECUSADOS PELO OPERADOR *" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO ROT-FIM.
                IF W-OPCAO NOT = "S" AND "s"
                   MOVE "*** DIGITE APENAS S=SIM e N=NAO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-OPC.
      *
       INC-OP0.
           OPEN INPUT CADCEP
           IF ST-ERRO NOT = "00"
               IF ST-ERRO = "30"
                      MOVE "* ARQUIVO CEP NAO EXISTE *" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ROT-FIM
               ELSE
                      MOVE "ERRO NA ABERTURA DO ARQUIVO CEP" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ROT-FIM
           ELSE
                    NEXT SENTENCE.
      *
           OPEN OUTPUT CADCEPTX
           IF ST-ERRO NOT = "00"
                      MOVE "ERRO ABERTURA DO ARQUIVO CADCEPTX" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ROT-FIM.
           WRITE REGCEPTX FROM CAB0.
           MOVE SPACES TO REGCEPTX
           WRITE REGCEPTX.
           WRITE REGCEPTX FROM CAB1.
           WRITE REGCEPTX FROM CAB2.
           MOVE SPACES TO REGCEPTX
           WRITE REGCEPTX.
      *
       LER-CEP01.
                READ CADCEP NEXT
                IF ST-ERRO NOT = "00"
                   IF ST-ERRO = "10"
                      MOVE "*** FIM DO CADCEP ***" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ROT-FIM
                   ELSE
                      MOVE "ERRO NA LEITURA CADCEP"   TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ROT-FIM
                ELSE
                   NEXT SENTENCE.
       INC-003.
           MOVE CEP-NUMCEP TO CEP-NUMCEPTX.
           MOVE CEP-LOGRA  TO CEP-LOGRATX.
           MOVE CEP-BAIRRO TO CEP-BAIRROTX.
           MOVE CEP-CIDADE TO CEP-CIDADETX.
           MOVE CEP-UF     TO CEP-UFTX.
           MOVE CEPTX      TO REGCEPTX.
       INC-WR1.
                WRITE REGCEPTX
                IF ST-ERRO = "00" OR "02"
                      MOVE "*** DADOS GRAVADOS *** " TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO LER-CEP01
                ELSE
                      MOVE "ERRO NA GRAVACAO DO ARQUIVO CADCEPTX"
                                                       TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ROT-FIM.
      *
      **********************
      * ROTINA DE FIM      *
      **********************
      *
       ROT-FIM.
                DISPLAY (01, 01) ERASE
                CLOSE CADCEP CADCEPTX.
       ROT-FIMP.
                EXIT PROGRAM.

       ROT-FIMS.
                STOP RUN.
      *
      **********************
      * ROTINA DE MENSAGEM *
      **********************
      *
       ROT-MENS.
                MOVE ZEROS TO W-CONT.
       ROT-MENS1.
               DISPLAY (23, 12) MENS.
       ROT-MENS2.
                ADD 1 TO W-CONT
                IF W-CONT < 3000
                   GO TO ROT-MENS2
                ELSE
                   DISPLAY (23, 12) LIMPA.
       ROT-MENS-FIM.
                EXIT.
       FIM-ROT-TEMPO.

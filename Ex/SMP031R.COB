       IDENTIFICATION DIVISION.
       PROGRAM-ID. SMP031.
       AUTHOR. KAIO V G S S
      ***********************************************
      * CONVERCAO DO ARQUIVO CIDS PARA TXT        *
      ***********************************************
      *----------------------------------------------------------------
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
                     DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CADCID ASSIGN TO DISK
                    ORGANIZATION IS INDEXED
                    ACCESS MODE  IS DYNAMIC
                    RECORD KEY   IS CODIGO
                    FILE STATUS  IS ST-ERRO
                    ALTERNATE RECORD KEY IS DENOMINACAO
                                   WITH DUPLICATES.
           SELECT CADCIDTX ASSIGN TO DISK
                  ORGANIZATION IS LINE SEQUENTIAL
                  ACCESS MODE IS SEQUENTIAL
                  FILE STATUS  IS ST-ERRO.
      *
      *-----------------------------------------------------------------
       DATA DIVISION.
       FILE SECTION.
       FD CADCID
               LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "CADCID.DAT".
       01 REGCID.
          03 CODIGO			PIC 9(06).
          03 DENOMINACAO    PIC X(30).
          03 OBSER       	PIC X(30). 
		  03 DATACID.
 		 	05 NUMERODD 	PIC 9(02).
			05 NUMEROMM 	PIC 9(02).
			05 NUMEROAA 	PIC 9(04).
      *
       FD CADCIDTX
               LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "CADCIDRL.TXT".
       01 REGCIDTX    PIC X(100).
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
       01 MEDTX.
                03 CODIGOTX      PIC 9(08).
                03 FILLER        PIC X(01) VALUE " ".
                03 DENOMINACAOTX  PIC X(30).
                03 FILLER        PIC X(13) VALUE " ".
                03 OBSERTX       PIC X(30).
                03 FILLER        PIC X(01) VALUE " ".
		
       01 CAB0.
                03 FILLER      PIC X(20) VALUE "SISTEMA MEDICO".
                03 FILLER      PIC X(30) VALUE
                        "   *** RELACAO DE CIDS ***".

       01 CAB1.
                03 CAB-CODIGOTX      PIC X(12) VALUE "  CODIGO   ".
                03 FILLER            PIC X(08) VALUE "      ".
                03 CAB-DENOMINACAOTX PIC X(30) VALUE "DENOMINACAO".
                03 FILLER            PIC X(01) VALUE " ".
                03 CAB-OBSERTX      PIC X(10) VALUE "   OBS".
                03 FILLER            PIC X(01) VALUE " ".
				
       01 CAB2.
                03 CAB-CODIGOTX      PIC X(08) VALUE "--------".
                03 FILLER            PIC X(01) VALUE " ".
                03 CAB-DENOMINACAOTX       PIC X(10) VALUE "----------".
                03 FILLER            PIC X(10) VALUE "----------".
                03 FILLER            PIC X(10) VALUE "----------".		
                03 FILLER            PIC X(10) VALUE " ".					
                03 CAB-OBSERTX      PIC X(20) VALUE "   ------".
                03 FILLER            PIC X(01) VALUE " ".
			

      *-----------------------------------------------------------------
       SCREEN SECTION.
       01  SMT025.
           05  BLANK SCREEN.
           05  LINE 02  COLUMN 01 
               VALUE  "                *** CONVERSAO DE ARQUIVO".
           05  LINE 02  COLUMN 41 
               VALUE  " DE CIDS PARA TXT ***".
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
                MOVE SPACES TO  DENOMINACAO OBSER.
                MOVE ZEROS  TO CODIGO .
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
           OPEN INPUT CADCID
           IF ST-ERRO NOT = "00"
               IF ST-ERRO = "30"
                      MOVE "* ARQUIVO CID NAO EXISTE *" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ROT-FIM
               ELSE
                      MOVE "ERRO NA ABERTURA DO ARQUIVO CID" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ROT-FIM
           ELSE
                    NEXT SENTENCE.
      *
           OPEN OUTPUT CADCIDTX
           IF ST-ERRO NOT = "00"
                      MOVE "ERRO ABERTURA DO ARQUIVO CADCIDTX" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ROT-FIM.
           WRITE REGCIDTX FROM CAB0.
           MOVE SPACES TO REGCIDTX
           WRITE REGCIDTX.
           WRITE REGCIDTX FROM CAB1.
           WRITE REGCIDTX FROM CAB2.
           MOVE SPACES TO REGCIDTX
           WRITE REGCIDTX.
      *
       LER-CID01.
                READ CADCID NEXT
                IF ST-ERRO NOT = "00"
                   IF ST-ERRO = "10"
                      MOVE "*** FIM DO CADCID ***" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ROT-FIM
                   ELSE
                      MOVE "ERRO NA LEITURA CADCID"   TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ROT-FIM
                ELSE
                   NEXT SENTENCE.
       INC-003.
           MOVE CODIGO TO CODIGOTX.
           MOVE DENOMINACAO  TO DENOMINACAOTX.
           MOVE OBSER TO OBSERTX.
	   
           MOVE MEDTX      TO REGCIDTX.
       INC-WR1.
                WRITE REGCIDTX
                IF ST-ERRO = "00" OR "02"
                      MOVE "*** DADOS GRAVADOS *** " TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO LER-CID01
                ELSE
                      MOVE "ERRO NA GRAVACAO DO ARQUIVO CADCIDTX"
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
                CLOSE CADCID CADCIDTX.
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

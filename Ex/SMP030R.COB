       IDENTIFICATION DIVISION.
       PROGRAM-ID. SMP030.
       AUTHOR. KAIO V G S S
      ***********************************************
      * CONVERCAO DO ARQUIVO CADMED PARA TXT        *
      ***********************************************
      *----------------------------------------------------------------
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
                     DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CADMED ASSIGN TO DISK
                    ORGANIZATION IS INDEXED
                    ACCESS MODE  IS DYNAMIC
                    RECORD KEY   IS CRM
                    FILE STATUS  IS ST-ERRO
                    ALTERNATE RECORD KEY IS NOME
                                   WITH DUPLICATES.
           SELECT CADMEDTX ASSIGN TO DISK
                  ORGANIZATION IS LINE SEQUENTIAL
                  ACCESS MODE IS SEQUENTIAL
                  FILE STATUS  IS ST-ERRO.
      *
      *-----------------------------------------------------------------
       DATA DIVISION.
       FILE SECTION.
       FD CADMED
               LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "CADMED.DAT".
       01 REGMED.
				03 CRM			PIC 9(06).
				03 NOME         PIC X(30).
				03 ESPECIALIDADE  	PIC 9(02).
				03 SEXO         PIC X(01).
                03 EMAIL        PIC X(30).
      *
       FD CADMEDTX
               LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "CADMEDRL.TXT".
       01 REGMEDTX    PIC X(100).
      *-----------------------------------------------------------------
       WORKING-STORAGE SECTION.
       77 W-SEL      PIC 9(01) VALUE ZEROS.
       77 W-CONT     PIC 9(06) VALUE ZEROS.
       77 W-OPCAO    PIC X(01) VALUE SPACES.
       77 ST-ERRO    PIC X(02) VALUE "00". 
       77 W-ACT      PIC 9(02) VALUE ZEROS.
       77 MENS       PIC X(50) VALUE SPACES.
       77 LIMPA      PIC X(50) VALUE SPACES.
	   01 TXTESPECIALIDADE    	PIC X(30) VALUE SPACES.
       01 TEXSEXO     PIC X(20) VALUE SPACES.
      *
       01 MEDTX.
                03 CRMTX      		 PIC 9(06).
                03 FILLER            PIC X(01) VALUE " ".
                03 NOMETX     		 PIC X(30).
                03 FILLER            PIC X(01) VALUE " ".
                03 ESPECIALIDADETX   PIC X(02).
                03 FILLER            PIC X(01) VALUE " ".
                03 SEXOTX      		 PIC X(02).
                03 FILLER            PIC X(01) VALUE " ".
                03 EMAILTX           PIC X(30).
       01 CAB0.
                03 FILLER      PIC X(20) VALUE "SISTEMA MEDICO".
                03 FILLER      PIC X(30) VALUE
                        "   *** RELACAO DE MEDICO ***".

       01 CAB1.
                03 CAB-CRMTX     PIC X(06) VALUE "  CRM".
                03 FILLER            PIC X(03) VALUE " ".
                03 CAB-NOMETX       PIC X(30) VALUE "   MEDICO".
                03 FILLER            PIC X(01) VALUE " ".
                03 CAB-ESPECIALIDADETX PIC X(13) VALUE "ESPECIALIDADE".
                03 FILLER            PIC X(01) VALUE " ".
                03 CAB-SEXOTX      PIC X(04) VALUE "SEXO".
                03 FILLER            PIC X(01) VALUE " ".
                03 CAB-EMAILTX       PIC X(30) VALUE "EMAIL".
				
       01 CAB2.
                03 CAB-CRMTX      PIC X(08) VALUE "------".
                03 FILLER           PIC X(01) VALUE " ".
                03 CAB-NOMETX       PIC X(10) VALUE "----------".	
                03 FILLER           PIC X(10) VALUE "----------".			
                03 FILLER           PIC X(10) VALUE "----------".				
                03 FILLER            PIC X(01) VALUE " ".
                03 CAB-ESPECIALIDADETX      PIC X(07) VALUE "-------".
                03 FILLER           PIC X(06) VALUE "------".					
                03 FILLER            PIC X(01) VALUE " ".
                03 CAB-SEXOTX      PIC X(04) VALUE "----".
                03 FILLER            PIC X(01) VALUE " ".
                03 CAB-EMAILTX       PIC X(10) VALUE "----------".
                03 FILLER           PIC X(10) VALUE "----------".
                03 FILLER           PIC X(10) VALUE "----------".				
      *-----------------------------------------------------------------
       SCREEN SECTION.
       01  SMT025.
           05  BLANK SCREEN.
           05  LINE 02  COLUMN 01 
               VALUE  "                *** CONVERSAO DO ARQUIVO".
           05  LINE 02  COLUMN 41 
               VALUE  " DE MEDICO PARA TXT ***".
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
                MOVE SPACES TO NOME  SEXO EMAIL.
                MOVE ZEROS  TO CRM ESPECIALIDADE.
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
           OPEN INPUT CADMED
           IF ST-ERRO NOT = "00"
               IF ST-ERRO = "30"
                      MOVE "* ARQUIVO MED NAO EXISTE *" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ROT-FIM
               ELSE
                      MOVE "ERRO NA ABERTURA DO ARQUIVO MED" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ROT-FIM
           ELSE
                    NEXT SENTENCE.
      *
           OPEN OUTPUT CADMEDTX
           IF ST-ERRO NOT = "00"
                      MOVE "ERRO ABERTURA DO ARQUIVO CADMEDTX" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ROT-FIM.
           WRITE REGMEDTX FROM CAB0.
           MOVE SPACES TO REGMEDTX
           WRITE REGMEDTX.
           WRITE REGMEDTX FROM CAB1.
           WRITE REGMEDTX FROM CAB2.
           MOVE SPACES TO REGMEDTX
           WRITE REGMEDTX.
      *
       LER-CEP01.
                READ CADMED NEXT
                IF ST-ERRO NOT = "00"
                   IF ST-ERRO = "10"
                      MOVE "*** FIM DO CADMED ***" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ROT-FIM
                   ELSE
                      MOVE "ERRO NA LEITURA CADMED"   TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ROT-FIM
                ELSE
                   NEXT SENTENCE.
       INC-003.
           MOVE CRM TO CRMTX.
           MOVE NOME  TO NOMETX.
           MOVE ESPECIALIDADE TO ESPECIALIDADETX.
           MOVE SEXO TO SEXOTX.
           MOVE EMAIL    TO EMAILTX.
           MOVE MEDTX      TO REGMEDTX.
       INC-WR1.
                WRITE REGMEDTX
                IF ST-ERRO = "00" OR "02"
                      MOVE "*** DADOS GRAVADOS *** " TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO LER-CEP01
                ELSE
                      MOVE "ERRO NA GRAVACAO DO ARQUIVO CADMEDTX"
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
                CLOSE CADMED CADMEDTX.
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

	 
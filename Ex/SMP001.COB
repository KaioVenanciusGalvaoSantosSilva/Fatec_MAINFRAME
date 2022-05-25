       IDENTIFICATION DIVISION.
       PROGRAM-ID. CADMED.
      *AUTHOR. KAIO V G S S.
      **************************************
      * MANUTENCAO DO CADASTRO DE MEDICOS   *
      **************************************
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
                    ALTERNATE RECORD KEY IS NOME WITH DUPLICATES.
      *
      *-----------------------------------------------------------------
       DATA DIVISION.
       FILE SECTION.
       FD CADMED
               LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "CADMED.DAT". 
       01 REGMED.
          03 CRM			PIC 9(06).
          03 NOME          	PIC X(30).
		  03 ESPECIALIDADE  PIC 9(02).
          03 SEXO         	PIC X(01).
          03 DATANASCIMENTO.
			05 NUMERODD 	PIC 9(02).
			05 NUMEROMM 	PIC 9(02).
			05 NUMEROAA 	PIC 9(04).
          03 EMAIL       	PIC X(30).
          03 TELEFONE.
			05 DDD 			PIC 9(03).
			05 NUMEROTEL1 	PIC 9(05).
			05 NUMEROTEL2 	PIC 9(04).


          
      *Defini a data de nascimento dessa forma como algo A+.
      *-----------------------------------------------------------------
       WORKING-STORAGE SECTION.
       77 W-CONT        PIC 9(06) VALUE ZEROS.
       77 W-OPCAO      	PIC X(01) VALUE SPACES.
       77 W-ACT         PIC 9(02) VALUE ZEROS.
       77 MENS          PIC X(50) VALUE SPACES.
       01 ST-ERRO      	PIC X(02) VALUE "00".
       01 W-SEL             PIC 9(01) VALUE ZEROS.	   
       01 TXTESPECIALIDADE    	PIC X(30) VALUE SPACES.
       01 TEXSEXO     PIC X(20) VALUE SPACES.

       01 TABMEDICO.
          03 TBMEDICO    PIC X(20) OCCURS 9 TIMES.
      *-----------------------------------------------------------------
       SCREEN SECTION.
       01  TELA2.
           05  BLANK SCREEN.
           05  LINE 01  COLUMN 01 
               VALUE  "                            ".
           05  LINE 01  COLUMN 41 
               VALUE  " ".
           05  LINE 02  COLUMN 01 
               VALUE                            "  CADASTRO DE MEDICOS".
           05  LINE 03  COLUMN 01 
               VALUE  "                            ".
           05  LINE 03  COLUMN 41 
               VALUE  " ".
           05  LINE 04  COLUMN 01 
               VALUE  "  ".
           05  LINE 05  COLUMN 01 
               VALUE  "    CRM :                    NOME :".
           05  LINE 06  COLUMN 01 
               VALUE  "  ".
           05  LINE 08  COLUMN 01 
               VALUE  "    ESPECIALIDADE: ".
           05  LINE 11  COLUMN 01 
               VALUE  "    SEXO :                      ".
           05  LINE 15  COLUMN 01 
               VALUE  "    E-MAIL :  ".
           05  LINE 25  COLUMN 01 
               VALUE  " MENSAGEM :".
           05  TCRM
               LINE 05  COLUMN 15  PIC 9(06)
               USING  CRM
               HIGHLIGHT.
           05  TNOME
               LINE 05  COLUMN 43  PIC X(30)
               USING  NOME
               HIGHLIGHT.
           05  TESPECIALIDADE
               LINE 08  COLUMN 19  PIC 9(02)
               USING  ESPECIALIDADE
               HIGHLIGHT.
		   05  TTXTESPECIALIDADE
			   LINE 08  COLUMN 22  PIC X(30)
			   USING  TXTESPECIALIDADE
			   HIGHLIGHT.
           05  TSEXO
               LINE 11  COLUMN 14  PIC X(01)
               USING  SEXO
               HIGHLIGHT.
           05  TTEXSEXO
               LINE 11  COLUMN 16  PIC X(20)
               USING  TEXSEXO
               HIGHLIGHT.
           05  TDATANASCIMENTO
               LINE 13  COLUMN 22  PIC X(10)
               USING  DATANASCIMENTO
               HIGHLIGHT.
           05  LINE 13  COLUMN 01 
               VALUE  "    DATA NASCIMENTO :  /  /    ".			   
           05  TNUMERODD
               LINE 13  COLUMN 22  PIC 9(02)
               USING  NUMERODD.		
           05  TNUMEROMM
               LINE 13  COLUMN 25  PIC 9(02)
               USING  NUMEROMM.	
           05  TNUMEROAA
               LINE 13  COLUMN 28  PIC 9(04)
               USING  NUMEROAA.	 
           05  TEMAIL
               LINE 15  COLUMN 14  PIC X(30)
               USING  EMAIL
               HIGHLIGHT.
           05  TTELEFONE
               LINE 17  COLUMN 01  PIC X(12)
               USING  TELEFONE
               HIGHLIGHT.
           05  LINE 17  COLUMN 01 
               VALUE  "    TELEFONE :(DDD)XXXXX-XXXX    ".			   
           05  TDDD
               LINE 17  COLUMN 16  PIC 9(03)
               USING  DDD.		
           05  TNUMEROTEL1
               LINE 17  COLUMN 20  PIC 9(05)
               USING  NUMEROTEL1.				   
           05  TNUMEROTEL2
               LINE 17  COLUMN 26  PIC 9(04)
               USING  NUMEROTEL2.				   		   
      *
       01  TELATA.
           05  LINE 14  COLUMN 50 VALUE  "01-CLINICA MEDICA ".
           05  LINE 15  COLUMN 50 VALUE  "02-UROLOGIA ".
           05  LINE 16  COLUMN 50 VALUE  "03-GINICOLOGIA".
           05  LINE 17  COLUMN 50 
               VALUE  "04-PEDIATRIA ".
           05  LINE 18  COLUMN 50 
               VALUE  "05-CARDIOLOGIA".
           05  LINE 19  COLUMN 50 
               VALUE  "06-OBSTETRIA".
           05  LINE 20  COLUMN 50 
               VALUE  "07-CIRURGIA".
           05  LINE 21  COLUMN 50 
               VALUE  "08-DERMATOLOGIA".
           05  LINE 22  COLUMN 50 
               VALUE  "09-GERIATRIA". 

      *-----------------------------------------------------------------
       PROCEDURE DIVISION.
       INICIO.
           MOVE "CLINICA MEDICA " TO TBMEDICO(01)
           MOVE "UROLOGIA" TO TBMEDICO(02)
           MOVE "GINICOLOGISTA"    TO TBMEDICO(03)
           MOVE "PEDIATRIA"   TO TBMEDICO(04)
           MOVE "CARDIOLOGISTA"    TO TBMEDICO(05)
           MOVE "OBSTETRIA"   TO TBMEDICO(06)
           MOVE "CIRURGIA"    TO TBMEDICO(07)
           MOVE "DERMATOLOGIA"        TO TBMEDICO(08)
           MOVE "GERIATRIA"  TO TBMEDICO(09).
       
       R0.
           OPEN I-O CADMED
           IF ST-ERRO NOT = "00"  
              IF ST-ERRO = "30"
                 OPEN OUTPUT CADMED
                 CLOSE CADMED
                 MOVE "*** ARQUIVO CADMED FOI CRIADO **" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO R0
              ELSE
                 MOVE "ERRO NA ABERTURA DO ARQUIVO CADMED" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM
           ELSE
                 NEXT SENTENCE.
      *                
       R1.
           MOVE SPACES TO  NOME SEXO DATANASCIMENTO EMAIL
           MOVE SPACES TO  TEXSEXO TXTESPECIALIDADE 
           MOVE ZEROS TO ESPECIALIDADE CRM TELEFONE
		   MOVE ZEROS TO NUMERODD NUMEROMM NUMEROAA
		   MOVE ZEROS TO DDD NUMEROTEL1 NUMEROTEL2
           DISPLAY TELA2.
       R2.
		DISPLAY TELA2
		   ACCEPT TCRM
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO ROT-FIM.
       R2A.
		IF CRM = ZEROS
		   MOVE "*Insira o CRM*" TO MENS
		   PERFORM ROT-MENS THRU ROT-MENS-FIM
		  GO TO R2.				  
		
		DISPLAY TCRM.
		DISPLAY TELA2.				   
				   				   
       LER-CADMEDICO.
           READ CADMED
           IF ST-ERRO NOT = "23"
              IF ST-ERRO = "00"
			  
                PERFORM R5A
                PERFORM R6A
                PERFORM R7A
                PERFORM R8A
				
                DISPLAY TELA2
                MOVE "*** MEDICO JA CADASTRADO ***" TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ACE-001
             ELSE
                MOVE "ERRO NA LEITURA ARQUIVO MEDICO" TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ROT-FIM
           ELSE
                MOVE "*** MEDICO NAO CADASTRADO ***" TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM.

       R3.
		DISPLAY TELA2
           ACCEPT TNOME
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO R2.
		DISPLAY TELA2.
       R3A.		
		IF NOME = SPACES
               MOVE "*Insira um Nome*" TO MENS
			   PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO R3.		 
		DISPLAY TNOME.
		DISPLAY TELA2.		
		
       R4.	   
           DISPLAY TELATA
           ACCEPT TESPECIALIDADE.
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO R3.
		DISPLAY TELA2.
       R4A.
		DISPLAY TELA2
           IF ESPECIALIDADE = 0 OR > 09
                 MOVE "* DIGITE APENAS DE 01 ATE 09 *" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO R4.
           MOVE TBMEDICO(ESPECIALIDADE) TO TXTESPECIALIDADE
           DISPLAY TTXTESPECIALIDADE.
		DISPLAY TELA2.
		   
       R5.
		DISPLAY TELA2
           ACCEPT TSEXO
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO R4.
		DISPLAY TELA2.
        R5A.	
		 IF SEXO = "M" OR SEXO = "m"
              MOVE "MASCULINO" TO TEXSEXO
           ELSE
              IF SEXO = "F" OR SEXO = "f"
                 MOVE "FEMENINO" TO TEXSEXO
              ELSE
                 MOVE "*DIGITE M=MASCULINO    F=FEMININO*" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO R5.
           DISPLAY TTEXSEXO.
		DISPLAY TELA2.		
		 
       R6.	   
		DISPLAY TELA2

			ACCEPT TNUMERODD.
			ACCEPT TNUMEROMM.
			ACCEPT TNUMEROAA.
			ACCEPT W-ACT FROM ESCAPE KEY
			IF W-ACT = 01
		GO TO R5.
       R6A.
		IF NUMERODD=ZEROS OR NUMEROMM=ZEROS OR NUMEROAA=ZEROS
               MOVE "*Insira a DATA corretamente*" TO MENS
			   PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO R6.
		IF NUMERODD>31 OR NUMEROMM>12 OR NUMEROAA<0
               MOVE "*Insira a DATA corretamente2*" TO MENS
			   PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO R6.
		IF NUMEROMM=02 AND NUMERODD >29
		   MOVE "*Insira a DATA corretamente3*" TO MENS
		   PERFORM ROT-MENS THRU ROT-MENS-FIM
		  GO TO R6.
				
	
			DISPLAY TNUMERODD.
			DISPLAY TNUMEROMM.
			DISPLAY TNUMEROAA.
		DISPLAY TELA2.	

       R7.
		DISPLAY TELA2
           ACCEPT TEMAIL
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO R6.
		DISPLAY TELA2.
       R7A.		
		IF EMAIL = SPACES
               MOVE "*Insira um e-mail*" TO MENS
			   PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO R7.		 
		DISPLAY TEMAIL.
		DISPLAY TELA2.
		
       R8.
		DISPLAY TELA2
		   ACCEPT TDDD
		   ACCEPT TNUMEROTEL1
		   ACCEPT TNUMEROTEL2
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO R7.
       R8A.
		IF DDD=ZEROS OR NUMEROTEL1=ZEROS OR NUMEROTEL2=ZEROS
               MOVE "*Insira o telefone correto*" TO MENS
			   PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO R8.				  
		
		DISPLAY TDDD.
		DISPLAY TNUMEROTEL1.
		DISPLAY TNUMEROTEL2.		
		DISPLAY TTELEFONE.
		DISPLAY TELA2.
		

      * ------------- VERICAR SE E ALTERACAO -----------------
           IF W-SEL = 1 
                GO TO ALT-OPC.
       INC-OPC.
                MOVE "S" TO W-OPCAO
                DISPLAY (25, 40) "DADOS OK (S/N) : ".
                ACCEPT (25, 57) W-OPCAO WITH UPDATE
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 01 GO TO R8.
                IF W-OPCAO = "N" OR "n"
                   MOVE "*** DADOS RECUSADOS PELO OPERADOR ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R1.
                IF W-OPCAO NOT = "S" AND "s"
                   MOVE "*** DIGITE APENAS S=SIM e N=NAO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-OPC.
       INC-WR1.
                WRITE REGMED
                IF ST-ERRO = "00" OR "02"
                      MOVE "*** DADOS GRAVADOS *** " TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO R1.
                IF ST-ERRO = "22"
                  MOVE "*MEDICO JA EXISTE,DADOS NAO GRAVADOS*" TO MENS
                  PERFORM ROT-MENS THRU ROT-MENS-FIM
                  GO TO R1
                ELSE
                      MOVE "ERRO NA GRAVACAO DO ARQUIVO"
                                                       TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ROT-FIM.

      *
      *****************************************
      * ROTINA DE CONSULTA/ALTERACAO/EXCLUSAO *
      *****************************************
      *
       ACE-001.
                DISPLAY (25, 12)
                     "N=NOVO REGISTRO   A=ALTERAR   E=EXCLUIR"
                ACCEPT (25, 55) W-OPCAO
                IF W-OPCAO NOT = "N" AND W-OPCAO NOT = "A" 
                    AND W-OPCAO NOT = "E" GO TO ACE-001.
                MOVE SPACES TO MENS
                DISPLAY (25, 12) MENS
                IF W-OPCAO = "N" 
                   GO TO R1  
                ELSE
                   IF W-OPCAO = "A"
                      MOVE 1 TO W-SEL
                      GO TO R3.
      *
       EXC-OPC.
                DISPLAY (25, 40) "EXCLUIR   (S/N) : ".
                ACCEPT (25, 57) W-OPCAO
                IF W-OPCAO = "N" OR "n"
                   MOVE "*** REGISTRO NAO EXCLUIDO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R1.
                IF W-OPCAO NOT = "S" AND "s"
                   MOVE "* DIGITE APENAS S=SIM  e  N=NAO *" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO EXC-OPC.
       EXC-DL1.
                DELETE CADMED RECORD
                IF ST-ERRO = "00"
                   MOVE "*** REGISTRO MEDICO EXCLUIDO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R1.
                MOVE "ERRO NA EXCLUSAO DO REGISTRO "   TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ROT-FIM.
      *
       ALT-OPC.
                DISPLAY (25, 40) "ALTERAR  (S/N) : ".
                ACCEPT (25, 57) W-OPCAO
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 01 GO TO R7.
                IF W-OPCAO = "N" OR "n"
                   MOVE "*** INFORMACOES NAO ALTERADAS *** " TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R1.
                IF W-OPCAO NOT = "S" AND "s"
                   MOVE "*** DIGITE APENAS S=SIM  e  N=NAO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO ALT-OPC.
       ALT-RW1.
                REWRITE REGMED
                IF ST-ERRO = "00" OR "02"
                   MOVE "*** REGISTRO ALTERADO ***         " TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R1.
                MOVE "ERRO NA EXCLUSAO DO REGISTRO MEDICO"   TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ROT-FIM.
      *-------------------------------------------------------------------------------------------
       ROT-FIM.
           CLOSE CADMED.
           STOP RUN.

      *---------[ ROTINA DE MENSAGEM ]---------------------
       ROT-MENS.
                MOVE ZEROS TO W-CONT.
       ROT-MENS1.
               DISPLAY (25, 12) MENS.
       ROT-MENS2.
                ADD 1 TO W-CONT
                IF W-CONT < 3000
                   GO TO ROT-MENS2
                ELSE
                   MOVE SPACES TO MENS
                   DISPLAY (25, 12) MENS.
       ROT-MENS-FIM.
                EXIT.
       FIM-ROT-TEMPO.

      *    FILE STATUS
      *    00 = OPERAÇÃO REALIZADO COM SUCESSO
      *    10 = FIM DE ARQUIVO ( QUANDO EXECUTADO LEITURA SEQUENCIAL ) 
      *    22 = REGISTRO JÁ CADASTRADO
      *    23 = REGISTRO NÃO ENCONTRADO
      *    30 = ARQUIVO NÃO ENCONTRADO
      *    95 = ISAM NÃO CARREGADO
      

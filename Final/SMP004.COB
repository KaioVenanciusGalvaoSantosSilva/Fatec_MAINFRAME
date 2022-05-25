       IDENTIFICATION DIVISION.
       PROGRAM-ID. CADPACI.
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
			  SELECT CADPACI ASSIGN TO DISK
                    ORGANIZATION IS INDEXED
                    ACCESS MODE  IS DYNAMIC
                    RECORD KEY   IS CPF
					FILE STATUS  IS ST-ERRO
                    ALTERNATE RECORD KEY IS NOME WITH DUPLICATES.

					
			  SELECT CADCEP ASSIGN TO DISK
                    ORGANIZATION IS INDEXED
                    ACCESS MODE  IS DYNAMIC
                    RECORD KEY   IS CEP
					FILE STATUS  IS ST-ERRO
                    ALTERNATE RECORD KEY IS ENDERECO WITH DUPLICATES.

			  SELECT CADCONV ASSIGN TO DISK
                    ORGANIZATION IS INDEXED
                    ACCESS MODE  IS DYNAMIC
                    RECORD KEY   IS CODIGO
                    FILE STATUS  IS ST-ERRO
                    ALTERNATE RECORD KEY IS NOMEC
					ALTERNATE RECORD KEY IS PLANO.
		
      *
      *-----------------------------------------------------------------
       DATA DIVISION.
       FILE SECTION.
       FD CADPACI
               LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "CADPACI.DAT". 
       01 REGPACI.
          03 CPF			PIC 9(04).
          03 NOME          	PIC X(30).
          03 DATANASCIMENTO.
			05 NUMERODD 	PIC 9(02).
			05 NUMEROMM 	PIC 9(02).
			05 NUMEROAA 	PIC 9(04).
          03 SEXO         	PIC X(01).	
          03 GENERO        	PIC X(01).
		  
          03 CONVENIO		PIC 9(04).
          03 CEPC           PIC 9(08).
          03 NUMEROENDERECO PIC 9(04).		  
          03 COMPLEMENTO    PIC X(10).		  
          03 EMAIL       	PIC X(30).
          03 TELEFONE.
			05 DDD 			PIC 9(03).
			05 NUMEROTEL1 	PIC 9(05).
			05 NUMEROTEL2 	PIC 9(04).
			
			
       FD CADCEP
               LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "CADCEP.DAT". 
       01 REGCEP.
          03 CEP            PIC 9(08).
          03 ENDERECO       PIC X(30).
		  03 BAIRRO  		PIC X(20).
          03 CIDADE        	PIC X(20).
          03 ESTADO        	PIC X(02).
		  
		  
		  
       FD CADCONV
               LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "CADCONV.DAT". 
       01 REGCONV.
          03 CODIGO			PIC 9(06).
          03 NOMEC         	PIC X(30).
		  03 PLANO			PIC 9(02).
		  03 DATAVALID.
 		 	05 NUMDD 	    PIC 9(02).
			05 NUMMM 	    PIC 9(02).
			05 NUMAA 	    PIC 9(04).		  

      *
      *-----------------------------------------------------------------
       WORKING-STORAGE SECTION.
       77 W-CONT            PIC 9(06) VALUE ZEROS.
       77 W-OPCAO           PIC X(01) VALUE SPACES.
       77 W-ACT             PIC 9(02) VALUE ZEROS.
       77 MENS              PIC X(50) VALUE SPACES.
       77 LIMPA             PIC X(50) VALUE SPACES. 
       01 ST-ERRO        	PIC X(02) VALUE "00".
       01 W-SEL             PIC 9(01) VALUE ZEROS.
       01 TXTTPAMIGO    	PIC X(10) VALUE SPACES.
       01 IND               PIC 9(02) VALUE ZEROS.
       01 TEXSEXO       	PIC X(20) VALUE SPACES.
	   01 TXTPLANO    	    PIC X(30) VALUE SPACES.

	   01 TABCONVENIO.
          03 TBCONVENIO    PIC X(20) OCCURS 12 TIMES.	   
      
	  01 TABGENEROX.
          03 FILLER     PIC X(15) VALUE "HHETEROSEXUAL".
          03 FILLER     PIC X(15) VALUE "SHOMESEXUAL".
          03 FILLER     PIC X(15) VALUE "BBISSESUXUAL".
          03 FILLER     PIC X(15) VALUE "PPANSEXUAL".
          03 FILLER     PIC X(15) VALUE "AASSEXUAL".
          03 FILLER     PIC X(15) VALUE "TTRANSEXUAL".
          03 FILLER     PIC X(15) VALUE "CCISGENERO".
          03 FILLER     PIC X(15) VALUE "ZNAO BINARIO".
          03 FILLER     PIC X(15) VALUE "NNAO DECLARADO".
          03 FILLER     PIC X(15) VALUE "GALUNO GAMER".
          03 FILLER     PIC X(15) VALUE "VPROFESSOR".		  
          03 FILLER     PIC X(15) VALUE "OOUTROS".		  
       01 TABGENERO REDEFINES TABGENEROX.
          03 TBGENERO   PIC X(15) OCCURS 12 TIMES.
       01 TXTGENERO.
          03 TXTGENERO1 PIC X(01) VALUE SPACES.
          03 TXTGENERO2 PIC X(14) VALUE SPACES. 


      *-----------------------------------------------------------------
       SCREEN SECTION.
       01  TELA2.
           05  BLANK SCREEN.
           05  LINE 01  COLUMN 01 
               VALUE  "                            ".
           05  LINE 01  COLUMN 41 
               VALUE  " ".
           05  LINE 02  COLUMN 01 
               VALUE                         "  CADASTRO DE PACIENTES".

           05  LINE 03  COLUMN 01 
               VALUE  "    -------------------------------------------".
			   
           05  LINE 04  COLUMN 01 
               VALUE  "    CPF :                    ".
           05  LINE 05  COLUMN 01 
               VALUE  "    NOME :                    ".			   
           05  LINE 07  COLUMN 01 
               VALUE  "    SEXO: " .
           05  LINE 07  COLUMN 20 
               VALUE  "    GENERO : " .
           05  LINE 08  COLUMN 01 
               VALUE  "    -------------------------------------------".			   
           05  LINE 09  COLUMN 01 
               VALUE  "    CONVENIO : ".

           05  LINE 19  COLUMN 01 
               VALUE  "    NUM. ENDERECO :  ".
           05  LINE 20 COLUMN 01 
               VALUE  "    COMPLEMENTO :  ".
           05  LINE 22 COLUMN 01 
               VALUE  "    E-MAIL :  ". 
           05  LINE 25  COLUMN 01 
               VALUE  " MENSAGEM :".
           05  TCPF
               LINE 04  COLUMN 15  PIC 9(04)
               USING  CPF
               HIGHLIGHT.
           05  TNOME
               LINE 05  COLUMN 13  PIC X(30)
               USING  NOME
               HIGHLIGHT.
           05  TGENERO
               LINE 07  COLUMN 33  PIC X(01)
               USING  GENERO
               HIGHLIGHT.
		   05  TTXTGENERO2
			   LINE 07  COLUMN 35  PIC X(14)
			   USING  TXTGENERO2
			   HIGHLIGHT.
           05  TSEXO
               LINE 07  COLUMN 11  PIC X(01)
               USING  SEXO
               HIGHLIGHT.
           05  TTEXSEXO
               LINE 07  COLUMN 14  PIC X(10)
               USING  TEXSEXO
               HIGHLIGHT.
           05  TDATANASCIMENTO
               LINE 06  COLUMN 22  PIC X(10)
               USING  DATANASCIMENTO
               HIGHLIGHT.
           05  LINE 06  COLUMN 01 
               VALUE  "    DATA NASCIMENTO :  /  /    ".			   
           05  TNUMERODD
               LINE 06  COLUMN 22  PIC 9(02)
               USING  NUMERODD.		
           05  TNUMEROMM
               LINE 06  COLUMN 25  PIC 9(02)
               USING  NUMEROMM.	
           05  TNUMEROAA
               LINE 06  COLUMN 28  PIC 9(04)
               USING  NUMEROAA.	
           05  TCONVENIO
               LINE 09  COLUMN 16  PIC 9(04)
               USING  CONVENIO
               HIGHLIGHT.	

           05  LINE 10  COLUMN 01 
               VALUE  "    NOME DO CONVENIO: ".
           05  TNOMEC
               LINE 10  COLUMN 24  PIC X(30)
               USING  NOMEC
               HIGHLIGHT.

           05  LINE 11  COLUMN 01 
               VALUE  "    PLANO : ".
           05  TPLANO
               LINE 11  COLUMN 15 PIC 9(02)
               USING  PLANO
               HIGHLIGHT.
           05  TTXTPLANO
               LINE 11  COLUMN 20 PIC X(30)
               USING  TXTPLANO
               HIGHLIGHT.			   
,
           05  TDATAVALID
               LINE 12  COLUMN 22  PIC X(10)
               USING  DATAVALID
               HIGHLIGHT.
           05  LINE 12  COLUMN 01 
               VALUE  "    DATA VALIDADE :    /  /    ".			   
           05  TNUMDD
               LINE 12  COLUMN 22  PIC 9(02)
               USING  NUMDD.		
           05  TNUMMM
               LINE 12  COLUMN 25  PIC 9(02)
               USING  NUMMM.	
           05  TNUMAA
               LINE 12  COLUMN 28  PIC 9(04)
               USING  NUMAA.	
  

           05  LINE 13  COLUMN 01 
               VALUE  "    -------------------------------------------".
			   
           05  TCEPC
               LINE 14  COLUMN 11  PIC 99999.999
               USING  CEPC
               HIGHLIGHT.		   
           05  LINE 14  COLUMN 01 
               VALUE  "    CEP : ".	
           05  LINE 15  COLUMN 01 
               VALUE  "    ENDERECO : ".
           05  TENDERECO
               LINE 15  COLUMN 16  PIC X(30)
               USING  ENDERECO
               HIGHLIGHT.				   
           05  LINE 16  COLUMN 01 
               VALUE  "    BAIRRO   : ".
           05  TBAIRRO
               LINE 16  COLUMN 16  PIC X(20)
               USING  BAIRRO
               HIGHLIGHT.				   
           05  LINE 17  COLUMN 01 
               VALUE  "    CIDADE   : ".
           05  TCIDADE
               LINE 17  COLUMN 16  PIC X(20)
               USING  CIDADE
               HIGHLIGHT.				   
           05  LINE 18  COLUMN 01 
               VALUE  "    ESTADO   : ".
           05  TESTADO
               LINE 18  COLUMN 16  PIC X(02)
               USING  ESTADO
               HIGHLIGHT.	
   
           05  TNUMEROENDERECO
               LINE 19  COLUMN 20  PIC 9(04)
               USING  NUMEROENDERECO
               HIGHLIGHT.			 
           05  TCOMPLEMENTO
               LINE 20 COLUMN 20  PIC X(10)
               USING  COMPLEMENTO
               HIGHLIGHT.
			   
           05  LINE 21  COLUMN 01 
               VALUE  "    -------------------------------------------".
			   			   
			   
           05  TEMAIL
               LINE 22  COLUMN 14  PIC X(30)
               USING  EMAIL
               HIGHLIGHT.
           05  TTELEFONE
               LINE 23  COLUMN 01  PIC X(12)
               USING  TELEFONE
               HIGHLIGHT.
           05  LINE 23  COLUMN 01 
               VALUE  "    TELEFONE :(DDD)XXXXX-XXXX    ".			   
           05  TDDD
               LINE 23  COLUMN 16  PIC 9(03)
               USING  DDD.		
           05  TNUMEROTEL1
               LINE 23  COLUMN 20  PIC 9(05)
               USING  NUMEROTEL1.				   
           05  TNUMEROTEL2
               LINE 23  COLUMN 26  PIC 9(04)
               USING  NUMEROTEL2.

           05  LINE 24  COLUMN 01 
               VALUE  "    -------------------------------------------".
			   					   
      *
          01  TELATA.
           05  LINE 05  COLUMN 50 VALUE  "H-HETEROSEXUAL".
           05  LINE 06  COLUMN 50 VALUE  "S-HOMESEXUAL".
           05  LINE 07  COLUMN 50 VALUE  "B-BISSESUXUAL".
           05  LINE 08  COLUMN 50 VALUE  "P-PANSEXUAL".
           05  LINE 09  COLUMN 50 VALUE  "A-AASSEXUAL".
           05  LINE 10  COLUMN 50 VALUE  "C-CISGENERO".
           05  LINE 11  COLUMN 50 VALUE  "T-TRANSEXUAL".
           05  LINE 12  COLUMN 50 VALUE  "Z-NAO BINARIO".
           05  LINE 13  COLUMN 50 VALUE  "N-NAO DECLARADO".
           05  LINE 14  COLUMN 50 VALUE  "G-ALUNO GAMER".
           05  LINE 15  COLUMN 50 VALUE  "V-PROFESSOR".			   
           05  LINE 16  COLUMN 50 VALUE  "O-OUTROS".
		   
		   
		  01  TELATA2.
           05  LINE 05  COLUMN 50 
               VALUE  "01-ENFERMARIA REGINAL ".
           05  LINE 06  COLUMN 50 
               VALUE  "02-ENFERMARIA NACIONAL".
           05  LINE 07  COLUMN 50 
               VALUE  "03-ENFERMARIA INTERNACIONAL".
           05  LINE 08  COLUMN 50 
               VALUE  "04-APTO PADRAO REGINAL ".
           05  LINE 09  COLUMN 50 
               VALUE  "05-APTO PADRAO NACIONAL".
           05  LINE 10  COLUMN 50 
               VALUE  "06-APTO PADRAO INTERNACIONAL".
           05  LINE 11  COLUMN 50 
               VALUE  "07-EMERGENCIA REGIONAL".
           05  LINE 12  COLUMN 50 
               VALUE  "08-EMERGENCIA NACIONAL".
           05  LINE 13  COLUMN 50 
               VALUE  "09-EMERCENCIA INTERNACIONAL".
           05  LINE 14  COLUMN 50 
               VALUE  "10-PLANO GLOBAL ".			   
           05  LINE 15  COLUMN 50 
               VALUE  "11-INTERPLANETARIO".
           05  LINE 16  COLUMN 50 
               VALUE  "12-DEEPWEB".	
      *-----------------------------------------------------------------
       PROCEDURE DIVISION.
       INICIO.
           MOVE "ENFERMARIA REGINAL" TO TBCONVENIO(01)
           MOVE "ENFERMARIA NACIONAL" TO TBCONVENIO(02)
           MOVE "ENFERMARIA INTERNACIONAL"    TO TBCONVENIO(03)
           MOVE "APTO PADRÃO REGINAL"   TO TBCONVENIO(04)
           MOVE "APTO PADRAO NACIONAL"   TO TBCONVENIO(05)
           MOVE "APTO PADRAO INTERNACIONAL"   TO TBCONVENIO(06)
           MOVE "EMERGENCIA REGIONAL"    TO TBCONVENIO(07)
           MOVE "EMERGENCIA NACIONAL"        TO TBCONVENIO(08)
           MOVE "EMERCENCIA INTERNACIONAL"  TO TBCONVENIO(09).
		   MOVE "PLANO GLOBAL"  TO TBCONVENIO(10).
		   MOVE "INTERPLANETARIO"  TO TBCONVENIO(11).
		   MOVE "DEEPWEB"  TO TBCONVENIO(12).
 
       R0.
           OPEN I-O CADPACI
           IF ST-ERRO NOT = "00"  
              IF ST-ERRO = "30"
                 OPEN OUTPUT CADPACI
                 CLOSE CADPACI
                 MOVE "*** ARQUIVO CADPACI FOI CRIADO **" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO R0
              ELSE
                 MOVE "ERRO NA ABERTURA DO ARQUIVO CADPACI" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM
           ELSE
                 NEXT SENTENCE.

       R0A.
		   OPEN INPUT CADCEP
		   IF ST-ERRO NOT = "00"  
              IF ST-ERRO = "30"
                 MOVE "*** ARQUIVO DE CEP NAO ENCONTRADO **" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM
              ELSE
                 MOVE "ERRO NA ABERTURA DO ARQUIVO DE CEP " TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM
			ELSE
                 NEXT SENTENCE.	


       R0B.
		   OPEN INPUT CADCONV
		   IF ST-ERRO NOT = "00"  
              IF ST-ERRO = "30"
                 MOVE "*ARQUIVO DE CONVENIO NAO ENCONTRADO*" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM
              ELSE
                 MOVE "ERRO NA ABERTURA DO ARQUIVO CADCONV" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM
			ELSE
                 NEXT SENTENCE.	
		
				 
      *                
       R1.
           MOVE ZEROS TO  CPF  NUMERODD NUMEROMM NUMEROAA	 
           MOVE ZEROS TO CONVENIO CEPC NUMEROENDERECO 		   
           MOVE ZEROS TO DDD NUMEROTEL1 NUMEROTEL2		   	   
		   MOVE ZEROS TO DATANASCIMENTO TELEFONE 
           MOVE SPACES TO  NOME SEXO GENERO COMPLEMENTO  
           MOVE SPACES TO EMAIL TXTTPAMIGO TEXSEXO
		   MOVE SPACES TO  ENDERECO BAIRRO CIDADE ESTADO
           MOVE SPACES TO  NOMEC DATAVALID TXTPLANO 
           MOVE ZEROS TO CODIGO PLANO 
		   MOVE ZEROS TO NUMDD NUMMM NUMAA		   

           DISPLAY TELA2.
		   
       R2.
		DISPLAY TELA2
		   ACCEPT TCPF
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO ROT-FIM.
       R2A.
		IF CPF = ZEROS
		   MOVE "*Insira o CPF*" TO MENS
		   PERFORM ROT-MENS THRU ROT-MENS-FIM
		  GO TO R2.				  
		
		DISPLAY TCPF.
		DISPLAY TELA2.				   
				   				   
       LER-CADPACIENTE.
           READ CADPACI
           IF ST-ERRO NOT = "23"
              IF ST-ERRO = "00"
			  
                PERFORM R5A
                PERFORM R6A
                PERFORM R7A
                PERFORM R8A
				
                DISPLAY TELA2
                MOVE "*** PACIENTE JA CADASTRADO ***" TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ACE-001
             ELSE
                MOVE "ERRO NA LEITURA ARQUIVO PACIENTE" TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ROT-FIM
           ELSE
                MOVE "*** PACIENTE NAO CADASTRADO ***" TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM.

       R3.
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
		DISPLAY TELA2

			ACCEPT TNUMERODD.
			ACCEPT TNUMEROMM.
			ACCEPT TNUMEROAA.
			ACCEPT W-ACT FROM ESCAPE KEY
			IF W-ACT = 01
		GO TO R3.
       R4A.
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
		  GO TO R4.				
	
			DISPLAY TNUMERODD.
			DISPLAY TNUMEROMM.
			DISPLAY TNUMEROAA.
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
		DISPLAY TELATA
		   DISPLAY TELATA
           MOVE 1 TO IND
           ACCEPT TGENERO.
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO R5.
       R6A.
           MOVE TBGENERO(IND) TO TXTGENERO
           IF TXTGENERO1 NOT = GENERO
              ADD 1 TO IND
              IF IND < 13
                 GO TO R6A
              ELSE
                 MOVE "*** TIPO GENERO INCORRETO***" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO R6
           ELSE
               DISPLAY TTXTGENERO2.

       R7.
		DISPLAY TELA2
           ACCEPT TCONVENIO
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   DISPLAY TELA2
				   
                   GO TO R6.
           IF CONVENIO = 0 
                 MOVE "*** CONVENIO NAO INFORMADO  ***" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO R7.
       R7A.
           MOVE CONVENIO TO CODIGO.		   
           READ CADCONV
           IF ST-ERRO NOT = "00"
               IF ST-ERRO = "23"
                   MOVE "*CONVENIO DIGITADO NAO ENCONTRADO*" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R7
               ELSE
                  MOVE "ERRO NA LEITURA ARQUIVO CADCONV" TO MENS
                  PERFORM ROT-MENS THRU ROT-MENS-FIM
                  GO TO ROT-FIM
           ELSE
				MOVE TBCONVENIO(PLANO) TO TXTPLANO.
				DISPLAY TTXTPLANO.
                DISPLAY TELA2.


       R8.
           ACCEPT TCEPC
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   DISPLAY TELA2
                   GO TO R7.
           IF CEPC = 0 
                 MOVE "*** CEP NAO INFORMADO  ***" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO R8.
       R8A.
           MOVE CEPC TO CEP.
           READ CADCEP
           IF ST-ERRO NOT = "00"
               IF ST-ERRO = "23"
                   MOVE "*** CEP DIGITADO NAO ENCONTRADO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R8
               ELSE
                  MOVE "ERRO NA LEITURA ARQUIVO CADASTRO CEP" TO MENS
                  PERFORM ROT-MENS THRU ROT-MENS-FIM
                  GO TO ROT-FIM
           ELSE
                DISPLAY TELA2.

       R9.			   
		DISPLAY TELA2
		   ACCEPT TNUMEROENDERECO
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO R8.
       R9A.
		IF NUMEROENDERECO = ZEROS
		   MOVE "*Insira o NUM. ENDERECO*" TO MENS
		   PERFORM ROT-MENS THRU ROT-MENS-FIM
		  GO TO R9.				  
		
		DISPLAY TNUMEROENDERECO.
		DISPLAY TELA2.
   
			   	   
       R10.
			DISPLAY TELA2
			   ACCEPT TCOMPLEMENTO
			   ACCEPT W-ACT FROM ESCAPE KEY
			   IF W-ACT = 01
					   GO TO R9.
			DISPLAY TELA2.
       R10A.		
		IF COMPLEMENTO = SPACES
			   MOVE "*Insira um COMPLEMENTO*" TO MENS
			   PERFORM ROT-MENS THRU ROT-MENS-FIM
			  GO TO R10.		 
		DISPLAY TCOMPLEMENTO.
		DISPLAY TELA2.

       R11.
			DISPLAY TELA2
			   ACCEPT TEMAIL
			   ACCEPT W-ACT FROM ESCAPE KEY
			   IF W-ACT = 01
					   GO TO R10.
			DISPLAY TELA2.
       R11A.		
			IF EMAIL = SPACES
				   MOVE "*Insira um e-mail*" TO MENS
				   PERFORM ROT-MENS THRU ROT-MENS-FIM
				  GO TO R11.		 
			DISPLAY TEMAIL.
			DISPLAY TELA2.
			
       R12.
			DISPLAY TELA2
		   ACCEPT TDDD
		   ACCEPT TNUMEROTEL1
		   ACCEPT TNUMEROTEL2
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO R11.
				   
       R12A.
		IF DDD=ZEROS OR NUMEROTEL1=ZEROS OR NUMEROTEL2=ZEROS
		   MOVE "*Insira o telefone correto*" TO MENS
		   PERFORM ROT-MENS THRU ROT-MENS-FIM
		  GO TO R12.				  
		
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
                IF W-ACT = 01 GO TO R12.
                IF W-OPCAO = "N" OR "n"
                   MOVE "*** DADOS RECUSADOS PELO OPERADOR ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R1.
                IF W-OPCAO NOT = "S" AND "s"
                   MOVE "*** DIGITE APENAS S=SIM e N=NAO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-OPC.
       INC-WR1.
                WRITE REGPACI
                IF ST-ERRO = "00" OR "02"
                      MOVE "*** DADOS GRAVADOS *** " TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO R1.
                IF ST-ERRO = "22"
                  MOVE "*PACIENTE JA EXISTE,DADOS NAO GRAVADOS*" TO MENS
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
                DELETE CADPACI RECORD
                IF ST-ERRO = "00"
                   MOVE "*** REGISTRO PACIENTE EXCLUIDO ***" TO MENS
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
                IF W-ACT = 01 GO TO R12.
                IF W-OPCAO = "N" OR "n"
                   MOVE "*** INFORMACOES NAO ALTERADAS *** " TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R1.
                IF W-OPCAO NOT = "S" AND "s"
                   MOVE "*** DIGITE APENAS S=SIM  e  N=NAO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO ALT-OPC.
       ALT-RW1.
                REWRITE REGPACI
                IF ST-ERRO = "00" OR "02"
                   MOVE "*** REGISTRO ALTERADO ***         " TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R1.
                MOVE "ERRO NA EXCLUSAO DO REGISTRO PACIENTE"   TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ROT-FIM.
      *-------------------------------------------------------------------------------------------
       ROT-FIM.
           CLOSE CADCEP CADCONV.
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
      


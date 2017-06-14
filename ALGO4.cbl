       IDENTIFICATION DIVISION.
       PROGRAM-ID. ALGO4.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT CONS1 ASSIGN TO DISK
                                ORGANIZATION IS LINE SEQUENTIAL
                                FILE STATUS IS FS-CONS1.
           SELECT CONS2 ASSIGN TO DISK
                                ORGANIZATION IS LINE SEQUENTIAL
                                FILE STATUS IS FS-CONS2.	   
           SELECT CONS3 ASSIGN TO DISK
                                ORGANIZATION IS LINE SEQUENTIAL
                                FILE STATUS IS FS-CONS3.
           SELECT CUENTAS ASSIGN TO DISK
                                ORGANIZATION IS LINE SEQUENTIAL
                                FILE STATUS IS FS-CTAS.
           SELECT ESTADOS ASSIGN TO DISK
                                ORGANIZATION IS LINE SEQUENTIAL
                                FILE STATUS IS FS-EST.
           SELECT MAESTRO ASSIGN TO DISK
                                ORGANIZATION IS LINE SEQUENTIAL
                                FILE STATUS IS FS-MAE.
           SELECT LISTADO ASSIGN TO DISK
                                ORGANIZATION IS LINE SEQUENTIAL
                                FILE STATUS IS FS-LIST.
           SELECT ESTADIST ASSIGN TO DISK
                                ORGANIZATION IS LINE SEQUENTIAL
                                FILE STATUS IS FS-ESTAD.
       DATA DIVISION.
       FILE SECTION.
       FD CONS1 	LABEL RECORD IS STANDARD
                        VALUE OF FILE-ID IS 
                        'cons1.dat'.

       01 REG-CONS1.	
           03 REG-CONS1-CUIT-CONS          PIC 9(15).
           03 REG-CONS1-FECHA-ALTA         PIC X(10).				
           03 REG-CONS1-FECHA-BAJA         PIC X(10).
           03 REG-CONS1-ESTADO             PIC 9(02).
           03 REG-CONS1-NOMBRE-CONSORCIO   PIC X(30).
           03 REG-CONS1-TEL                PIC X(15).
           03 REG-CONS1-DIR                PIC X(30).

       FD CONS2 	LABEL RECORD IS STANDARD
			VALUE OF FILE-ID IS 
                        'cons2.dat'.
       01 REG-CONS2.	
	   03 REG-CONS2-CUIT-CONS          PIC 9(15).
	   03 REG-CONS2-FECHA-ALTA         PIC X(10).
	   03 REG-CONS2-FECHA-BAJA         PIC X(10).
	   03 REG-CONS2-ESTADO             PIC 9(02).
	   03 REG-CONS2-NOMBRE-CONSORCIO   PIC X(30).
	   03 REG-CONS2-TEL                PIC X(15).
	   03 REG-CONS2-DIR                PIC X(30).

       FD CONS3 	LABEL RECORD IS STANDARD
			VALUE OF FILE-ID IS 
                        'cons3.dat'.
       01 REG-CONS3.	
	   03 REG-CONS3-CUIT-CONS          PIC 9(15).
	   03 REG-CONS3-FECHA-ALTA         PIC X(10).
	   03 REG-CONS3-FECHA-BAJA         PIC X(10).
	   03 REG-CONS3-ESTADO             PIC 9(02).
	   03 REG-CONS3-NOMBRE-CONSORCIO   PIC X(30).
	   03 REG-CONS3-TEL                PIC X(15).
	   03 REG-CONS3-DIR                PIC X(30).

       FD CUENTAS LABEL RECORD IS STANDARD
	           VALUE OF FILE-ID IS 
                   "cuentas.dat".
       01 CTA. 
	   03 CTA-CUIT-CONS                PIC 9(15).
	   03 CTA-NRO-CTA                  PIC 9(08).
	   03 CTA-FECHA-ALTA               PIC X(10).
	   03 CTA-ENTIDAD                  PIC 9(03).
	   03 CTA-SUCURSAL                 PIC 9(03).

       FD ESTADOS LABEL RECORD IS STANDARD
	          VALUE OF FILE-ID IS 
                  "estados.dat".   

       01 EST.
	   03 EST-ESTADO                  PIC 9(02).
	   03 EST-DESCRIP                 PIC X(15).

       FD MAESTRO LABEL RECORD IS STANDARD
                  VALUE OF FILE-ID IS 
                  "maestro.dat".

       01 MAE.
	   03 MAE-CUIT-CONS               PIC 9(15).
	   03 MAE-FECHA-ALTA              PIC X(10).
	   03 MAE-DESCRIP-ESTADO          PIC X(15).
	   03 MAE-NOMBRE-CONSORCIO        PIC X(30).
	   03 MAE-TEL                     PIC X(15).
	   03 MAE-DIR                     PIC X(30).
	   03 MAE-NRO-CTA                 PIC 9(08).
	
       FD LISTADO LABEL RECORD IS STANDARD
                  VALUE OF FILE-ID IS 
                  "lisBajas".
       01 LINEA                           PIC X(80).

       FD ESTADIST LABEL RECORD IS STANDARD
                  VALUE OF FILE-ID IS 
                  "Estadist".
       01 LINEA-E                         PIC X(125).

       WORKING-STORAGE SECTION.
       77 WS-NRO-CTA-AUX PIC 9(8).
       77 FS-CONS1 		PIC XX.
          88 FS-CONS1-PERMITTED      VALUES '10' '23'. 
          88 FS-CONS1-END-OF-FILE    VALUE '10'. 
          88 FS-CONS1-INVALID-KEY    VALUE '23'.       
       77 FS-CONS2 		PIC XX.
          88 FS-CONS2-PERMITTED      VALUES '10' '23'. 
          88 FS-CONS2-END-OF-FILE    VALUE '10'. 
          88 FS-CONS2-INVALID-KEY    VALUE '23'.       
       77 FS-CONS3 		PIC XX.
          88 FS-CONS3-PERMITTED      VALUES '10' '23'. 
          88 FS-CONS3-END-OF-FILE    VALUE '10'. 
          88 FS-CONS3-INVALID-KEY    VALUE '23'.       
       77 FS-CTAS		PIC XX.
          88 FS-CTAS-PERMITTED      VALUES '10' '23'. 
          88 FS-CTAS-END-OF-FILE    VALUE '10'. 
          88 FS-CTAS-INVALID-KEY    VALUE '23'.       
       77 FS-EST		PIC XX.
          88 FS-EST-PERMITTED      VALUES '10' '23'. 
          88 FS-EST-END-OF-FILE    VALUE '10'. 
          88 FS-EST-INVALID-KEY    VALUE '23'.       
       77 FS-MAE		PIC XX.
       77 FS-LIST		PIC XX.
       77 FS-ESTAD              PIC XX.
       77 CUIT-N1               PIC 9(15).
       77 CUIT-N2               PIC 9(15).
       77 CUIT-N3               PIC 9(15).
		
       77 cantConsorcios 		PIC 99 VALUE 0.
       77 bajas 			PIC 99 VALUE 0.
       77 cantLineas 			PIC 99 VALUE 0.
       77 cantHojas 			PIC 99 VALUE 1.
       77 cantRegmC 			PIC 99 VALUE 0.
       77 CANTESTADOS 			PIC 99 VALUE 0.
       77 CONT-ANIO 			PIC 99 VALUE 0.
       77 I                    PIC 99.
       77 J                    PIC 99.
       77 IND2                 PIC 99.
       77 MAX-EST			PIC 99 VALUE 30.
       77 CAN-EST                       PIC 99 VALUE 0.
       77 EST-OK               PIC XX.
       77 ENCONTRADO			PIC X(02).
       77 ANIO-ESTADISTICA     PIC X(04).
       77 EXISTE-ESTADISTICA   PIC X(02).
       77 EST-ACTUAL           PIC 9(02).
       77 WS-DESCRIP-ESTADO    PIC X(15).
       77 WS-L-CONT-EST	       PIC 99.
       77 L-CONT-EST           PIC 99.
       01 CON-MENOR.
          03 CON-MENOR-CUIT-CONS          PIC 9(15).
          03 CON-MENOR-FECHA-ALTA         PIC X(10).
          03 CON-MENOR-FECHA-BAJA         PIC X(10).
          03 CON-MENOR-ESTADO             PIC 9(02).
          03 CON-MENOR-NOMBRE-CONSORCIO   PIC X(30).
          03 CON-MENOR-TEL                PIC X(15).
          03 CON-MENOR-DIR                PIC X(30).
       01 AUX.
          03 AUX-EST PIC X(02).
          03 AUX-DESCRIP PIC X(15).
       01 T-ESTADISTICAS.
          03 EST-FILA OCCURS 30 TIMES.
             05 T-EST-ANIO PIC X(04).
             05 T-EST-COL OCCURS 30 TIMES PIC 9(02).
       01 FEC-ESTADISTICA.
          03 F-EST-ANIO PIC X(4).
          03 FILLER     PIC X(1) VALUE '-'.
          03 F-EST-MES  PIC X(2).
          03 FILLER     PIC X(1) VALUE '-'.
          03 F-EST-DIA  PIC X(2).
       01 TAB-ESTADOS.
          03 TAB-ESTADOS-ELM 
                         OCCURS 30 TIMES
                         ascending key TAB-ESTADOS-ESTADO
                         INDEXED BY IND.
                                05 TAB-ESTADOS-ESTADO PIC 9(02) 
                                                      VALUE 99.
                                05 TAB-ESTADOS-DESCRIP PIC X(15)
                                       VALUE 'ZZZZZZZZZZZZZZZ'.
       01 MIERDA         PIC 9(02).
       01 FECHA.
          03 FECHA-AA    PIC 9(02).
          03 FECHA-MM    PIC 9(02).
          03 FECHA-DD    PIC 9(02).
       01 PE1-ENCABE.
          03 FILLER PIC X(07) VALUE 'Fecha: '.
          03 PE1-FECHA-DD PIC Z9.
          03 FILLER       PIC X VALUE '/'.
          03 PE1-FECHA-MM PIC 99.
          03 FILLER       PIC X VALUE '/'.
          03 PE1-FECHA-AA PIC 99.
          03 FILLER       PIC X(61) VALUE
             '                                  Nro. Hoja: '.
          03 PE1-HOJA     PIC 9999 VALUE ZERO.
      
       01 PE2-ENCABE.
          03 FILLER PIC X(80) VALUE ALL ' '.
       01 PE3-ENCABE.
          03 FILLER   PIC X(25).
          03 FILLER   PIC X(30) VALUE 'LISTADO DE CONSORCIOS DE BAJA'.
          03 FILLER   PIC X(25).
      
       01 PB1-BAJA.
          03 FILLER   PIC X(13) VALUE 'CUIT-CONS'.
          03 FILLER   PIC X(13) VALUE 'FEC-ALTA'.
          03 FILLER   PIC X(13) VALUE 'FEC-BAJA'.
          03 FILLER   PIC X(13) VALUE 'NOMBRE'.
          03 FILLER   PIC X(13) VALUE 'TELEFONO'.
          03 FILLER   PIC X(13) VALUE 'DIRECCION'.
      
       01 PB2-BAJA.
          03 PB2-BAJA-CUIT-CONS   PIC 9(15).
          03 PB2-BAJA-FEC-ALTA    PIC X(10).
          03 PB2-BAJAR-FEC-BAJA   PIC X(10).
          03 PB2-BAJA-NOMBRE      PIC X(10).
          03 PB2-BAJA-TELEFONO    PIC X(15).
          03 PB2-BAJA-DIRECCION   PIC X(20).
      
       01 PB3-BAJA.
          03 F PIC X(26) VALUE 'TOTAL NOVEDADES POR CUIT: '.
          03 PB3-TOTAL-NOV    PIC 9999 VALUE ZERO.
                
       01 PB-FINAL.
          03 F PIC X(35) 
           	  VALUE 'Total de Consorcios dados de baja: '.
          03 PB-FINAL-TOTAL PIC 9999 VALUE ZERO.
		   
       01 EST-ENCABEZADO-1.
          03 FILLER PIC X(24).
          03 FILLER PIC X(31) 
                    VALUE 'CANTIDAD DE ALTAS POR CONSORCIO'.
          03 FILLER PIC X(25).
       01 EST-ENCABEZADO-2.
          03 FILLER PIC X(80) VALUE ALL ' '.
		   
       01 EST-ENCABEZADO-3.
          03 FILLER PIC X(4) VALUE 'ANIO'.
          03 FILLER PIC X(2) VALUE '  '.
          03 FILLER PIC X(7) VALUE 'ESTADOS'.
          03 FILLER PIC X(67) VALUE ALL ' '.

       01 EST-ENCABEZADO-L.
          03 FILLER PIC X(80) VALUE ALL '-'.		   

       01 EST-ENCABEZADO-4.
          03 FILLER PIC X(4) VALUE ALL ' '.
          03 FILLER PIC X(2) VALUE ' |'.
          03 E-01 PIC X(2).
          03 FILLER PIC X(2) VALUE ' |'.
          03 E-02 PIC X(2).
          03 FILLER PIC X(2) VALUE ' |'.
          03 E-03 PIC X(2).
          03 FILLER PIC X(2) VALUE ' |'.
          03 E-04 PIC X(2).
          03 FILLER PIC X(2) VALUE ' |'.
          03 E-05 PIC X(2).
          03 FILLER PIC X(2) VALUE ' |'.
          03 E-06 PIC X(2).
          03 FILLER PIC X(2) VALUE ' |'.
          03 E-07 PIC X(2).
          03 FILLER PIC X(2) VALUE ' |'.
          03 E-08 PIC X(2).
          03 FILLER PIC X(2) VALUE ' |'.
          03 E-09 PIC X(2).
          03 FILLER PIC X(2) VALUE ' |'.
          03 E-10 PIC X(2).
          03 FILLER PIC X(2) VALUE ' |'.
          03 E-11 PIC X(2).
          03 FILLER PIC X(2) VALUE ' |'.
          03 E-12 PIC X(2).
          03 FILLER PIC X(2) VALUE ' |'.
          03 E-13 PIC X(2).
          03 FILLER PIC X(2) VALUE ' |'.
          03 E-14 PIC X(2).
          03 FILLER PIC X(2) VALUE ' |'.
          03 E-15 PIC X(2).
          03 FILLER PIC X(2) VALUE ' |'.
          03 E-16 PIC X(2).
          03 FILLER PIC X(2) VALUE ' |'.
          03 E-17 PIC X(2).
          03 FILLER PIC X(2) VALUE ' |'.
          03 E-18 PIC X(2).
          03 FILLER PIC X(2) VALUE ' |'.
          03 E-19 PIC X(2).
          03 FILLER PIC X(2) VALUE ' |'.
          03 E-20 PIC X(2).
          03 FILLER PIC X(2) VALUE ' |'.
          03 E-21 PIC X(2).
          03 FILLER PIC X(2) VALUE ' |'.
          03 E-22 PIC X(2).
          03 FILLER PIC X(2) VALUE ' |'.
          03 E-23 PIC X(2).
          03 FILLER PIC X(2) VALUE ' |'.
          03 E-24 PIC X(2).
          03 FILLER PIC X(2) VALUE ' |'.
          03 E-25 PIC X(2).
          03 FILLER PIC X(2) VALUE ' |'.
          03 E-26 PIC X(2).
          03 FILLER PIC X(2) VALUE ' |'.
          03 E-27 PIC X(2).
          03 FILLER PIC X(2) VALUE ' |'.
          03 E-28 PIC X(2).
          03 FILLER PIC X(2) VALUE ' |'.
          03 E-29 PIC X(2).
          03 FILLER PIC X(2) VALUE ' |'.
          03 E-30 PIC X(2).
          03 FILLER PIC X VALUE '|'.
		
       01 LINEA-ESTADISTICA.
          03 L-ANIO PIC X(4).
          03 FILLER PIC X(2) VALUE ' |'.
          03 L-EST-01 PIC X(2).
          03 FILLER PIC X(2) VALUE ' |'.
          03 L-EST-02 PIC X(2).
          03 FILLER PIC X(2) VALUE ' |'.
          03 L-EST-03 PIC X(2).
          03 FILLER PIC X(2) VALUE ' |'.
          03 L-EST-04 PIC X(2).
          03 FILLER PIC X(2) VALUE ' |'.
          03 L-EST-05 PIC X(2).
          03 FILLER PIC X(2) VALUE ' |'.
          03 L-EST-06 PIC X(2).
          03 FILLER PIC X(2) VALUE ' |'.
          03 L-EST-07 PIC X(2).
          03 FILLER PIC X(2) VALUE ' |'.
          03 L-EST-08 PIC X(2).
          03 FILLER PIC X(2) VALUE ' |'.
          03 L-EST-09 PIC X(2).
          03 FILLER PIC X(2) VALUE ' |'.
          03 L-EST-10 PIC X(2).
          03 FILLER PIC X(2) VALUE ' |'.
          03 L-EST-11 PIC X(2).
          03 FILLER PIC X(2) VALUE ' |'.
          03 L-EST-12 PIC X(2).
          03 FILLER PIC X(2) VALUE ' |'.
          03 L-EST-13 PIC X(2).
          03 FILLER PIC X(2) VALUE ' |'.
          03 L-EST-14 PIC X(2).
          03 FILLER PIC X(2) VALUE ' |'.
          03 L-EST-15 PIC X(2).
          03 FILLER PIC X(2) VALUE ' |'.
          03 L-EST-16 PIC X(2).
          03 FILLER PIC X(2) VALUE ' |'.
          03 L-EST-17 PIC X(2).
          03 FILLER PIC X(2) VALUE ' |'.
          03 L-EST-18 PIC X(2).
          03 FILLER PIC X(2) VALUE ' |'.
          03 L-EST-19 PIC X(2).
          03 FILLER PIC X(2) VALUE ' |'.
          03 L-EST-20 PIC X(2).
          03 FILLER PIC X(2) VALUE ' |'.
          03 L-EST-21 PIC X(2).
          03 FILLER PIC X(2) VALUE ' |'.
          03 L-EST-22 PIC X(2).
          03 FILLER PIC X(2) VALUE ' |'.
          03 L-EST-23 PIC X(2).
          03 FILLER PIC X(2) VALUE ' |'.
          03 L-EST-24 PIC X(2).
          03 FILLER PIC X(2) VALUE ' |'.
          03 L-EST-25 PIC X(2).
          03 FILLER PIC X(2) VALUE ' |'.
          03 L-EST-26 PIC X(2).
          03 FILLER PIC X(2) VALUE ' |'.
          03 L-EST-27 PIC X(2).
          03 FILLER PIC X(2) VALUE ' |'.
          03 L-EST-28 PIC X(2).
          03 FILLER PIC X(2) VALUE ' |'.
          03 L-EST-29 PIC X(2).
          03 FILLER PIC X(2) VALUE ' |'.
          03 L-EST-30 PIC X(2).
          03 FILLER PIC X VALUE '|'.
       
       PROCEDURE DIVISION.
       DECLARATIVES.
       DECLAR-INPUT SECTION.
       USE AFTER ERROR PROCEDURE ON INPUT.
       CONTINUE-INPUT.
           CONTINUE.
       DECLAR-OUTPUT SECTION.
       USE AFTER ERROR PROCEDURE ON OUTPUT.
       CONTINUE-OUTPUT.
           CONTINUE.
       DECLAR-IO SECTION.
       USE AFTER ERROR PROCEDURE ON I-O.
       CONTINUE-IO.
           CONTINUE.
       DECLAR-EXTEND SECTION.
       USE AFTER ERROR PROCEDURE ON EXTEND.
       CONTINUE-EXTEND.
           CONTINUE.
       END DECLARATIVES.
       
       PROGRAMA SECTION.
       INICIO.    
           perform INICIALIZAR.
           perform ABRIR-ARCHIVOS.
           perform GEN-TABLA-ESTADOS.
           perform LEO-CONSORCIO-1.
           perform LEO-CONSORCIO-2.
           perform LEO-CONSORCIO-3.
           perform LEO-CUENTAS.
           perform IMPRIMO-ENCABEZADO.
           perform CICLO-CONSORCIO UNTIL FS-CONS1 = 10 AND 
                         FS-CONS2 = 10 AND FS-CONS3 = 10.
           perform IMPRIMIR-BAJAS-FIN.
           perform MOSTRAR-ESTADISTICAS.
           perform CERRAR-ARCHIVOS.
           STOP RUN.

       INICIALIZAR.
      *     DISPLAY "INICIALIZAR INICIA".
           MOVE 0 TO bajas.
           MOVE 1 TO cantHojas.
           MOVE 0 TO CONT-ANIO.
           MOVE 0 TO CAN-EST.
           ACCEPT FECHA FROM DATE.
           MOVE FECHA-AA TO PE1-FECHA-AA.
           MOVE FECHA-MM TO PE1-FECHA-MM.
           MOVE FECHA-DD TO PE1-FECHA-DD.
      *     DISPLAY "INICIALIZAR FIN".
      
       ABRIR-ARCHIVOS.
      *     DISPLAY "ABRIR-ARCHIVOS INICIA".
           OPEN INPUT CONS1.
           IF FS-CONS1 NOT = ZERO
              DISPLAY "Err abrir Consorcios1: " FS-CONS1
              STOP RUN.
           OPEN INPUT CONS2.
           IF FS-CONS2 NOT = ZERO
              DISPLAY "Err abrir Consorcios2: " FS-CONS2
              STOP RUN.
           OPEN INPUT CONS3.
           IF FS-CONS3 NOT = ZERO
              DISPLAY "Err abrir Consorcios3: " FS-CONS3
           STOP RUN.
           OPEN INPUT CUENTAS.
           IF FS-CTAS NOT = ZERO
              DISPLAY "Error al abrir Cuentas: " FS-CTAS
              STOP RUN.
           OPEN INPUT ESTADOS.
           IF FS-EST NOT = ZERO
              DISPLAY "Error al abrir Estados: " FS-EST
              STOP RUN.
           OPEN OUTPUT MAESTRO.
           IF FS-MAE NOT = ZERO
              DISPLAY "Err abrir Maestro: " FS-MAE
              STOP RUN.
           OPEN OUTPUT LISTADO.
           IF FS-LIST NOT = ZERO
              DISPLAY "Err abrir listado: " FS-LIST
              STOP RUN.

           OPEN OUTPUT ESTADIST.
           IF FS-ESTAD NOT = ZERO
              DISPLAY "Err abrir Estadisticas: " FS-ESTAD
              STOP RUN.
      *     DISPLAY "ABRIR-ARCHIVOS FIN".

       GEN-TABLA-ESTADOS.
           DISPLAY "GEN-TABLA-ESTADOS".
           PERFORM LEO-ESTADO.
           PERFORM CARGAR-ESTADO VARYING WS-L-CONT-EST
                   FROM 1 BY 1 
                   UNTIL FS-EST = 10 OR WS-L-CONT-EST > 30.	
           PERFORM ORDENAR-TABLA-ESTADOS.	
		
       CARGAR-ESTADO.
      *     DISPLAY "1: " EST.
           MOVE EST TO TAB-ESTADOS-ELM (WS-L-CONT-EST).
      *     DISPLAY "2: " TAB-ESTADOS-ELM (1).
           PERFORM LEO-ESTADO.
			
       LEO-ESTADO.
           DISPLAY "LEO-ESTADO-INICIA".
           MOVE 'NO' TO EST-OK.
           PERFORM UNTIL EST-OK = 'SI'
             READ ESTADOS
             IF EST-ESTADO <= 30 
                MOVE 'SI' TO EST-OK
             END-IF
           END-PERFORM.
           IF FS-EST = ZERO ADD 1 TO CAN-EST.
      *     DISPLAY "FS-EST = " FS-EST.
           IF FS-EST NOT = ZERO AND 10
              DISPLAY "Error al leer Estados: " FS-EST
              STOP RUN.			
			
       ORDENAR-TABLA-ESTADOS.
           DISPLAY "ORDENAR TABLA ESTADOS".
           MOVE 1 TO I.
           PERFORM UNTIL I > MAX-EST
            MOVE I TO J
            PERFORM UNTIL J > MAX-EST
             IF (TAB-ESTADOS-ELM (I) > TAB-ESTADOS-ELM (J))
                MOVE TAB-ESTADOS-ELM (I) TO AUX
                MOVE TAB-ESTADOS-ELM (J) TO TAB-ESTADOS-ELM (I)
                MOVE AUX TO TAB-ESTADOS-ELM (J)
             END-IF
             ADD 1 TO J GIVING J
            END-PERFORM
            ADD 1 TO I GIVING I
           END-PERFORM.
       
       LEO-CONSORCIO-1.
           READ CONS1.
           DISPLAY "LEO-CONSOR1 " FS-CONS1.
           IF FS-CONS1 NOT = ZERO AND 10
              DISPLAY "Err leer consorcios1 " FS-CONS1
              STOP RUN.
 
       LEO-CONSORCIO-2.
           READ CONS2.
           DISPLAY "LEO-CONSOR2 " FS-CONS2.
           IF FS-CONS2 NOT = ZERO AND 10
              DISPLAY "Err: leer consorcios2:" FS-CONS2
              STOP RUN.
            
       LEO-CONSORCIO-3.
           READ CONS3.
           DISPLAY "LEO-CONSORC3 " FS-CONS3.
           IF FS-CONS3 NOT = ZERO AND 10
              DISPLAY "Err: leer consorcios3:" FS-CONS3
              STOP RUN.    
  
       LEO-CUENTAS.
           DISPLAY "LEO-CUENTAS".
           READ CUENTAS.
           IF FS-CTAS NOT = ZERO AND 10
              DISPLAY "Error al leer cUENTas: " FS-CTAS
              STOP RUN.

       OBTENER-ESTADO.
      *     DISPLAY "OBTENER ESTADO " CON-MENOR-ESTADO "--".
           SET IND TO 1.
           SEARCH ALL TAB-ESTADOS-ELM
                  AT END MOVE "--ERROR--ENE--" 
                                            TO WS-DESCRIP-ESTADO                 
                  WHEN TAB-ESTADOS-ESTADO(IND) = CON-MENOR-ESTADO
                       PERFORM OBTENER-INFO-ESTADO
           END-SEARCH.
				
       OBTENER-INFO-ESTADO.
      *     DISPLAY "OBTENER INFORMACION DEL ESTADO".
           MOVE TAB-ESTADOS-DESCRIP(IND) TO WS-DESCRIP-ESTADO.
		
       IMPRIMO-ENCABEZADO.
           MOVE cantHojas TO PE1-HOJA.
           WRITE LINEA FROM PE1-ENCABE.
           PERFORM CHECK-WRITE-LISBAJAS.
           WRITE LINEA FROM PE2-ENCABE.
           PERFORM CHECK-WRITE-LISBAJAS.
           WRITE LINEA FROM PE3-ENCABE.
           PERFORM CHECK-WRITE-LISBAJAS.
           WRITE LINEA FROM PE2-ENCABE.
           PERFORM CHECK-WRITE-LISBAJAS.
           ADD 1 TO cantHojas.
           MOVE 4 TO cantLineas.
       
       LISTAR-BAJA.
           IF cantLineas >= 60 
              PERFORM IMPRIMO-ENCABEZADO.
           PERFORM IMPRIMIR-BAJA.
           ADD 1 TO bajas.

       IMPRIMIR-BAJAS-FIN.
           DISPLAY "IMPRIMIR-BAJA".
           MOVE bajas TO PB-FINAL-TOTAL.
           WRITE LINEA FROM PB-FINAL.
           PERFORM CHECK-WRITE-LISBAJAS.
           
       IMPRIMIR-BAJA.
           DISPLAY "IMPRIMO-BAJAS".
           WRITE LINEA FROM PB1-BAJA.
           PERFORM CHECK-WRITE-LISBAJAS.
           MOVE CON-MENOR-CUIT-CONS TO PB2-BAJA-CUIT-CONS.
           MOVE CON-MENOR-FECHA-ALTA TO PB2-BAJA-FEC-ALTA.
           MOVE CON-MENOR-FECHA-BAJA TO PB2-BAJAR-FEC-BAJA.
           MOVE CON-MENOR-NOMBRE-CONSORCIO TO PB2-BAJA-NOMBRE.
           MOVE CON-MENOR-TEL TO PB2-BAJA-TELEFONO.
           MOVE CON-MENOR-DIR TO PB2-BAJA-DIRECCION.
           WRITE LINEA FROM PB2-BAJA.
           PERFORM CHECK-WRITE-LISBAJAS.
           MOVE cantRegmC TO PB3-TOTAL-NOV.
           WRITE LINEA FROM PB3-BAJA.
           PERFORM CHECK-WRITE-LISBAJAS.
           WRITE LINEA FROM PE2-ENCABE.
           PERFORM CHECK-WRITE-LISBAJAS.
           ADD 4 TO cantLineas.
           
       CICLO-CONSORCIO.
           DISPLAY "CICLO-CONSORCIO".
           PERFORM DET-MENOR.
           MOVE 1 TO cantRegmC.
           PERFORM POS-CUENTAS UNTIL FS-CTAS = 10 
                   OR CTA-CUIT-CONS >= CON-MENOR-CUIT-CONS. 
           PERFORM POS-CONSORN1 UNTIL FS-CONS1 = 10 
                   OR REG-CONS1-CUIT-CONS IS NOT EQUAL 
                                TO CON-MENOR-CUIT-CONS. 
           PERFORM POS-CONSORN2 UNTIL FS-CONS2 = 10 
                   OR REG-CONS2-CUIT-CONS IS NOT EQUAL 
                                TO CON-MENOR-CUIT-CONS. 
           PERFORM POS-CONSORN3 UNTIL FS-CONS3 = 10 
                   OR REG-CONS3-CUIT-CONS IS NOT EQUAL 
                                TO CON-MENOR-CUIT-CONS. 
           PERFORM OBTENER-ESTADO.
           IF CON-MENOR-ESTADO = '02'
              PERFORM LISTAR-BAJA
           ELSE
              PERFORM ALTA-MAESTRO.
           
       MOSTRAR-ESTADISTICAS.
      *     DISPLAY "MOSTRAR-ESTADISTICAS".
      *     DISPLAY EST-ENCABEZADO-1.
           WRITE LINEA-E FROM EST-ENCABEZADO-1.
           PERFORM CHECK-WRITE-ESTADIST.
      *     DISPLAY EST-ENCABEZADO-2.
           WRITE LINEA-E FROM EST-ENCABEZADO-2.
           PERFORM CHECK-WRITE-ESTADIST.
      *     DISPLAY EST-ENCABEZADO-3.
           WRITE LINEA-E FROM EST-ENCABEZADO-3.
           PERFORM CHECK-WRITE-ESTADIST.
           PERFORM EST-ENCAB-T-ESTADOS.
      *     DISPLAY EST-ENCABEZADO-L.
           WRITE LINEA-E FROM EST-ENCABEZADO-L.
           PERFORM CHECK-WRITE-ESTADIST.
           MOVE 1 TO IND2.			
           PERFORM CICLO-ESTADISTICA-1 UNTIL IND2 > CONT-ANIO.
				
       EST-ENCAB-T-ESTADOS.
           IF CAN-EST >= 1 MOVE TAB-ESTADOS-ESTADO (1) TO E-01.
           IF CAN-EST >= 2 MOVE TAB-ESTADOS-ESTADO (2) TO E-02.
           IF CAN-EST >= 3 MOVE TAB-ESTADOS-ESTADO (3) TO E-03.
           IF CAN-EST >= 4 MOVE TAB-ESTADOS-ESTADO (4) TO E-04.
           IF CAN-EST >= 5 MOVE TAB-ESTADOS-ESTADO (5) TO E-05.
           IF CAN-EST >= 6 MOVE TAB-ESTADOS-ESTADO (6) TO E-06.
           IF CAN-EST >= 7 MOVE TAB-ESTADOS-ESTADO (7) TO E-07.
           IF CAN-EST >= 8 MOVE TAB-ESTADOS-ESTADO (8) TO E-08.
           IF CAN-EST >= 9 MOVE TAB-ESTADOS-ESTADO (9) TO E-09.
           IF CAN-EST >= 10 MOVE TAB-ESTADOS-ESTADO (10) TO E-10.
           IF CAN-EST >= 11 MOVE TAB-ESTADOS-ESTADO (11) TO E-11.
           IF CAN-EST >= 12 MOVE TAB-ESTADOS-ESTADO (12) TO E-12.
           IF CAN-EST >= 13 MOVE TAB-ESTADOS-ESTADO (13) TO E-13.
           IF CAN-EST >= 14 MOVE TAB-ESTADOS-ESTADO (14) TO E-14.
           IF CAN-EST >= 15 MOVE TAB-ESTADOS-ESTADO (15) TO E-15.
           IF CAN-EST >= 16 MOVE TAB-ESTADOS-ESTADO (16) TO E-16.
           IF CAN-EST >= 17 MOVE TAB-ESTADOS-ESTADO (17) TO E-17.
           IF CAN-EST >= 18 MOVE TAB-ESTADOS-ESTADO (18) TO E-18.
           IF CAN-EST >= 19 MOVE TAB-ESTADOS-ESTADO (19) TO E-19.
           IF CAN-EST >= 20 MOVE TAB-ESTADOS-ESTADO (20) TO E-20.
           IF CAN-EST >= 21 MOVE TAB-ESTADOS-ESTADO (21) TO E-21.
           IF CAN-EST >= 22 MOVE TAB-ESTADOS-ESTADO (22) TO E-22.
           IF CAN-EST >= 23 MOVE TAB-ESTADOS-ESTADO (23) TO E-23.
           IF CAN-EST >= 24 MOVE TAB-ESTADOS-ESTADO (24) TO E-24.
           IF CAN-EST >= 25 MOVE TAB-ESTADOS-ESTADO (25) TO E-25.
           IF CAN-EST >= 26 MOVE TAB-ESTADOS-ESTADO (26) TO E-26.
           IF CAN-EST >= 27 MOVE TAB-ESTADOS-ESTADO (27) TO E-27.
           IF CAN-EST >= 28 MOVE TAB-ESTADOS-ESTADO (28) TO E-28.
           IF CAN-EST >= 29 MOVE TAB-ESTADOS-ESTADO (29) TO E-29.
           IF CAN-EST >= 30 MOVE TAB-ESTADOS-ESTADO (30) TO E-30.
      *     DISPLAY EST-ENCABEZADO-4.
           WRITE LINEA-E FROM EST-ENCABEZADO-4.
           PERFORM CHECK-WRITE-ESTADIST.
			
       CICLO-ESTADISTICA-1.
           MOVE 1 TO L-CONT-EST.
      *     MOVE T-EST-COL(IND2, L-CONT-EST) TO L-ANIO.
           MOVE T-EST-ANIO(IND2) TO L-ANIO.
      *     DISPLAY "ANIO ES: " L-ANIO " IND2 vale: " IND2. 
           PERFORM ARMAR-LINEA-ESTADISTICA.
      *     DISPLAY LINEA-ESTADISTICA.
           WRITE LINEA-E FROM LINEA-ESTADISTICA.
           PERFORM CHECK-WRITE-ESTADIST.
           ADD 1 TO IND2.
			
       ARMAR-LINEA-ESTADISTICA.
           MOVE T-EST-COL(IND2, L-CONT-EST) TO L-EST-01.
           ADD 1 TO L-CONT-EST.
           MOVE T-EST-COL(IND2, L-CONT-EST) TO L-EST-02.
           ADD 1 TO L-CONT-EST.
           MOVE T-EST-COL(IND2, L-CONT-EST) TO L-EST-03.
           ADD 1 TO L-CONT-EST.
           MOVE T-EST-COL(IND2, L-CONT-EST) TO L-EST-04.
           ADD 1 TO L-CONT-EST.
           MOVE T-EST-COL(IND2, L-CONT-EST) TO L-EST-05.
           ADD 1 TO L-CONT-EST.
           MOVE T-EST-COL(IND2, L-CONT-EST) TO L-EST-06.
           ADD 1 TO L-CONT-EST.
           MOVE T-EST-COL(IND2, L-CONT-EST) TO L-EST-07.
           ADD 1 TO L-CONT-EST.
           MOVE T-EST-COL(IND2, L-CONT-EST) TO L-EST-08.
           ADD 1 TO L-CONT-EST.
           MOVE T-EST-COL(IND2, L-CONT-EST) TO L-EST-09.
           ADD 1 TO L-CONT-EST.
           MOVE T-EST-COL(IND2, L-CONT-EST) TO L-EST-10.
           ADD 1 TO L-CONT-EST.
           MOVE T-EST-COL(IND2, L-CONT-EST) TO L-EST-11.
           ADD 1 TO L-CONT-EST.
           MOVE T-EST-COL(IND2, L-CONT-EST) TO L-EST-12.
           ADD 1 TO L-CONT-EST.
           MOVE T-EST-COL(IND2, L-CONT-EST) TO L-EST-13.
           ADD 1 TO L-CONT-EST.
           MOVE T-EST-COL(IND2, L-CONT-EST) TO L-EST-14.
           ADD 1 TO L-CONT-EST.
           MOVE T-EST-COL(IND2, L-CONT-EST) TO L-EST-15.
           ADD 1 TO L-CONT-EST.
           MOVE T-EST-COL(IND2, L-CONT-EST) TO L-EST-16.
           ADD 1 TO L-CONT-EST.
           MOVE T-EST-COL(IND2, L-CONT-EST) TO L-EST-17.
           ADD 1 TO L-CONT-EST.
           MOVE T-EST-COL(IND2, L-CONT-EST) TO L-EST-18.
           ADD 1 TO L-CONT-EST.
           MOVE T-EST-COL(IND2, L-CONT-EST) TO L-EST-19.
           ADD 1 TO L-CONT-EST.
           MOVE T-EST-COL(IND2, L-CONT-EST) TO L-EST-20.
           ADD 1 TO L-CONT-EST.
           MOVE T-EST-COL(IND2, L-CONT-EST) TO L-EST-21.
           ADD 1 TO L-CONT-EST.
           MOVE T-EST-COL(IND2, L-CONT-EST) TO L-EST-22.
           ADD 1 TO L-CONT-EST.
           MOVE T-EST-COL(IND2, L-CONT-EST) TO L-EST-23.
           ADD 1 TO L-CONT-EST.
           MOVE T-EST-COL(IND2, L-CONT-EST) TO L-EST-24.
           ADD 1 TO L-CONT-EST.
           MOVE T-EST-COL(IND2, L-CONT-EST) TO L-EST-25.
           ADD 1 TO L-CONT-EST.
           MOVE T-EST-COL(IND2, L-CONT-EST) TO L-EST-26.
           ADD 1 TO L-CONT-EST.
           MOVE T-EST-COL(IND2, L-CONT-EST) TO L-EST-27.
           ADD 1 TO L-CONT-EST.
           MOVE T-EST-COL(IND2, L-CONT-EST) TO L-EST-28.
           ADD 1 TO L-CONT-EST.
           MOVE T-EST-COL(IND2, L-CONT-EST) TO L-EST-29.
           ADD 1 TO L-CONT-EST.
           MOVE T-EST-COL(IND2, L-CONT-EST) TO L-EST-30.
           ADD 1 TO L-CONT-EST.			

       CERRAR-ARCHIVOS.
           DISPLAY "CERRAR-ARCHIVOS".
           CLOSE CONS1.
           CLOSE CONS2.
           CLOSE CONS3.
           CLOSE CUENTAS.
           CLOSE ESTADOS.
           CLOSE MAESTRO.
           CLOSE LISTADO.
           CLOSE ESTADIST.

       DET-MENOR.
      *     DISPLAY "DET-MENOR".
      *     DISPLAY "REG-CONS1: " REG-CONS1.
           MOVE REG-CONS1 TO CON-MENOR.
      *     DISPLAY "CON-MENOR: " CON-MENOR.
           IF REG-CONS2-CUIT-CONS < CON-MENOR-CUIT-CONS
      *        DISPLAY REG-CONS2
              MOVE REG-CONS2 TO CON-MENOR.
      *        DISPLAY "CON-MENOR: " CON-MENOR
           IF REG-CONS3-CUIT-CONS < CON-MENOR-CUIT-CONS
      *        DISPLAY REG-CONS3
              MOVE REG-CONS3 TO CON-MENOR.
      *        DISPLAY "CON-MENOR: " CON-MENOR.
      *     DISPLAY "El menor es " CON-MENOR.

       POS-CUENTAS.
      *     DISPLAY "POS-CUENTAS".
           PERFORM  LEO-CUENTAS.

       POS-CONSORN1.			
           MOVE REG-CONS1-FECHA-ALTA TO FEC-ESTADISTICA.
           MOVE REG-CONS1-ESTADO TO EST-ACTUAL.
           PERFORM GENERAR-ESTADISTICAS.
      *     MOVE REG-CONS1 TO CON-MENOR.
           PERFORM LEO-CONSORCIO-1.
           IF FS-CONS1 = 10 
              MOVE 999999999999999 TO REG-CONS1-CUIT-CONS.
           IF REG-CONS1-CUIT-CONS = CON-MENOR-CUIT-CONS 
              ADD 1 TO cantRegmC.           
        
       POS-CONSORN2.
      *     DISPLAY "Estoy en POS-CONSORN2".
           MOVE REG-CONS2-FECHA-ALTA TO FEC-ESTADISTICA.
           MOVE REG-CONS2-ESTADO TO EST-ACTUAL.
           PERFORM GENERAR-ESTADISTICAS.
      *     MOVE REG-CONS2 TO CON-MENOR.
           PERFORM LEO-CONSORCIO-2.
           IF FS-CONS2 = 10 
              MOVE 999999999999999 TO REG-CONS2-CUIT-CONS.
           IF REG-CONS2-CUIT-CONS = CON-MENOR-CUIT-CONS 
              ADD 1 TO cantRegmC.
        
       POS-CONSORN3.
           MOVE REG-CONS3-FECHA-ALTA TO FEC-ESTADISTICA.
           MOVE REG-CONS3-ESTADO TO EST-ACTUAL.
           PERFORM GENERAR-ESTADISTICAS.
      *     MOVE REG-CONS3 TO CON-MENOR.
           PERFORM LEO-CONSORCIO-3.
           IF FS-CONS3 = 10 
              MOVE 999999999999999 TO REG-CONS3-CUIT-CONS.
           IF REG-CONS3-CUIT-CONS = CON-MENOR-CUIT-CONS 
              ADD 1 TO cantRegmC.
	
       ALTA-MAESTRO.
      *     DISPLAY "ALTA MAESTRO".
           MOVE CTA-NRO-CTA TO WS-NRO-CTA-AUX.
           IF CON-MENOR-CUIT-CONS NOT EQUAL TO CTA-CUIT-CONS
              MOVE 0 TO WS-NRO-CTA-AUX.
           ADD 1 TO cantConsorcios.
           MOVE CON-MENOR-CUIT-CONS TO MAE-CUIT-CONS.
           MOVE CON-MENOR-FECHA-ALTA TO MAE-FECHA-ALTA.
           MOVE WS-DESCRIP-ESTADO TO MAE-DESCRIP-ESTADO.
           MOVE CON-MENOR-NOMBRE-CONSORCIO TO MAE-NOMBRE-CONSORCIO.
           MOVE CON-MENOR-TEL TO MAE-TEL.
           MOVE CON-MENOR-DIR TO MAE-DIR.
           MOVE WS-NRO-CTA-AUX TO MAE-NRO-CTA.
           WRITE MAE.

       GENERAR-ESTADISTICAS.
      *     DISPLAY "GENERAR-ESTADISTICAS".
           MOVE F-EST-ANIO TO ANIO-ESTADISTICA.			
           PERFORM BUSCAR-ANIO.
           IF EXISTE-ESTADISTICA = 'SI'
              PERFORM ACTUALIZAR-ESTADISTICA
           ELSE
              PERFORM AGREGAR-Y-ACTUALIZAR.
				
       BUSCAR-ANIO.
           MOVE 1 TO I.
           MOVE 'NO' TO ENCONTRADO.
           PERFORM UNTIL I > CONT-ANIO
                         OR ENCONTRADO = 'SI'
             IF T-EST-ANIO(I) = ANIO-ESTADISTICA
                MOVE I TO IND2
                MOVE 'SI' TO ENCONTRADO
             ELSE 
                ADD 1 TO I
             END-IF
           END-PERFORM.
           IF ENCONTRADO = 'SI'
              PERFORM EXISTE-ANIO
           ELSE 
              PERFORM ANIO-NO-ENCONTRADO.
			
       ANIO-NO-ENCONTRADO.
           MOVE 'NO' TO EXISTE-ESTADISTICA.
           ADD 1 TO CONT-ANIO.
			
       EXISTE-ANIO.
           MOVE 'SI' TO EXISTE-ESTADISTICA.

       ACTUALIZAR-ESTADISTICA.
           DISPLAY "ACTUALIZAR-EST".
           ADD 1 TO EST-ACTUAL.
           ADD 1 TO T-EST-COL(IND2, EST-ACTUAL).			
		
       AGREGAR-Y-ACTUALIZAR.
           DISPLAY "AGREGAR-Y-ACTUALIZAR-EST".
           MOVE ANIO-ESTADISTICA TO T-EST-ANIO(CONT-ANIO).
           ADD 1 TO EST-ACTUAL.
           ADD 1 TO T-EST-COL(CONT-ANIO, EST-ACTUAL).

       CHECK-WRITE-LISBAJAS.
          IF FS-LIST NOT = ZERO AND 10
              DISPLAY "Error al escribir lisBAJAS: " FS-LIST
              STOP RUN.

       CHECK-WRITE-ESTADIST.
          IF FS-ESTAD NOT = ZERO AND 10
              DISPLAY "Error al escribir ESTADIST: " FS-ESTAD
              STOP RUN.        


			
			
				
			
			


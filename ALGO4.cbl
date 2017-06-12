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
       DATA DIVISION.
       FILE SECTION.
       FD CONS1 	LABEL RECORD IS STANDARD
                        VALUE OF FILE-ID IS 
                        'C:\consor\cons1.dat'.

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
                        'C:\consor\cons2.dat'.
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
                        'C:\consor\cons3.dat'.
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
                   "C:\consor\cuentas.dat".
       01 CTA. 
	   03 CTA-CUIT-CONS                PIC 9(15).
	   03 CTA-NRO-CTA                  PIC 9(08).
	   03 CTA-FECHA-ALTA               PIC X(10).
	   03 CTA-ENTIDAD                  PIC 9(03).
	   03 CTA-SUCURSAL                 PIC 9(03).

       FD ESTADOS LABEL RECORD IS STANDARD
	          VALUE OF FILE-ID IS 
                  "C:\consor\estados.dat".   

       01 EST.
	   03 EST-ESTADO                  PIC 9(02).
	   03 EST-DESCRIP                 PIC X(15).

       FD MAESTRO LABEL RECORD IS STANDARD
                  VALUE OF FILE-ID IS 
                  "C:\consor\maestro.dat".

       01 MAE.
	   03 MAE-CUIT-CONS               PIC 9(15).
	   03 MAE-FECHA-ALTA              PIC X(10).
	   03 MAE-DESCRIP-ESTADO          PIC X(15).
	   03 MAE-NOMBRE-CONSORCIO        PIC X(30).
	   03 MAE-TEL                     PIC X(15).
	   03 MAE-DIR                     PIC X(30).
	   03 MAE-NRO-CTA                 PIC 9(08).
	
       FD LISTADO LABEL RECORD OMITTED.
       
       01 LINEA                           PIC X(80).


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
       77 CUIT-N1               PIC 9(15).
       77 CUIT-N2               PIC 9(15).
       77 CUIT-N3               PIC 9(15).
		
       77 cantConsorcios 		PIC 99 VALUE 0.
       77 bajas 			PIC 99 VALUE 0.
       77 cantLineas 			PIC 99 VALUE 0.
       77 cantHojas 			PIC 99 VALUE 1.
       77 cantCons1 			PIC 99 VALUE 0.
       77 cantCons2 			PIC 99 VALUE 0.
       77 cantCons3 			PIC 99 VALUE 0.
       77 CANTESTADOS 			PIC 99 VALUE 0.
       77 CONT-ANIO 			PIC 99 VALUE 0.
       77 I                    PIC 99.
       77 J                    PIC 99.
       77 IND2                 PIC 99.
       77 MAX-EST			PIC 99 VALUE 30.
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
                                05 TAB-ESTADOS-ESTADO PIC 9(02).
                                05 TAB-ESTADOS-DESCRIP PIC X(15).
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
		   
		
       01 LINEA-ESTADISTICA.
          03 L-ANIO PIC X(4).
          03 FILLER PIC X(2) VALUE ' '.
          03 L-EST-01 PIC X(2).
          03 FILLER PIC X(2) VALUE ' '.
          03 L-EST-02 PIC X(2).
          03 FILLER PIC X(2) VALUE ' '.
          03 L-EST-03 PIC X(2).
          03 FILLER PIC X(2) VALUE ' '.
          03 L-EST-04 PIC X(2).
          03 FILLER PIC X(2) VALUE ' '.
          03 L-EST-05 PIC X(2).
          03 FILLER PIC X(2) VALUE ' '.
          03 L-EST-06 PIC X(2).
          03 FILLER PIC X(2) VALUE ' '.
          03 L-EST-07 PIC X(2).
          03 FILLER PIC X(2) VALUE ' '.
          03 L-EST-08 PIC X(2).
          03 FILLER PIC X(2) VALUE ' '.
          03 L-EST-09 PIC X(2).
          03 FILLER PIC X(2) VALUE ' '.
          03 L-EST-10 PIC X(2).
          03 FILLER PIC X(2) VALUE ' '.
          03 L-EST-11 PIC X(2).
          03 FILLER PIC X(2) VALUE ' '.
          03 L-EST-12 PIC X(2).
          03 FILLER PIC X(2) VALUE ' '.
          03 L-EST-13 PIC X(2).
          03 FILLER PIC X(2) VALUE ' '.
          03 L-EST-14 PIC X(2).
          03 FILLER PIC X(2) VALUE ' '.
          03 L-EST-15 PIC X(2).
          03 FILLER PIC X(2) VALUE ' '.
          03 L-EST-16 PIC X(2).
          03 FILLER PIC X(2) VALUE ' '.
          03 L-EST-17 PIC X(2).
          03 FILLER PIC X(2) VALUE ' '.
          03 L-EST-18 PIC X(2).
          03 FILLER PIC X(2) VALUE ' '.
          03 L-EST-19 PIC X(2).
          03 FILLER PIC X(2) VALUE ' '.
          03 L-EST-20 PIC X(2).
          03 FILLER PIC X(2) VALUE ' '.
          03 L-EST-21 PIC X(2).
          03 FILLER PIC X(2) VALUE ' '.
          03 L-EST-22 PIC X(2).
          03 FILLER PIC X(2) VALUE ' '.
          03 L-EST-23 PIC X(2).
          03 FILLER PIC X(2) VALUE ' '.
          03 L-EST-24 PIC X(2).
          03 FILLER PIC X(2) VALUE ' '.
          03 L-EST-25 PIC X(2).
          03 FILLER PIC X(2) VALUE ' '.
          03 L-EST-26 PIC X(2).
          03 FILLER PIC X(2) VALUE ' '.
          03 L-EST-27 PIC X(2).
          03 FILLER PIC X(2) VALUE ' '.
          03 L-EST-28 PIC X(2).
          03 FILLER PIC X(2) VALUE ' '.
          03 L-EST-29 PIC X(2).
          03 FILLER PIC X(2) VALUE ' '.
          03 L-EST-30 PIC X(2).
       
       PROCEDURE DIVISION.
       DECLARATIVES. 
       DC-EST SECTION. 
       USE AFTER STANDARD ERROR PROCEDURE ON ESTADOS. 
       DC-EST-10. 
           IF NOT FS-EST-PERMITTED 
              DISPLAY 'ERROR ON FILE INFILE, STATUS ' FS-EST 
              STOP RUN. 
       DC-EST-EXIT. 
           EXIT. 
       DC-CONS1 SECTION. 
       USE AFTER STANDARD ERROR PROCEDURE ON CONS1. 
       DC-CONS1-10. 
           IF NOT FS-CONS1-PERMITTED 
              DISPLAY 'ERROR ON FILE INFILE, STATUS ' FS-CONS1 
              STOP RUN. 
       DC-CONS1-EXIT. 
           EXIT. 
       DC-CONS2 SECTION. 
       USE AFTER STANDARD ERROR PROCEDURE ON CONS2. 
       DC-CONS2-10. 
           IF NOT FS-CONS2-PERMITTED 
              DISPLAY 'ERROR ON FILE INFILE, STATUS ' FS-CONS2 
              STOP RUN. 
       DC-CONS2-EXIT. 
           EXIT. 
       DC-CONS3 SECTION. 
       USE AFTER STANDARD ERROR PROCEDURE ON CONS3. 
       DC-CONS3-10. 
           IF NOT FS-CONS3-PERMITTED 
              DISPLAY 'ERROR ON FILE INFILE, STATUS ' FS-CONS3 
              STOP RUN. 
       DC-CONS3-EXIT. 
           EXIT. 
       DC-CUENTAS SECTION.
       USE AFTER STANDARD ERROR PROCEDURE ON CUENTAS. 
       DC-CUENTAS-10. 
           IF NOT FS-CTAS-PERMITTED 
             DISPLAY 'ERROR ON FILE INFILE, STATUS ' FS-CTAS 
             STOP RUN. 
       DC-CUENTAS-EXIT. 
           EXIT. 
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
           perform IMPRIMO-BAJAS-FIN.
           perform MOSTRAR-ESTADISTICAS.
           perform CERRAR-ARCHIVOS.
           STOP RUN.

       INICIALIZAR.
           DISPLAY "INICIALIZAR INICIA".
           MOVE 0 TO cantCons1.
           MOVE 0 TO cantCons2.
           MOVE 0 TO cantCons3.
           MOVE 0 TO bajas.
           MOVE 1 TO cantHojas.
           MOVE 0 TO CONT-ANIO.
           ACCEPT FECHA FROM DATE.
           MOVE FECHA-AA TO PE1-FECHA-AA.
           MOVE FECHA-MM TO PE1-FECHA-MM.
           MOVE FECHA-DD TO PE1-FECHA-DD.
           DISPLAY "INICIALIZAR FIN".
      
       ABRIR-ARCHIVOS.
           DISPLAY "ABRIR-ARCHIVOS INICIA".
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
           OPEN OUTPUT LISTADO.
           DISPLAY "ABRIR-ARCHIVOS FIN".

       GEN-TABLA-ESTADOS.
           DISPLAY "GEN-TABLA-ESTADOS".
           PERFORM LEO-ESTADO.
           PERFORM CARGAR-ESTADO VARYING WS-L-CONT-EST
                   FROM 1 BY 1 
                   UNTIL FS-EST = 10 OR WS-L-CONT-EST > 30.			
           PERFORM ORDENAR-TABLA-ESTADOS.	
		
       CARGAR-ESTADO.
           DISPLAY EST.
           MOVE EST TO TAB-ESTADOS-ELM (WS-L-CONT-EST).
           PERFORM LEO-ESTADO.
			
       LEO-ESTADO.
           DISPLAY "LEO-ESTADO-INICIA".
           READ ESTADOS.
           DISPLAY "FS-EST = " FS-EST.
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
           DISPLAY "OBTENER ESTADO " CON-MENOR-ESTADO "--".
           SET IND TO 1.
           SEARCH ALL TAB-ESTADOS-ELM
                  AT END DISPLAY "Estado no encontrado"                 
                  WHEN TAB-ESTADOS-ESTADO(IND) = CON-MENOR-ESTADO
                       PERFORM OBTENER-INFO-ESTADO
           END-SEARCH.
				
       OBTENER-INFO-ESTADO.
           DISPLAY "OBTENER INFORMACION DEL ESTADO".
           MOVE TAB-ESTADOS-DESCRIP(IND) TO WS-DESCRIP-ESTADO.
		
       IMPRIMO-ENCABEZADO.
           MOVE cantHojas TO PE1-HOJA.
           WRITE LINEA FROM PE1-ENCABE.
           WRITE LINEA FROM PE2-ENCABE.
           WRITE LINEA FROM PE3-ENCABE.
           WRITE LINEA FROM PE2-ENCABE.
           ADD 1 TO cantHojas.
           MOVE 4 TO cantLineas.
       
       LISTAR-BAJA.
           IF cantLineas >= 60 
              PERFORM IMPRIMO-ENCABEZADO.
           PERFORM IMPRIMIR-BAJA.
           ADD 1 TO bajas.

       IMPRIMIR-BAJA-FIN.
           DISPLAY "IMPRIMIR-BAJA".
           MOVE bajas TO PB-FINAL-TOTAL.
           WRITE LINEA FROM PB-FINAL.
           
       IMPRIMIR-BAJA.
           DISPLAY "IMPRIMO-BAJAS".
           WRITE LINEA FROM PB1-BAJA.
           MOVE CON-MENOR-CUIT-CONS TO PB2-BAJA-CUIT-CONS.
           MOVE CON-MENOR-FECHA-ALTA TO PB2-BAJA-FEC-ALTA.
           MOVE CON-MENOR-FECHA-BAJA TO PB2-BAJAR-FEC-BAJA.
           MOVE CON-MENOR-NOMBRE-CONSORCIO TO PB2-BAJA-NOMBRE.
           MOVE CON-MENOR-TEL TO PB2-BAJA-TELEFONO.
           MOVE PB2-BAJA-DIRECCION TO PB2-BAJA-DIRECCION.
           WRITE LINEA FROM PB2-BAJA.
           WRITE LINEA FROM PB3-BAJA.
           WRITE LINEA FROM PE2-ENCABE.
           ADD 4 TO cantLineas.
           
       CICLO-CONSORCIO.
           DISPLAY "CICLO-CONSORCIO".
           PERFORM DET-MENOR.
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
           DISPLAY "MOSTRAR-ESTADISTICAS".
           DISPLAY EST-ENCABEZADO-1.
           DISPLAY EST-ENCABEZADO-2.
           DISPLAY EST-ENCABEZADO-3.
           MOVE 1 TO IND2.			
           PERFORM CICLO-ESTADISTICA-1 UNTIL IND2 > CONT-ANIO.
				
				
       CICLO-ESTADISTICA-1.
           MOVE 1 TO L-CONT-EST.
           MOVE T-EST-COL(IND2, L-CONT-EST) TO L-ANIO.
           PERFORM ARMAR-LINEA-ESTADISTICA.
           DISPLAY LINEA-ESTADISTICA.
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

       DET-MENOR.
           DISPLAY "DET-MENOR".
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
           DISPLAY "El menor es " CON-MENOR.

       POS-CUENTAS.
           DISPLAY "POS-CUENTAS".
           PERFORM  LEO-CUENTAS.

       POS-CONSORN1.			
           MOVE REG-CONS1-FECHA-ALTA TO FEC-ESTADISTICA.
           MOVE REG-CONS1-ESTADO TO EST-ACTUAL.
           PERFORM GENERAR-ESTADISTICAS.
           MOVE REG-CONS1 TO CON-MENOR.
           PERFORM LEO-CONSORCIO-1.
           IF FS-CONS1 = 10 
              MOVE 999999999999999 TO REG-CONS1-CUIT-CONS.           
        
       POS-CONSORN2.
           DISPLAY "Estoy en POS-CONSORN2".
           MOVE REG-CONS2-FECHA-ALTA TO FEC-ESTADISTICA.
           MOVE REG-CONS2-ESTADO TO EST-ACTUAL.
           PERFORM GENERAR-ESTADISTICAS.
           MOVE REG-CONS2 TO CON-MENOR.
           PERFORM LEO-CONSORCIO-2.
           IF FS-CONS2 = 10 
              MOVE 999999999999999 TO REG-CONS2-CUIT-CONS.
        
       POS-CONSORN3.
           MOVE REG-CONS3-FECHA-ALTA TO FEC-ESTADISTICA.
           MOVE REG-CONS3-ESTADO TO EST-ACTUAL.
           PERFORM GENERAR-ESTADISTICAS.
           MOVE REG-CONS3 TO CON-MENOR.
           PERFORM LEO-CONSORCIO-3.
           IF FS-CONS3 = 10 
              MOVE 999999999999999 TO REG-CONS3-CUIT-CONS.
		
       ALTA-MAESTRO.
           DISPLAY "ALTA MAESTRO".
           MOVE CTA-NRO-CTA TO WS-NRO-CTA-AUX.
           IF CON-MENOR-CUIT-CONS NOT EQUAL TO CTA-CUIT-CONS
              MOVE 0 TO WS-NRO-CTA-AUX.
           ADD 1 TO cantConsorcios.
           MOVE CON-MENOR-CUIT-CONS TO  MAE-CUIT-CONS.
           MOVE CON-MENOR-FECHA-ALTA TO  MAE-FECHA-ALTA.
           MOVE WS-DESCRIP-ESTADO TO MAE-DESCRIP-ESTADO.
           MOVE CON-MENOR-NOMBRE-CONSORCIO TO MAE-NOMBRE-CONSORCIO.
           MOVE CON-MENOR-TEL TO MAE-TEL.
           MOVE CON-MENOR-DIR TO MAE-DIR.
           MOVE WS-NRO-CTA-AUX TO  MAE-NRO-CTA.
           WRITE MAE.

       IMPRIMO-BAJAS-FIN.
           DISPLAY "IMPRIMO-BAJAS-FIN".

       GENERAR-ESTADISTICAS.
           DISPLAY "GENERAR-ESTADISTICAS".
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
           ADD 1 TO EST-ACTUAL.
           ADD 1 TO T-EST-COL(IND2, EST-ACTUAL).			
		
       AGREGAR-Y-ACTUALIZAR.
           MOVE ANIO-ESTADISTICA TO T-EST-ANIO(CONT-ANIO).
           ADD 1 TO EST-ACTUAL.
           ADD 1 TO T-EST-COL(CONT-ANIO, EST-ACTUAL).
			
			
				
			
			


	IDENTIFICATION DIVISION.
	PROGRAM-ID. ALGO4.

	ENVIRONMENT DIVISION.
	INPUT-OUTPUT SECTION.
	FILE-CONTROL.
	SELECT CONS1 ASSIGN TO DISK
					ORGANIZATION IS LINE SEQUENTIAL
					FILE STATUS IS FS_CONS1.
	SELECT CONS2 ASSIGN TO DISK
					ORGANIZATION IS LINE SEQUENTIAL
					FILE STATUS IS FS_CONS2.
	SELECT CONS3 ASSIGN TO DISK
					ORGANIZATION IS LINE SEQUENTIAL
					FILE STATUS IS FS_CONS3.
	SELECT CUENTAS ASSIGN TO DISK
					ORGANIZATION IS LINE SEQUENTIAL
					FILE STATUS IS FS_CTAS.
	SELECT ESTADOS ASSIGN TO DISK
					ORGANIZATION IS LINE SEQUENTIAL
					FILE STATUS IS FS_EST.
	SELECT MAESTRO ASSIGN TO DISK
					ORGANIZATION IS LINE SEQUENTIAL
					FILE STATUS IS FS_MAE.
	SELECT LISTADO ASSIGN TO DISK
					ORGANIZATION IS LINE SEQUENTIAL
					FILE STATUS IS FS_LIST.

	DATA DIVISION.
	FILE SECTION.
	FD CONS1
			LABEL RECORD IS STANDARD
			VALUE OF FILE-ID IS "cons1.dat".
			 
	01 CONS_1.	
	   03 CONS_1-CUIT-CONS          PIC 9(15).
	   03 CONS_1-FECHA-ALTA         PIC X(10).
	   03 CONS_1-FECHA-BAJA         PIC X(10).
	   03 CONS_1-ESTADO             PIC 9(02).
	   03 CONS_1-NOMBRE-CONSORCIO   PIC X(30).
	   03 CONS_1-TEL                PIC X(15).
	   03 CONS_1-DIR                PIC X(30).

	FD CONS2
			LABEL RECORD IS STANDARD
			VALUE OF FILE-ID IS "cons2.dat".
	01 CONS_2.	
	   03 CONS_2-CUIT-CONS          PIC 9(15).
	   03 CONS_2-FECHA-ALTA         PIC X(10).
	   03 CONS_2-FECHA-BAJA         PIC X(10).
	   03 CONS_2-ESTADO             PIC 9(02).
	   03 CONS_2-NOMBRE-CONSORCIO   PIC X(30).
	   03 CONS_2-TEL                PIC X(15).
	   03 CONS_2-DIR                PIC X(30).

	FD CONS3
			LABEL RECORD IS STANDARD
			VALUE OF FILE-ID IS "cons3.dat".
	01 CONS_3.	
	   03 CONS_3-CUIT-CONS          PIC 9(15).
	   03 CONS_3-FECHA-ALTA         PIC X(10).
	   03 CONS_3-FECHA-BAJA         PIC X(10).
	   03 CONS_3-ESTADO             PIC 9(02).
	   03 CONS_3-NOMBRE-CONSORCIO   PIC X(30).
	   03 CONS_3-TEL                PIC X(15).
	   03 CONS_3-DIR                PIC X(30).

	FD CUENTAS LABEL RECORD IS STANDARD
	           VALUE OF FILE-ID IS "cuentas.dat".

	01 CTA. 
	   03 CTA-CUIT-CONS           PIC 9(15).
	   03 CTA-NRO-CTA             PIC 9(08).
	   03 CTA-FECHA-ALTA          PIC X(10).
	   03 CTA-ENTIDAD             PIC 9(03).
	   03 CTA-SUCURSAL            PIC 9(03).

	FD ESTADOS LABEL RECORD IS STANDARD
	           VALUE OF FILE-ID IS "estados.dat".   

	01 EST.
	   03 EST-ESTADO              PIC 9(02).
	   03 EST-DESCRIP             PIC X(15).

	FD MAESTRO LABEL RECORD IS STANDARD
	           VALUE OF FILE-ID IS "maestro.dat".

	01 MAE.
	   03 MAE-CUIT-CONS           PIC 9(15).
	   03 MAE-FECHA-ALTA          PIC X(10).
	   03 MAE-DESCRIP-ESTADO      PIC X(15).
	   03 MAE-NOMBRE-CONSORCIO    PIC X(30).
	   03 MAE-TEL                 PIC X(15).
	   03 MAE-DIR                 PIC X(30).
	   03 MAE-NRO-CTA             PIC 9(08).

	FD LISTADO LABEL RECORD IS STANDARD
				VALUE OF FILE-ID IS "listado.dat".
	01 LINEA PIC X(80).

	WORKING-STORAGE SECTION.

	77 FS_CONS1 PIC XX.
	77 FS_CONS2 PIC XX.
	77 FS_CONS3 PIC XX.
	77 FS_CTAS	PIC XX.
	77 FS_EST	PIC XX.
	77 FS_MAE	PIC XX.
	77 FS_LIST	PIC XX.

	77 CUIT-N1          PIC 9(15).
	77 CUIT-N2          PIC 9(15).
	77 CUIT-N3          PIC 9(15).
	
	77 cantConsorcios 	PIC 99 VALUE 0.
	77 bajas 			PIC 99 VALUE 0.
	77 cantLineas 		PIC 99 VALUE 0.
	77 cantHojas 		PIC 99 VALUE 1.
	77 cantCons1 		PIC 99 VALUE 0.
	77 cantCons2 		PIC 99 VALUE 0.
	77 cantCons3 		PIC 99 VALUE 0.
	77 CANTESTADOS 		PIC 99 VALUE 0.
	77 CONT_ANIO 		PIC 99 VALUE 0.
	77 WS_DESCRIP_ESTADO PIC X(15).
	77 WS_CONT_ESTADOS PIC 99.
    01 WS_CONS_MENOR. 
		03 WS_CONS_MENOR-CUIT-CONS          PIC 9(15).
        03 WS_CONS_MENOR-FECHA-ALTA         PIC X(10).
        03 WS_CONS_MENOR-FECHA-BAJA         PIC X(10).
        03 WS_CONS_MENOR-ESTADO             PIC 9(02).		   
        03 WS_CONS_MENOR-NOMBRE-CONSORCIO   PIC X(30).
        03 WS_CONS_MENOR-TEL                PIC X(15).
        03 WS_CONS_MENOR-DIR                PIC X(30).
	01 WS_TABLA_ESTADOS.
		03 WS_TABLA_ESTADOS_ELEMENTO OCCURS 30 TIMES
		ASCENDING KEY IS WS_TABLA_ESTADOS_ESTADO INDEXED BY IND.		   
			05 WS_TABLA_ESTADOS_ESTADO PIC 9(02).
			05 WS_TABLA_ESTADOS_DESCRIP PIC X(15).

	77 CONS1_EOF		PIC XX VALUE "NO".
	77 CONS2_EOF		PIC XX VALUE "NO".
	77 CONS3_EOF		PIC XX VALUE "NO".
	77 CONS_EOF			PIC XX VALUE "NO".
	   88 EOF 				   VALUE "SI".

	PROCEDURE DIVISION.
		perform ABRIR-ARCHIVOS.
		perform GEN-TABLA-ESTADOS.
		perform LEO-CONSORCIOS.
		perform LEO-CUENTAS.
		perform IMPRIMO-ENCABEZADO.
		perform CICLO-CONSORCIO.
		perform IMPRIMO-BAJAS.
		perform MOSTRAR-ESTADISTICAS.
		perform CERRAR-ARCHIVOS.
	STOP RUN.

	ABRIR-ARCHIVOS.
		DISPLAY "ABRIR-ARCHIVOS".
		OPEN INPUT CONS1.
		IF FS_CONS1 NOT = ZERO
			DISPLAY "Error en open CONS1. FS: " FS_CONS1
			STOP RUN.
		OPEN INPUT CONS2.
		IF FS_CONS2 NOT = ZERO
			DISPLAY "Error en open CONS2. FS: " FS_CONS2
			STOP RUN.
		OPEN INPUT CONS3.
		IF FS_CONS3 NOT = ZERO
			DISPLAY "Error en open CONS3. FS: " FS_CONS3
			STOP RUN.
		OPEN INPUT CUENTAS.
		IF FS_CTAS NOT = ZERO
			DISPLAY "Error en open CUENTAS - FS:" FS_CTAS
			STOP RUN.
		OPEN INPUT ESTADOS.
		IF FS_EST NOT = ZERO
			DISPLAY "Error en open ESTADOS - FS:" FS_EST
			STOP RUN.
		OPEN INPUT MAESTRO.
		IF FS_MAE NOT = ZERO
			DISPLAY "Error en open MAESTRO - FS:" FS_MAE
			STOP RUN.
		OPEN OUTPUT LISTADO.

	GEN-TABLA-ESTADOS.
		DISPLAY "GEN-TABLA-ESTADOS".
		PERFORM LEO-ESTADO.
		PERFORM CARGAR-ESTADO VARYING WS_CONT_ESTADOS FROM 1 BY 1
						UNTIL FS_EST = 10 OR WS_CONT_ESTADOS > 30.			
		PERFORM ORDENAR-TABLA-ESTADOS.	
		
	CARGAR-ESTADO.
		MOVE EST TO WS_TABLA_ESTADOS (WS_CONT_ESTADOS).
		PERFORM LEO-ESTADO.
		
	LEO-ESTADO.
		DISPLAY "LEO-ESTADO-INICIA".
			READ ESTADOS.
			IF FS_EST NOT = ZERO
				DISPLAY "Error al leer Archivo de Estados: " FS_EST.
				STOP RUN.			
		
	ORDENAR-TABLA-ESTADOS.
		DISPLAY "ORDENAR TABLA ESTADOS".

	LEO-CONSORCIOS.
		DISPLAY "LEO-CONSORCIOS".
		READ CONS1
			AT END MOVE "SI" TO CONS1_EOF.
		MOVE CONS_1-CUIT-CONS TO CUIT-N1.
		ADD 1 TO cantCons1.

		READ CONS2
			AT END MOVE "SI" TO CONS2_EOF.
		MOVE CONS_2-CUIT-CONS TO CUIT-N2.
		ADD 1 TO cantCons2.

		READ CONS3
			AT END MOVE "SI" TO CONS3_EOF.
		MOVE CONS_3-CUIT-CONS TO CUIT-N3.
		ADD 1 TO cantCons3.

		IF CONS1_EOF = "SI" AND CONS2_EOF = "SI" AND CONS3_EOF = "SI"
			MOVE "SI" TO CONS_EOF.

	LEO-CUENTAS.
		DISPLAY "LEO-CUENTAS".
		
		
	OBTENER-ESTADO.
		DISPLAY "OBTENER ESTADO".
		SEARCH ALL WS_TABLA_ESTADOS
			AT END DISPLAY "Estado no encontrado"
		WHEN WS_TABLA_ESTADOS_ESTADO(IND) = WS_CONS_MENOR_ESTADO
			PERFORM OBTENER-INFO-ESTADO.
		END SEARCH.
				
	OBTENER-INFO-ESTADO
		DISPLAY "OBTENER INFORMACION DEL ESTADO".
		MOVE WS_TABLA_ESTADOS_DESCRIP TO WS_DESCRIP_ESTADO.	
	
	IMPRIMO-ENCABEZADO.
		DISPLAY "IMPRIMO-ENCABEZADO".
	IMPRIMO-BAJAS.
		DISPLAY "IMPRIMO-BAJAS".
	CICLO-CONSORCIO.
		DISPLAY "CICLO-CONSORCIO".
	MOSTRAR-ESTADISTICAS.
		DISPLAY "MOSTRAR-ESTADISTICAS".
	CERRAR-ARCHIVOS.
		DISPLAY "CERRAR-ARCHIVOS".
		CLOSE CONS1.
		CLOSE CONS2.
		CLOSE CONS3.
		CLOSE CUENTAS.
		CLOSE ESTADOS.
		CLOSE MAESTRO.
		CLOSE LISTADO.


----------------------------------------
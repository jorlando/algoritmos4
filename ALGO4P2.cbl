       IDENTIFICATION DIVISION.
       PROGRAM-ID. ALGO4P2.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT MAESTRO ASSIGN TO DISK
                                ORGANIZATION IS LINE SEQUENTIAL
                                FILE STATUS IS FS-MAE.
           SELECT LIS-PROV ASSIGN TO DISK
                                ORGANIZATION IS LINE SEQUENTIAL
                                FILE STATUS IS FS-LIST.
           SELECT CUITPROV ASSIGN TO DISK
                                ORGANIZATION IS INDEXED
                                ACCESS MODE IS DYNAMIC
                                RECORD KEY IS CPR-CLAVE
                                FILE STATUS IS FS-CPR.
           SELECT SD-SORT ASSIGN TO DISK
                                 file STATUS IS FS-SORT.
       
       DATA DIVISION.
       FILE SECTION.

       FD MAESTRO LABEL RECORD IS STANDARD
                  VALUE OF FILE-ID IS 
                  "C:\PROVS\maestro.dat".

       01 MAE.
	   03 MAE-CUIT-CONS               PIC 9(15).
	   03 MAE-FECHA-ALTA              PIC X(10).
	   03 MAE-DESCRIP-ESTADO          PIC X(15).
	   03 MAE-NOMBRE-CONSORCIO        PIC X(30).
	   03 MAE-TEL                     PIC X(15).
	   03 MAE-DIR                     PIC X(30).
	   03 MAE-NRO-CTA                 PIC 9(08).
	
       FD LIS-PROV LABEL RECORD IS STANDARD
                   VALUE OF FILE-ID IS 
                   "C:\PROVS\lprovasi.dat".
       
       01 LINEA                           PIC X(80).

       FD CUITPROV LABEL RECORD IS STANDARD
                   VALUE OF FILE-ID IS 
                   "C:\PROVS\cuitprov.dat".
       
       01 CPR.
           03 CPR-CLAVE.
              05 CPR-CUIT-CONS                PIC 9(15).
              05 CPR-COD-PROV                 PIC 9(08).
           03 CPR-FECHA-ALTA                  PIC 9(08).
   
       SD SD-SORT DATA RECORD IS REG-SORT.
 
       01 REG-SORT.
           03 SD-CLAVE.
              05 SD-RUBRO                    PIC 9(04).
              05 SD-COD-PROV                 PIC 9(08).
              05 SD-CUIT-CONS                PIC 9(15).
           03 SD-DESC-RUBRO               PIC X(15).
           03 SD-NOM-CONS                 PIC X(30).
           03 SD-TEL-CONS                 PIC X(15).
           03 SD-DIR-CONS                 PIC X(30).


       WORKING-STORAGE SECTION.
       77 FS-MAE		PIC XX.
       77 FS-LIST		PIC XX.
       77 FS-CPR                PIC XX.
       77 FS-SORT               PIC XX.
       77 SP-OPT                PIC X.
       77 cantLineas            PIC 99 VALUE 0.
       77 cantHojas 		PIC 99 VALUE 1.
       77 cantProvs             PIC 99 VALUE 0.
       77 cantRubros            PIC 999 VALUE 0.
       77 WS-RUBRO-ACTUAL       PIC 9(04).
       77 RSP-RUBRO             PIC 9(04).
       77 RSP-DESC-RUBRO        PIC X(15).

       01 FECHA.
          03 FECHA-AA    PIC 9(02).
          03 FECHA-MM    PIC 9(02).
          03 FECHA-DD    PIC 9(02).

       01 MAE-ACTUAL.
	   03 MAE-ACTUAL-CUIT-CONS               PIC 9(15).
	   03 MAE-ACTUAL-FECHA-ALTA              PIC X(10).
	   03 MAE-ACTUAL-DESCRIP-ESTADO          PIC X(15).
	   03 MAE-ACTUAL-NOMBRE-CONSORCIO        PIC X(30).
	   03 MAE-ACTUAL-TEL                     PIC X(15).
	   03 MAE-ACTUAL-DIR                     PIC X(30).
	   03 MAE-ACTUAL-NRO-CTA                 PIC 9(08).

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
          03 FILLER   PIC X(24) VALUE ALL ' '.
          03 FILLER   PIC X(31) VALUE 'LISTADO DE PROVEDORES ASIGNADOS'.
          03 FILLER   PIC X(25) VALUE ALL ' '.
                
       01 P-FINAL.
          03 F PIC X(35) 
           	  VALUE 'Total Rubros: '.
          03 P-FINAL-TOTAL PIC 999 VALUE ZERO.
		   

       01 PER1.
          03 FILLER       PIC X(13) VALUE 'RUBRO: '.
          03 PER1-RUBRO   PIC 9(08) VALUE 0.
          03 FILLER   PIC X(20) VALUE ' DESCRIPCION RUBRO: '.
          03 PER1-D-RUBRO PIC X(13).

       01 PER2.
          03 FILLER   PIC X(09) VALUE 'COD-PROV'.
          03 FILLER   PIC X(15) VALUE 'CUIT-CONS'.
          03 FILLER   PIC X(20) VALUE 'NOMBRE-CONS'.
          03 FILLER   PIC X(15) VALUE 'TEL'.
          03 FILLER   PIC X(21) VALUE 'DIRECCION'.
      
       01 PDETR.
          03 PDETR-COD-PROV       PIC 9(08).
          03 PDETR-CUIT-CONS      PIC X(15).
          03 PDETR-NOMBRE-CONS    PIC X(20).
          03 PDETR-TEL            PIC X(15).
          03 PDETR-DIRECCION      PIC X(21).
      
       01 PEPR.
          03 F PIC X(40) 
           VALUE 'TOTAL DE PROVEEDORES POR RUBRO: '.
          03 PEPR-TOTAL PIC 9999 VALUE ZERO.
        
       01 PARAM                 PIC X.
       01 CLAVE                 PIC 9(08).        
       01 RUBRO                 PIC 9(04).
       01 DESCRIP-RUBRO         PIC X(15).
       01 S-ERR                 PIC XX.
        
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
           SORT SD-SORT
                ON ASCENDING KEY SD-CLAVE
                INPUT PROCEDURE PROCESOPROVS
                OUTPUT PROCEDURE LISTADO
           STOP RUN.

       PROCESOPROVS SECTION.
           perform INICIALIZAR.
           perform ABRIR-ARCHIVOS.
           PERFORM ABRIR-PROV.
           PERFORM LEER-MAESTRO.
           PERFORM CICLO-CUIT UNTIL FS-MAE = 10 OR FS-CPR = 10.
           
       PP-GRAL SECTION.
       INICIALIZAR.
      *     DISPLAY "INICIALIZAR INICIA".
           MOVE 1 TO cantHojas.
           MOVE 4 TO cantLineas.
           ACCEPT FECHA FROM DATE.
           MOVE FECHA-AA TO PE1-FECHA-AA.
           MOVE FECHA-MM TO PE1-FECHA-MM.
           MOVE FECHA-DD TO PE1-FECHA-DD.

       ABRIR-ARCHIVOS.
      *     DISPLAY "ABRIR-ARCHIVOS INICIA".
           OPEN INPUT MAESTRO.
           IF FS-MAE NOT = ZERO
              DISPLAY "Err abrir Maestro: " FS-MAE
              STOP RUN.
           OPEN OUTPUT LIS-PROV.
           IF FS-LIST NOT = ZERO
              DISPLAY "Err abrir listado: " FS-LIST
              STOP RUN.
           OPEN INPUT CUITPROV.
           IF FS-CPR NOT = ZERO
              DISPLAY "Err abrir Estadisticas: " FS-CPR
              STOP RUN.    

       ABRIR-PROV.
      * HACER EL LLAMADO AL SUBPROG CON PARAM "A" Y ABRIR
           MOVE 'A' TO PARAM.
           CALL 'ACTPROV' USING PARAM, CLAVE, RUBRO, 
                 DESCRIP-RUBRO, S-ERR.
           IF S-ERR = '01' 
              DISPLAY "Error en subprograma"
              STOP RUN.

       LEER-MAESTRO.
           READ MAESTRO.
      *     DISPLAY "LEO-MAESTRO " FS-MAE.
           IF FS-MAE NOT = ZERO AND 10
              DISPLAY "Err leer maestro " FS-MAE
              STOP RUN.         

       CICLO-CUIT.
           MOVE MAE TO MAE-ACTUAL.
           PERFORM BUSCAR-EN-CPR.
           PERFORM LEER-MAESTRO.

       BUSCAR-EN-CPR.
           MOVE MAE-ACTUAL-CUIT-CONS TO CPR-CUIT-CONS.
           MOVE 00000000 TO CPR-COD-PROV.
           START CUITPROV KEY >= CPR-CLAVE.
           IF NOT FS-CPR EQUAL TO ZERO
              IF FS-CPR = 23
                 DISPLAY "EL CUIT: " CPR-CUIT-CONS "NO EXISTE"
              ELSE 
                 DISPLAY "Err al buscar un cuit" FS-CPR
                 STOP RUN
              END-IF
           ELSE 
              PERFORM PROCESAR-CUIT
           END-IF.

       PROCESAR-CUIT.
           PERFORM LEER-PROX-CPR.
           PERFORM LEER-REGS UNTIL FS-CPR = 10 OR 
                   CPR-CUIT-CONS NOT EQUAL TO 
                   MAE-ACTUAL-CUIT-CONS.
           

       LEER-PROX-CPR.
           READ CUITPROV NEXT RECORD.
           IF FS-CPR NOT = 00 AND FS-CPR NOT = 10
              DISPLAY "Err al leer next" FS-CPR
              STOP RUN
           END-IF.

       LEER-REGS.
           PERFORM ACTUALIZA-PROV.
           PERFORM LIBERAR-REG-SD.
           PERFORM LEER-PROX-CPR.

       LIBERAR-REG-SD.
           MOVE CPR-COD-PROV TO SD-COD-PROV.
           MOVE CPR-CUIT-CONS TO SD-CUIT-CONS.
           MOVE MAE-ACTUAL-NOMBRE-CONSORCIO TO SD-NOM-CONS.
           MOVE MAE-ACTUAL-TEL TO SD-TEL-CONS.
           MOVE MAE-ACTUAL-DIR TO SD-DIR-CONS.
      * ESTAS TIENEN Q VENIR DEL SUBPROGRAMA 
           MOVE 'M' TO PARAM.
           MOVE CPR-COD-PROV TO CLAVE.
           CALL 'ACTPROV' USING PARAM, CLAVE, RUBRO, 
                 DESCRIP-RUBRO, S-ERR.
           IF S-ERR = '01' 
              DISPLAY "Error en subprograma"
              STOP RUN.   
           MOVE RUBRO TO SD-RUBRO.
           MOVE DESCRIP-RUBRO TO SD-DESC-RUBRO.
           RELEASE REG-SORT.

       ACTUALIZA-PROV.
      * HACER EL LLAMADO AL SUBPROGRAMA! PASAR PARAM Y RECIBIRLOS
      * RSP-RUBRO Y RSP-DESC-RUBRO AL MENOS TIENEN Q VOLVER

       LISTADO SECTION.
           perform IMPRIMIR-ENCABEZADO.
           PERFORM LEER-DATOS.
           PERFORM CICLO-GRAL UNTIL FS-SORT = 10.
           IF cantLineas >= 60 
              PERFORM IMPRIMIR-ENCABEZADO.
           perform IMPRIMIR-TOTAL-FINAL.
           PERFORM CERRAR-PROV.
           perform CERRAR-ARCHIVOS.   
           STOP RUN.    


       OTROS2 SECTION.
       LEER-DATOS.
           RETURN SD-SORT RECORD 
                  AT END  MOVE 10 TO FS-SORT.

       CERRAR-PROV.
      * HACER LLAMADO A SUBPROG CON PARAM "C" Y CERRAR ARCHIVO
           MOVE 'C' TO PARAM.
           CALL 'ACTPROV' USING PARAM, CLAVE, RUBRO, 
                 DESCRIP-RUBRO, S-ERR.
           IF S-ERR = '01' 
              DISPLAY "Error en subprograma"
              STOP RUN.

       CERRAR-ARCHIVOS.
      *     DISPLAY "CERRAR ARCHS PPAL".
           CLOSE MAESTRO.
           CLOSE LIS-PROV.
           CLOSE CUITPROV.

       CICLO-GRAL.
           ADD 1 to cantRubros.
           MOVE 1 TO cantProvs.
           MOVE SD-RUBRO TO WS-RUBRO-ACTUAL.
           IF cantLineas >= 60 
              PERFORM IMPRIMIR-ENCABEZADO.
           PERFORM IMPRIMIR-ENC-RUBRO.
           PERFORM PROCESAR-SIGUIENTE UNTIL 
                       WS-RUBRO-ACTUAL NOT = SD-RUBRO 
                       OR FS-SORT = 10.
           IF cantLineas >= 60 
              PERFORM IMPRIMIR-ENCABEZADO.
           PERFORM IMPRIMIR-TOTAL-RUBRO.
 
       PROCESAR-SIGUIENTE.
           IF cantLineas >= 60 
              PERFORM IMPRIMIR-ENCABEZADO.
           PERFORM IMPRIMIR-DET-PROV.
           PERFORM LEER-DATOS.    
           IF WS-RUBRO-ACTUAL = SD-RUBRO
              ADD 1 TO cantProvs.

       IMPRIMIR-ENCABEZADO.
           MOVE cantHojas TO PE1-HOJA.
           WRITE LINEA FROM PE1-ENCABE.
           PERFORM CHECK-WRITE-LIS-PROV.
           WRITE LINEA FROM PE2-ENCABE.
           PERFORM CHECK-WRITE-LIS-PROV.
           WRITE LINEA FROM PE3-ENCABE.
           PERFORM CHECK-WRITE-LIS-PROV.
           WRITE LINEA FROM PE2-ENCABE.
           PERFORM CHECK-WRITE-LIS-PROV.
           ADD 1 TO cantHojas.
           MOVE 4 TO cantLineas.

       IMPRIMIR-TOTAL-FINAL.
      *     DISPLAY "IMPRIMIR-TOTAL-FINAL" cantRubros.
           MOVE cantRubros TO P-FINAL-TOTAL.
           WRITE LINEA FROM P-FINAL.
           PERFORM CHECK-WRITE-LIS-PROV.

       IMPRIMIR-ENC-RUBRO.
           MOVE SD-RUBRO TO PER1-RUBRO.
           MOVE SD-DESC-RUBRO TO PER1-D-RUBRO.
           WRITE LINEA FROM PER1.
           PERFORM CHECK-WRITE-LIS-PROV.
           WRITE LINEA FROM PER2.
           PERFORM CHECK-WRITE-LIS-PROV.
           ADD 2 TO cantLineas.

       IMPRIMIR-DET-PROV.
           MOVE SD-COD-PROV TO PDETR-COD-PROV.
           MOVE SD-CUIT-CONS TO PDETR-CUIT-CONS.
           MOVE SD-NOM-CONS TO PDETR-NOMBRE-CONS.
           MOVE SD-TEL-CONS TO PDETR-TEL.
           MOVE SD-DIR-CONS TO PDETR-DIRECCION.
           WRITE LINEA FROM PDETR.
           PERFORM CHECK-WRITE-LIS-PROV.
           ADD 1 TO cantLineas.

       IMPRIMIR-TOTAL-RUBRO.
           MOVE cantProvs TO PEPR-TOTAL.
           WRITE LINEA FROM PEPR.
           PERFORM CHECK-WRITE-LIS-PROV.
           ADD 1 TO cantLineas.

       CHECK-WRITE-LIS-PROV.
           IF FS-LIST NOT = ZERO AND 10
              DISPLAY "Error al escribir LIS-PROV: " FS-LIST
              STOP RUN.

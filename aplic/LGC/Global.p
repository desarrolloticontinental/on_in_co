DEFINE NEW GLOBAL SHARED VARIABLE S-CODALM  AS CHARACTER FORMAT "X(3)".
DEFINE NEW GLOBAL SHARED VARIABLE S-DESALM  AS CHARACTER.
DEFINE NEW GLOBAL SHARED VARIABLE S-CODDIV  AS CHARACTER.
DEFINE NEW GLOBAL SHARED VARIABLE lh_Handle AS HANDLE.
DEFINE NEW GLOBAL SHARED VARIABLE S-CODCIA  AS INTEGER INIT 1.
DEFINE NEW GLOBAL SHARED VARIABLE S-NOMCIA  AS CHAR INIT "CONTINENTAL S.A.C.".
DEFINE NEW GLOBAL SHARED VARIABLE S-USER-ID AS CHAR INIT 'MASTER'.
DEFINE NEW GLOBAL SHARED VARIABLE S-NROSER  AS INTEGER INIT 1.
DEFINE NEW GLOBAL SHARED VARIABLE S-MOVVAL  AS LOGICAL.
DEFINE NEW GLOBAL SHARED VARIABLE S-CODFAM  AS CHAR.
DEFINE NEW GLOBAL SHARED VARIABLE S-CODDOC  AS CHAR.
DEFINE NEW GLOBAL SHARED VARIABLE S-FLGSIT  AS CHAR.
DEFINE NEW GLOBAL SHARED VARIABLE S-TPODOC AS CHAR INIT "N".
DEFINE NEW GLOBAL SHARED VARIABLE S-PORCFR AS DEC INIT 10.
DEFINE NEW GLOBAL SHARED VARIABLE S-TABLA  AS CHAR.
DEFINE NEW GLOBAL SHARED VARIABLE CL-CODCIA  AS INTEGER INIT 0.
DEFINE NEW GLOBAL SHARED VARIABLE PV-CODCIA  AS INTEGER INIT 0.
DEFINE NEW GLOBAL SHARED VARIABLE s-adm-new-record AS CHAR.

DEFINE NEW GLOBAL SHARED VARIABLE S-PROVEE  AS CHAR.
DEFINE NEW GLOBAL SHARED VARIABLE S-TPOCMB  AS DECIMAL.
DEFINE NEW GLOBAL SHARED VARIABLE S-CODMON  AS INTEGER.
DEFINE NEW GLOBAL SHARED VARIABLE S-AFTIGV  AS LOG.
DEFINE NEW GLOBAL SHARED VARIABLE S-CODPRO  AS CHAR.
DEFINE NEW GLOBAL SHARED VARIABLE s-HojaRuta AS LOG.
DEFINE NEW GLOBAL SHARED VARIABLE s-codmat AS CHAR.
DEFINE NEW GLOBAL SHARED VARIABLE s-nroo_c AS INT.
DEFINE NEW GLOBAL SHARED VARIABLE s-codmov AS INT.
DEFINE NEW GLOBAL SHARED VARIABLE s-codcli AS CHAR.

DEFINE NEW GLOBAL SHARED TEMP-TABLE DCMP LIKE LG-DOCmp.
DEFINE NEW GLOBAL SHARED TEMP-TABLE ITEM LIKE integral.almdmov.
DEFINE NEW GLOBAL SHARED TEMP-TABLE ITEM2 LIKE integral.lg-liqcsgd.
DEFINE NEW GLOBAL SHARED TEMP-TABLE DREP LIKE integral.Almdrequ.
DEFINE NEW GLOBAL SHARED TEMP-TABLE DREQ LIKE integral.LG-DREQU.
DEFINE NEW GLOBAL SHARED TEMP-TABLE T-DOSER LIKE LG-DOSER.
DEFINE NEW GLOBAL SHARED TEMP-TABLE RUTA LIKE Di-RutaC.
DEFINE NEW GLOBAL SHARED TEMP-TABLE T-MATG NO-UNDO LIKE Almmmatg.

dEFINE NEW GLOBAL SHARED VARIABLE input-var-1 AS CHARACTER.
DEFINE NEW GLOBAL SHARED VARIABLE input-var-2 AS CHARACTER.
DEFINE NEW GLOBAL SHARED VARIABLE input-var-3 AS CHARACTER.
DEFINE NEW GLOBAL SHARED VARIABLE output-var-1 AS ROWID.
DEFINE NEW GLOBAL SHARED VARIABLE output-var-2 AS CHARACTER.
DEFINE NEW GLOBAL SHARED VARIABLE output-var-3 AS CHARACTER.
DEFINE NEW GLOBAL SHARED VARIABLE s-tipo       AS CHARACTER.
DEFINE NEW GLOBAL SHARED VARIABLE s-contrato-marco AS LOG.

DEFINE NEW GLOBAL SHARED TEMP-TABLE t-prev 
      FIELD Tipo AS CHAR
      FIELD Subalm AS CHAR
      FIELD Codmov AS INTEGER
      FIELD NroGui AS CHAR
      FIELD Periodo LIKE cb-dmov.periodo
      FIELD NroMes LIKE cb-dmov.nromes
      FIELD Codope LIKE cb-dmov.codope
      FIELD Codcta LIKE cb-dmov.codcta
      FIELD CodDiv LIKE cb-dmov.coddiv
      FIELD Codmon LIKE cb-dmov.codmon
      FIELD Fchdoc LIKE cb-dmov.fchdoc
      FIELD Coddoc LIKE cb-dmov.coddoc
      FIELD Nrodoc LIKE cb-dmov.nrodoc
      FIELD Glodoc LIKE cb-dmov.glodoc
      FIELD Tpocmb LIKE cb-dmov.tpocmb
      FIELD TpoMov LIKE cb-dmov.tpomov
      FIELD ImpMn1 LIKE cb-dmov.impmn1 
      FIELD ImpMn2 LIKE cb-dmov.impmn2
      FIELD cco    LIKE cb-dmov.cco  
      FIELD clfaux LIKE cb-dmov.Clfaux
      FIELD codaux LIKE cb-dmov.Codaux
      FIELD Codref LIKE cb-dmov.codref
      FIELD Nroref LIKE cb-dmov.nroref
      INDEX IDX01 Tipo.






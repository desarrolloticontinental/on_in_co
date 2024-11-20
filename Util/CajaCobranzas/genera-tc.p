DEF VAR s-user-id AS CHAR INIT 'ADMIN'.
DEFINE BUFFER b_pen FOR CcbPenDep.

CREATE ccbpendep.
ASSIGN
    CcbPenDep.CodCia = 001
    CcbPenDep.CodDoc = "TCR"
    CcbPenDep.CodDiv = '00502'
    CcbPenDep.CodRef = 'I/C'
    CcbPenDep.NroRef = '512073741'
    CcbPenDep.FchCie = 10/20/2015
    CcbPenDep.NroDoc = "01 VISA|377752xxxxxx5162"
    CcbPenDep.FlgEst = "P"
    CcbPenDep.HorCie = '12:00'
    CcbPenDep.usuario = 'ADMIN'.
ASSIGN
    CcbPenDep.ImpNac = 34.90
    CcbPenDep.SdoNac = 34.90.

RUN proc_Genera-Deposito.



PROCEDURE proc_Genera-Deposito:

    DEF VAR F-Banco AS CHAR NO-UNDO.
    DEF VAR F-Cta AS CHAR NO-UNDO.
    DEF VAR FILL-IN-NroOpe AS CHAR NO-UNDO.
    DEF VAR F-Fecha AS DATE NO-UNDO.

    ASSIGN F-Fecha = TODAY.

    {ccb/i-pendep-02.i}



END PROCEDURE.

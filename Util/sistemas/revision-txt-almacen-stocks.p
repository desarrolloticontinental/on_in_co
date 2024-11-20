DEF TEMP-TABLE t1 LIKE almacen_stocks.
DEF TEMP-TABLE t2 LIKE almacen_stocks.

INPUT FROM d:\almacen_stocks2.txt.
REPEAT :
    CREATE t1.
    IMPORT DELIMITER "|" t1.
END.
INPUT CLOSE.

INPUT FROM d:\almacen_stocks3.txt.
REPEAT :
    CREATE t2.
    IMPORT DELIMITER "|" t2.               
END.
INPUT CLOSE.


FOR EACH almacen_stocks NO-LOCK WHERE almacen_stocks.codalm > '',
    FIRST t1 NO-LOCK WHERE t1.codalm = almacen_stocks.codalm AND
    t1.codmat = almacen_stocks.codmat:
    IF almacen_stocks.stkact <> t1.stkact THEN
        DISPLAY almacen_stocks.codalm almacen_stocks.codmat almacen_stocks.stkact t1.stkact
        WITH STREAM-IO NO-BOX WIDTH 320.

END.

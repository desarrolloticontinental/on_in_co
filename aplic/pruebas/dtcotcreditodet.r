	��VS]Y�9  ; �                                              Uz 398800EFutf-8 MAIN C:\newsie\on_in_co\aplic\pruebas\dtcotcreditodet.w,, PROCEDURE disable_UI,, PROCEDURE serverCommit,,INPUT-OUTPUT RowObjUpd TABLE,OUTPUT pcMessages CHARACTER,OUTPUT pcUndoIds CHARACTER PROCEDURE remoteCommit,,INPUT-OUTPUT pcContext CHARACTER,INPUT-OUTPUT RowObjUpd TABLE,OUTPUT pcMessages CHARACTER,OUTPUT pcUndoIds CHARACTER PROCEDURE pushTableAndValidate,,INPUT pcValType CHARACTER,INPUT-OUTPUT RowObjUpd TABLE PROCEDURE pushRowObjUpdTable,,INPUT RowObjUpd TABLE PROCEDURE initProps,, PROCEDURE start-super-proc,,INPUT pcProcName CHARACTER PROCEDURE adm-clone-props,, PROCEDURE viewObject,, PROCEDURE toggleData,,INPUT plEnabled LOGICAL PROCEDURE showMessageProcedure,,INPUT pcMessage CHARACTER,OUTPUT plAnswer LOGICAL PROCEDURE returnFocus,,INPUT hTarget HANDLE PROCEDURE repositionObject,,INPUT pdRow DECIMAL,INPUT pdCol DECIMAL PROCEDURE removeLink,,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE PROCEDURE removeAllLinks,, PROCEDURE modifyUserLinks,,INPUT pcMod CHARACTER,INPUT pcLinkName CHARACTER,INPUT phObject HANDLE PROCEDURE modifyListProperty,,INPUT phCaller HANDLE,INPUT pcMode CHARACTER,INPUT pcListName CHARACTER,INPUT pcListValue CHARACTER PROCEDURE hideObject,, PROCEDURE exitObject,, PROCEDURE editInstanceProperties,, PROCEDURE displayLinks,, PROCEDURE createControls,, PROCEDURE changeCursor,,INPUT pcCursor CHARACTER PROCEDURE applyEntry,,INPUT pcField CHARACTER PROCEDURE adjustTabOrder,,INPUT phObject HANDLE,INPUT phAnchor HANDLE,INPUT pcPosition CHARACTER PROCEDURE addMessage,,INPUT pcText CHARACTER,INPUT pcField CHARACTER,INPUT pcTable CHARACTER PROCEDURE addLink,,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE PROCEDURE unbindServer,,INPUT pcMode CHARACTER PROCEDURE runServerObject,,INPUT phAppService HANDLE PROCEDURE disconnectObject,, PROCEDURE destroyObject,, PROCEDURE bindServer,, PROCEDURE transferDBRow,,INPUT pcRowIdent CHARACTER,INPUT piRowNum INTEGER PROCEDURE startFilter,, PROCEDURE releaseDBRow,, PROCEDURE refetchDBRow,,INPUT phRowObjUpd HANDLE PROCEDURE filterContainerHandler,,INPUT phFilterContainer HANDLE PROCEDURE fetchDBRowForUpdate,, PROCEDURE confirmContinue,,INPUT-OUTPUT pioCancel LOGICAL PROCEDURE compareDBRow,, PROCEDURE bufferCopyDBToRO,,INPUT phRowObj HANDLE,INPUT phBuffer HANDLE,INPUT pcExcludes CHARACTER,INPUT pcAssigns CHARACTER PROCEDURE assignDBRow,,INPUT phRowObjUpd HANDLE PROCEDURE updateState,,INPUT pcState CHARACTER PROCEDURE updateQueryPosition,, PROCEDURE updateAddQueryWhere,,INPUT pcWhere CHARACTER,INPUT pcField CHARACTER PROCEDURE undoTransaction,, PROCEDURE transferToExcel,,INPUT pcFieldList CHARACTER,INPUT plIncludeObj LOGICAL,INPUT plUseExisting LOGICAL,INPUT piMaxRecords INTEGER PROCEDURE synchronizeProperties,,INPUT pcPropertiesForServer CHARACTER,OUTPUT pcPropertiesForClient CHARACTER PROCEDURE submitValidation,,INPUT pcValueList CHARACTER,INPUT pcUpdColumns CHARACTER PROCEDURE submitForeignKey,,INPUT pcRowIdent CHARACTER,INPUT-OUTPUT pcValueList CHARACTER,INPUT-OUTPUT pcUpdColumns CHARACTER PROCEDURE submitCommit,,INPUT pcRowIdent CHARACTER,INPUT plReopen LOGICAL PROCEDURE startServerObject,, PROCEDURE setPropertyList,,INPUT pcProperties CHARACTER PROCEDURE serverFetchRowObjUpdTable,,OUTPUT phRowObjUpd TABLE-HANDLE PROCEDURE serverSendRows,,INPUT piStartRow INTEGER,INPUT pcRowIdent CHARACTER,INPUT plNext LOGICAL,INPUT piRowsToReturn INTEGER,OUTPUT piRowsReturned INTEGER,OUTPUT phRowObject TABLE-HANDLE PROCEDURE saveContextAndDestroy,,OUTPUT pcContext CHARACTER PROCEDURE rowObjectState,,INPUT pcState CHARACTER PROCEDURE retrieveFilter,, PROCEDURE restartServerObject,, PROCEDURE remoteSendRows,,INPUT-OUTPUT piocContext CHARACTER,INPUT piStartRow INTEGER,INPUT pcRowIdent CHARACTER,INPUT plNext LOGICAL,INPUT piRowsToReturn INTEGER,OUTPUT pioRowsReturned INTEGER,OUTPUT phRowObject TABLE-HANDLE,OUTPUT pocMessages CHARACTER PROCEDURE refreshRow,, PROCEDURE printToCrystal,,INPUT pcFieldList CHARACTER,INPUT plIncludeObj LOGICAL,INPUT piMaxRecords INTEGER PROCEDURE isUpdatePending,,INPUT-OUTPUT plUpdate LOGICAL PROCEDURE initializeServerObject,, PROCEDURE initializeObject,, PROCEDURE home,, PROCEDURE genContextList,,OUTPUT pcContext CHARACTER PROCEDURE fetchPrev,, PROCEDURE fetchNext,, PROCEDURE fetchLast,, PROCEDURE fetchFirst,, PROCEDURE fetchBatch,,INPUT plForwards LOGICAL PROCEDURE endClientDataRequest,, PROCEDURE destroyServerObject,, PROCEDURE describeSchema,,INPUT pcSdoName CHARACTER,OUTPUT hTtSchema TABLE-HANDLE PROCEDURE dataAvailable,,INPUT pcRelative CHARACTER PROCEDURE copyColumns,,INPUT pcViewColList CHARACTER,INPUT phDataQuery HANDLE PROCEDURE commitTransaction,, PROCEDURE clientSendRows,,INPUT piStartRow INTEGER,INPUT pcRowIdent CHARACTER,INPUT plNext LOGICAL,INPUT piRowsToReturn INTEGER,OUTPUT piRowsReturned INTEGER PROCEDURE batchServices,,INPUT pcServices CHARACTER,OUTPUT pcValues CHARACTER FUNCTION deleteRecordStatic,logical,INPUT piTableIndex INTEGER FUNCTION getRowObjUpdStatic,widget-handle, FUNCTION Signature,CHARACTER,INPUT pcName CHARACTER FUNCTION showmessage,LOGICAL,INPUT pcMessage CHARACTER FUNCTION setUserProperty,LOGICAL,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER FUNCTION setUIBMode,LOGICAL,INPUT pcMode CHARACTER FUNCTION setTranslatableProperties,LOGICAL,INPUT pcPropList CHARACTER FUNCTION setSupportedLinks,LOGICAL,INPUT pcLinkList CHARACTER FUNCTION setRunAttribute,LOGICAL,INPUT cRunAttribute CHARACTER FUNCTION setPhysicalVersion,LOGICAL,INPUT cVersion CHARACTER FUNCTION setPhysicalObjectName,LOGICAL,INPUT cTemp CHARACTER FUNCTION setPassThroughLinks,LOGICAL,INPUT pcLinks CHARACTER FUNCTION setParentDataKey,LOGICAL,INPUT cParentDataKey CHARACTER FUNCTION setObjectVersion,LOGICAL,INPUT cObjectVersion CHARACTER FUNCTION setObjectParent,LOGICAL,INPUT phParent HANDLE FUNCTION setObjectName,LOGICAL,INPUT pcName CHARACTER FUNCTION setLogicalVersion,LOGICAL,INPUT cVersion CHARACTER FUNCTION setLogicalObjectName,LOGICAL,INPUT c CHARACTER FUNCTION setInstanceProperties,LOGICAL,INPUT pcPropList CHARACTER FUNCTION setDynamicObject,LOGICAL,INPUT lTemp LOGICAL FUNCTION setDesignDataObject,LOGICAL,INPUT pcDataObject CHARACTER FUNCTION setDBAware,LOGICAL,INPUT lAware LOGICAL FUNCTION setDataTargetEvents,LOGICAL,INPUT pcEvents CHARACTER FUNCTION setDataTarget,LOGICAL,INPUT pcTarget CHARACTER FUNCTION setDataSourceNames,LOGICAL,INPUT pcSourceNames CHARACTER FUNCTION setDataSourceEvents,LOGICAL,INPUT pcEventsList CHARACTER FUNCTION setDataSource,LOGICAL,INPUT phObject HANDLE FUNCTION setDataLinksEnabled,LOGICAL,INPUT lDataLinksEnabled LOGICAL FUNCTION setContainerSourceEvents,LOGICAL,INPUT pcEvents CHARACTER FUNCTION setContainerSource,LOGICAL,INPUT phObject HANDLE FUNCTION setContainerHidden,LOGICAL,INPUT plHidden LOGICAL FUNCTION setChildDataKey,LOGICAL,INPUT cChildDataKey CHARACTER FUNCTION reviewMessages,CHARACTER, FUNCTION propertyType,CHARACTER,INPUT pcPropName CHARACTER FUNCTION messageNumber,CHARACTER,INPUT piMessage INTEGER FUNCTION mappedEntry,CHARACTER,INPUT pcEntry CHARACTER,INPUT pcList CHARACTER,INPUT plFirst LOGICAL,INPUT pcDelimiter CHARACTER FUNCTION linkProperty,CHARACTER,INPUT pcLink CHARACTER,INPUT pcPropName CHARACTER FUNCTION linkHandles,CHARACTER,INPUT pcLink CHARACTER FUNCTION instancePropertyList,CHARACTER,INPUT pcPropList CHARACTER FUNCTION getUserProperty,CHARACTER,INPUT pcPropName CHARACTER FUNCTION getUIBMode,CHARACTER, FUNCTION getTranslatableProperties,CHARACTER, FUNCTION getSupportedLinks,CHARACTER, FUNCTION getRunAttribute,CHARACTER, FUNCTION getQueryObject,LOGICAL, FUNCTION getPropertyDialog,CHARACTER, FUNCTION getPhysicalVersion,CHARACTER, FUNCTION getPhysicalObjectName,CHARACTER, FUNCTION getPassThroughLinks,CHARACTER, FUNCTION getParentDataKey,CHARACTER, FUNCTION getObjectVersionNumber,CHARACTER, FUNCTION getObjectVersion,CHARACTER, FUNCTION getObjectParent,HANDLE, FUNCTION getObjectPage,INTEGER, FUNCTION getObjectName,CHARACTER, FUNCTION getObjectInitialized,LOGICAL, FUNCTION getObjectHidden,LOGICAL, FUNCTION getLogicalVersion,CHARACTER, FUNCTION getLogicalObjectName,CHARACTER, FUNCTION getInstanceProperties,CHARACTER, FUNCTION getDynamicObject,LOGICAL, FUNCTION getDesignDataObject,CHARACTER, FUNCTION getDBAware,LOGICAL, FUNCTION getDataTargetEvents,CHARACTER, FUNCTION getDataTarget,CHARACTER, FUNCTION getDataSourceNames,CHARACTER, FUNCTION getDataSourceEvents,CHARACTER, FUNCTION getDataSource,HANDLE, FUNCTION getDataLinksEnabled,LOGICAL, FUNCTION getContainerType,CHARACTER, FUNCTION getContainerSourceEvents,CHARACTER, FUNCTION getContainerSource,HANDLE, FUNCTION getContainerHidden,LOGICAL, FUNCTION getContainerHandle,HANDLE, FUNCTION getChildDataKey,CHARACTER, FUNCTION fetchMessages,CHARACTER, FUNCTION assignLinkProperty,LOGICAL,INPUT pcLink CHARACTER,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER FUNCTION anyMessage,LOGICAL, FUNCTION setServerOperatingMode,LOGICAL,INPUT pcServerOperatingMode CHARACTER FUNCTION setServerFileName,LOGICAL,INPUT pcFileName CHARACTER FUNCTION setASUsePrompt,LOGICAL,INPUT plFlag LOGICAL FUNCTION setASInitializeOnRun,LOGICAL,INPUT plInitialize LOGICAL FUNCTION setASInfo,LOGICAL,INPUT pcInfo CHARACTER FUNCTION setASHandle,LOGICAL,INPUT phASHandle HANDLE FUNCTION setASDivision,LOGICAL,INPUT pcDivision CHARACTER FUNCTION setAppService,LOGICAL,INPUT pcAppService CHARACTER FUNCTION runServerProcedure,HANDLE,INPUT pcServerFileName CHARACTER,INPUT phAppService HANDLE FUNCTION getServerOperatingMode,CHARACTER, FUNCTION getServerFileName,CHARACTER, FUNCTION getASUsePrompt,LOGICAL, FUNCTION getASInitializeOnRun,LOGICAL, FUNCTION getASInfo,CHARACTER, FUNCTION getASHasStarted,LOGICAL, FUNCTION getASHandle,HANDLE, FUNCTION getAsDivision,CHARACTER, FUNCTION getASBound,LOGICAL, FUNCTION getAppService,CHARACTER, FUNCTION whereClauseBuffer,CHARACTER,INPUT pcWhere CHARACTER FUNCTION setQueryWhere,LOGICAL,INPUT pcWhere CHARACTER FUNCTION setQueryString,LOGICAL,INPUT pcQueryString CHARACTER FUNCTION setQuerySort,LOGICAL,INPUT pcSort CHARACTER FUNCTION setQueryPosition,LOGICAL,INPUT pcPosition CHARACTER FUNCTION rowidWhereCols,CHARACTER,INPUT pcColumns CHARACTER,INPUT pcValues CHARACTER,INPUT pcOperators CHARACTER FUNCTION rowidWhere,CHARACTER,INPUT pcWhere CHARACTER FUNCTION resolveBuffer,CHARACTER,INPUT pcBuffer CHARACTER FUNCTION removeQuerySelection,LOGICAL,INPUT pcColumns CHARACTER,INPUT pcOperators CHARACTER FUNCTION removeForeignKey,LOGICAL, FUNCTION refreshRowident,CHARACTER,INPUT pcRowident CHARACTER FUNCTION newWhereClause,CHARACTER,INPUT pcBuffer CHARACTER,INPUT pcExpression CHARACTER,INPUT pcWhere CHARACTER,INPUT pcAndOr CHARACTER FUNCTION newQueryWhere,CHARACTER,INPUT pcWhere CHARACTER FUNCTION newQueryValidate,CHARACTER,INPUT pcQueryString CHARACTER,INPUT pcExpression CHARACTER,INPUT pcBuffer CHARACTER,INPUT pcAndOr CHARACTER FUNCTION newQueryString,CHARACTER,INPUT pcColumns CHARACTER,INPUT pcValues CHARACTER,INPUT pcOperators CHARACTER,INPUT pcQueryString CHARACTER,INPUT pcAndOr CHARACTER FUNCTION insertExpression,CHARACTER,INPUT pcWhere CHARACTER,INPUT pcExpression CHARACTER,INPUT pcAndOr CHARACTER FUNCTION indexInformation,CHARACTER,INPUT pcQuery CHARACTER,INPUT plUseTableSep LOGICAL,INPUT pcIndexInfo CHARACTER FUNCTION getTargetProcedure,HANDLE, FUNCTION getQueryWhere,CHARACTER, FUNCTION getQueryString,CHARACTER, FUNCTION getQuerySort,CHARACTER, FUNCTION getQueryPosition,CHARACTER, FUNCTION getForeignValues,CHARACTER, FUNCTION getDataColumns,CHARACTER, FUNCTION excludeColumns,CHARACTER,INPUT iTable INTEGER FUNCTION dbColumnHandle,HANDLE,INPUT pcColumn CHARACTER FUNCTION dbColumnDataName,CHARACTER,INPUT pcDbColumn CHARACTER FUNCTION columnValMsg,CHARACTER,INPUT pcColumn CHARACTER FUNCTION columnTable,CHARACTER,INPUT pcColumn CHARACTER FUNCTION columnQuerySelection,CHARACTER,INPUT pcColumn CHARACTER FUNCTION columnDbColumn,CHARACTER,INPUT pcColumn CHARACTER FUNCTION columnDataType,CHARACTER,INPUT pcColumn CHARACTER FUNCTION bufferWhereClause,CHARACTER,INPUT pcBuffer CHARACTER,INPUT pcWhere CHARACTER FUNCTION bufferCompareDBToRO,LOGICAL,INPUT phRowObjUpd HANDLE,INPUT phBuffer HANDLE,INPUT pcExcludes CHARACTER,INPUT pcAssigns CHARACTER FUNCTION assignQuerySelection,LOGICAL,INPUT pcColumns CHARACTER,INPUT pcValues CHARACTER,INPUT pcOperators CHARACTER FUNCTION addQueryWhere,LOGICAL,INPUT pcWhere CHARACTER,INPUT pcBuffer CHARACTER,INPUT pcAndOr CHARACTER FUNCTION getObjectType,character, FUNCTION updateRow,LOGICAL,INPUT pcKeyValues CHARACTER,INPUT pcValueList CHARACTER FUNCTION submitRow,LOGICAL,INPUT pcRowIdent CHARACTER,INPUT pcValueList CHARACTER FUNCTION rowValues,CHARACTER,INPUT pcColumns CHARACTER,INPUT pcFormat CHARACTER,INPUT pcDelimiter CHARACTER FUNCTION rowAvailable,LOGICAL,INPUT pcDirection CHARACTER FUNCTION prepareQuery,LOGICAL,INPUT pcQuery CHARACTER FUNCTION openQuery,LOGICAL, FUNCTION openDataQuery,LOGICAL,INPUT pcPosition CHARACTER FUNCTION hasForeignKeyChanged,LOGICAL, FUNCTION getLastCommitErrorType,CHARACTER, FUNCTION firstRowIds,CHARACTER,INPUT pcQueryString CHARACTER FUNCTION findRowWhere,LOGICAL,INPUT pcColumns CHARACTER,INPUT pcValues CHARACTER,INPUT pcOperators CHARACTER FUNCTION findRow,LOGICAL,INPUT pcKeyValues CHARACTER FUNCTION fetchRowIdent,CHARACTER,INPUT pcRowIdent CHARACTER,INPUT pcViewColList CHARACTER FUNCTION fetchRow,CHARACTER,INPUT piRow INTEGER,INPUT pcViewColList CHARACTER FUNCTION deleteRow,LOGICAL,INPUT pcRowIdent CHARACTER FUNCTION createRow,LOGICAL,INPUT pcValueList CHARACTER FUNCTION copyRow,CHARACTER,INPUT pcViewColList CHARACTER FUNCTION colValues,CHARACTER,INPUT pcViewColList CHARACTER FUNCTION columnProps,CHARACTER,INPUT pcColList CHARACTER,INPUT pcPropList CHARACTER FUNCTION closeQuery,LOGICAL, FUNCTION canNavigate,LOGICAL, FUNCTION cancelRow,CHARACTER, FUNCTION addRow,CHARACTER,INPUT pcViewColList CHARACTER TEMP-TABLE RowObjUpd 0,RowNum:RowNum 0 NO,AftIgv logical 0 0,AftIsc logical 1 0,CanPed decimal 2 0,CodCia integer 3 0,CodCli character 4 0,CodDiv character 5 0,CodDoc character 6 0,codmat character 7 0,Factor decimal 8 0,FchPed date 9 0,FlgEst character 10 0,Hora character 11 0,ImpDto decimal 12 0,ImpDto2 decimal 13 0,ImpIgv decimal 14 0,ImpIsc decimal 15 0,ImpLin decimal 16 0,Libre_c05 character 17 0,MrgUti decimal 18 0,NroItm integer 19 0,NroPed character 20 0,PorDto decimal 21 0,PorDto2 decimal 22 0,Por_Dsctos1 decimal 23 0,Por_Dsctos2 decimal 24 0,Por_Dsctos3 decimal 25 0,PreBas decimal 26 0,PreUni decimal 27 0,PreVta1 decimal 28 0,PreVta2 decimal 29 0,PreVta3 decimal 30 0,UndVta character 31 0,CanApr decimal 32 0,DesMat character 33 0,DesMar character 34 0,RowNum integer 35 0,RowIdent character 36 0,RowMod character 37 0,RowIdentIdx character 38 0,RowUserProp character 39 0,ChangedFields character 40 0       `              �L             �1 `  ��              ��              �H     +   �� �  W   ,� `  X   �� �  Y   L   [   \   \   t <  ]   �    ^   � 0  `   ?   �$  iSO8859-1                                                                           \_     �                                      �                   |�                �_  @    t   Ȼ   T�              ��  �   �_      �_                                                       PROGRESS                                    
    
                    �              �                                                                                                     
  T         �          �  |6  m   �6     �!  �`�X�7  ;                     `          !      �                INTEGRAL                         PROGRESS                         4                                      �`�X              i�                              �  �                        �  -�     CODMATDESMATCODMARUNDSTKUNDCMPFACEQUCODCTACODNEWMONVTAPREVTAPREBASAFTIGVVINMN1CODCIAVINMN2CODFAMVCTMN1FCHACTCODPR1CODPR2VCTMN2ARTPROFCHUSALFCHUCMPPMAXMN1PMAXMN2PULTMN1PULTMN2USUARIOFCHINGFCHCESFCHALZCLFMATUNDBASSUBFAMCODBRRCODANTTIPARTFCHPRMDFCHPRMHFCHREAPESMATDETALLECANEMPALMACENESDESMARAFTISCPORISCPORVTATPOMRGCTOLISCTOPRMMRGUTIPORMAXFCHMPREUNDANTPREANTPREACTDSCTOSTPOPROPORIGVCTOTOTTPOSUMCTOUNDORDENORDLISORDTMPTPOARTTPOCMBPPCHR__01CHR__02CHR__03DEC__01DEC__02DEC__03DATE__01DATE__02DATE__03MRGUTI-AMRGUTI-BMRGUTI-CPREOFIUNDAUNDBUNDCFLGINTFLGPRECLASEFCHPROMCATCONTATIPROTDSCTOPROMINFORFLGINFORPROMDIVIPROMFCHDPROMFCHHPROMDTODTOVOLRDTOVOLDUNDALTDSCALTMRGALTPREALTLICENCIAPROMMINDIVIPROMMINFCHDPROMMINFCHHPROMMINDTOCODDIGESAVTODIGESALIBRE_C01LIBRE_C02LIBRE_C03LIBRE_C04LIBRE_C05LIBRE_D01LIBRE_D02LIBRE_F01LIBRE_F02STKMINSTKMAXSTKREPDESCRIPCION-LARGADESCRIPCION-TECNICASW-WEBWEB-SUBCATEGORIALIBRE_D03LIBRE_D04LIBRE_D05PESOBRUTOPAQUETELARGOALTOANCHOCTOLISMARCOCTOTOTMARCOCODSSFAMCLFESPECIALFLGCOMERCIALLIBRE_C06LIBRE_C07LIBRE_C08LIBRE_C09LIBRE_C10CODIGOPADREFACTORPADRE                                                                         	          
                                                                                                                                                                                                                                      !          "          #          $          %          &          '          (          )          *          +          ,          -          .          /          0          1          2         3          4          5         6          7          8         9          :          ;          <         =          >          ?          @          A          B          C          D          E          F          G          H          I          J          K          L          M          N          O          P          Q          R          S          T          U          V          W          X          Y          Z          [         \         ]         ^         _         `         a 
        b 
        c 
        d 
        e 
        f 
        g         h         i         j         k         l 
        m 
        n 
        o 
        p          q          r          s          t          u          v          w          x          y          z          {          |          }          ~                    �          �          �          �          �          �          �          �          �          �          �          �          �          �          �          �          �          �          �          �          �          �          �        ,  
    
                    �             �                                                                                                    
  \  )      �  
    
                  �  �             H                                                                                          )          
    ;      �  
    
                  p  8             �                                                                                          ;          
  �  H      0  
    
                    �             �                                                                                          H          
  `  [      �  
    
                  �  �  	           L                                                                                          [          
    m      �  
    
                  t  <  
           �                                                                                          m          
  �  �      4  
    
                     �             �                                                                                          �          
  d  �      �  
    
                  �  �             P                                                                                          �          
    �      �                         x  @             �                                                                                          �            �  �      8                        $  �             �                                                                                          �            h  �      �  
    
                  �  �             T                                                                                          �          
    �      �  
    
                  |  D                                                                                                        �          
  �  �      <  
    
                  (  �             �                                                                                          �          
  l  �      �                        �  �             X                                                                                          �              �      �                        �  H                                                                                                       �            �        @                        ,  �             �                                                                                                                �                        �  p             \                                                                                                      �         �       �  X  dJ  [   �J  �  5�      K  (       �             �8          �;      �              �       �  X   ^  \   \^  �  *!      �^  )       �         �    �K          O      �                 ��                                               ��          �    L l�                �         
             
             
                                         
                                                                                                               
             
                                          L   \   l   |   �   �   �   �   �   �   �   �       ,  <  L  \      L   \   l   |   �   �   �   �   �   �   �   �      ,  <  L  \                                                                                                                                                                         	                  
                                                                                                                                                                                                                                                                                                                                                                                         !                  "                  ,                  -                  .                  /                  0                  7                  #                  $                  %                  &                  '                  (                  )                  1                  2                  3                  *                  +                  5                  6                  4                  8                  9                  :                  ;                  <                                 `+  h+  p+  �+  x+                         �+  �+  �+  �+  �+          �+             �+  �+  �+  �+  �+          �+             �+  �+  �+  �+  �+           ,              ,   ,  (,  @,  4,                          D,  L,  T,  t,  d,                         x,  �,  �,  �,  �,                          �,  �,  �,  �,  �,                         �,  �,  �,  -   -          -             $-  ,-  <-  \-  L-                         `-  h-  p-  �-  |-                         �-  �-  �-  �-  �-                         �-  �-  �-  �-  �-                         �-  .  .  $.  .                         (.  0.  4.  D.  <.                         H.  P.  X.  h.  `.                          l.  t.  |.  �.  �.                          �.  �.  �.  �.  �.                         �.  �.  �.  �.  �.                          /  /  /  0/  $/                         4/  </  L/  d/  X/                         h/  p/  �/  �/  �/          �/             �/  �/  �/  �/  �/                          �/  �/  �/   0                             0  0  0  ,0   0                          40  <0  H0  p0  \0                         t0  |0  �0  �0  �0                         �0  �0  �0  �0  �0          �0             �0  �0  �0  1  �0          1             (1  41  <1  L1  D1                         P1  \1  d1  p1                              t1  |1  �1  �1  �1          �1              �1  �1  �1  2  �1          2             2  $2  42  \2  H2          `2             t2  |2  �2  �2  �2          �2             �2  �2  �2  �2                             �2  �2   3  3                              3  3  (3  03                             43  <3  P3  X3                             \3  h3  p3  |3                              �3  �3  �3  �3                              �3  �3  �3  �3                              �3  �3  �3  �3                              �3  �3   4  4                              4  4  04  <4                             @4  L4  `4  l4                             p4  |4  �4  �4                             �4  �4  �4  �4                             �4  �4  �4  �4                             �4  �4   5  5                              5  5  (5  45                              85  D5  P5  \5                              `5  l5  x5  �5                              �5  �5  �5  �5                              �5  �5  �5  �5                              �5  �5  �5  �5                              �5  6  6   6                             $6  06  @6  L6                             P6  \6  l6  x6                                                                         CodDoc  x(3)    Codigo  Codigo      CodCia  999 Cia Cia 0   C�digo de compa�ia  Factor  >>,>>9.9999 Factor  Factor  0   Factor  UndVta  x(8)    Unidad  Und     Unidad de movimiento    TipVta  X(1)    Tipo Venta  Tipo venta      codmat  X(6)    Codigo Articulo Codigo Articulo     CodAux  X(12)   Codigo Cliente  Codigo!Cliente      NroPed  X(12)   No. Pedido  Numero!Pedido       CanPed  >,>>>,>>9.9999  Cantidad    Cantidad    0   Cantidad despachada PreUni  >,>>>,>>9.99999 Precio Unitario Precio Unitario 0   PorDto  >>9.99  % Dscto.    % Dscto.    0   PorDto2 >>9.99  % Dscto.    % Dscto.    0   ImpDto  >,>>>,>>9.9999  Importe Descuento   Importe!Descuento   0   ImpLin  ->>,>>>,>>9.99  Importe Importe 0   NroItm  >>9 No.Item No.Item 0   AftIgv  Si/No   I.G.V.  I.G.V.  Si  AftIsc  Si/No   I.S.C.  I.S.C.  No  PreBas  >,>>>,>>9.9999  Precio Base Precio Base 0   PreVta  >>,>>>,>>9.99   Precio Venta    Precio Venta    0   ImpIgv  >,>>>,>>9.9999  Importe Igv Importe Igv 0   ImpIsc  >,>>>,>>9.9999  Importe Isc Importe Isc 0   canate  >,>>>,>>9.9999  Cantidad    Cantidad    0   Cantidad atendida   Hora    X(5)    Hora    Hora    string(TIME,"HH:MM")    FlgEst  X(1)    FlgEst  P   FchPed  99/99/9999  Fecha   Fch.Pedido  today   MrgUti  ->>,>>9.99  Margen de Utilidad  Margen de! Utilidad 0   Pesmat  ->>,>>9.9999    Peso    Peso    0   CodCli  x(8)    Codigo  Codigo      C�digo del Cliente  AlmDes  x(3)    Almac�n Despacho    Almac�n!Despacho        Almac�n despacho    Por_Dsctos  ->>9.99 % Dscto % Dscto 0   Flg_Factor  X(1)    Flg_Factor      CodDiv  x(5)    Division    Division    00000   Ingrese el Codigo de Division   CanPick >,>>>,>>9.9999  Cantidad Pickeada   Cantidad!Pickeada   0   Cantidad pickeada   CanSol  >,>>>,>>9.9999  Cantidad Solicitada Cantidad!Solicitada 0   Cantidad solicitada CanApr  >,>>>,>>9.9999  Cantidad Aprobada   Cantidad!Aprobada   0   Cantidad aprobada   ImpDto2 ->>>,>>>,>>9.99 ImpDto2 0   AlmTrf  x(8)    AlmTrf      CanTrf  ->>>,>>>,>>9.9999   CanTrf  0   SdoCot  ->>>,>>>,>>9.9999   SdoCot  0   Libre_c01   x(60)   Libre_c01       Libre_c02   x(60)   Libre_c02       Libre_c03   x(60)   Libre_c03       Libre_c04   x(60)   Libre_c04       Libre_c05   x(60)   Libre_c05       Libre_d01   ->>>,>>>,>>9.99<<<  Libre_d01   0   Libre_d02   ->>>,>>>,>>9.99<<<  Libre_d02   0   Libre_d03   ->>,>>9.99  Libre_d03   0   Libre_d04   ->>,>>9.99  Libre_d04   0   Libre_d05   ->>,>>9.99  Libre_d05   0   Libre_f01   99/99/99    Libre_f01   ?   Libre_f02   99/99/99    Libre_f02   ?   Libre_f03   99/99/9999  Libre_f03   ?   Libre_f04   99/99/9999  Libre_f04   ?   Libre_f05   99/99/9999  Libre_f05   ?   CodMatWeb   x(15)   CodMatWeb       DesMatWeb   x(60)   DesMatWeb       CanPedWeb   >,>>>,>>9.9999  CanPedWeb   0   PreUniWeb   >,>>>,>>9.99999 PreUniWeb   0   ImpLinWeb   ->>,>>>,>>9.99  ImpLinWeb   0   �    B W�  ���<������              �    �   strinP�       �    � 00000        ��        ���             �$        �$        �$        �$                �     i  i  i  i      i  i  i  i      i  i  i      i  i  i  i  i     	 	 	 	 	 	 	 	    ,   3   :   A   O   ]   d   k   r   �   �   �   �   �   �   �   �   �   �   �   �   �   V   �   y   H   �   �   �   �         F  P  Z  d  n  x  �  �  �    "  )  1  8  �  �  �  �  �  �  ?  �  �  �  �                                                                                                                                       	                  
                                                                                                                                                                                                                                                                                                                                                                                                                               !                  "                  #                  $                  %                  &                  '                  (                  )                                 �B  �B  �B  �B  �B                         �B  �B   C  C  C                         C  C  ,C  DC  8C          HC             \C  dC  hC  pC  lC          tC             �C  �C  �C  �C  �C          �C             �C  �C  �C  �C  �C          �C             D  D   D  0D  (D                         4D  <D  DD  dD  TD                         hD  pD  |D  �D  �D          �D             �D  �D  �D  �D  �D                         �D  �D  �D  �D                             �D  �D  �D  E  �D                         E  $E  4E  \E  HE                         `E  hE  xE  �E                             �E  �E  �E  �E  �E                         �E  �E  �E  �E  �E                         �E  �E  F  F  F                         F  $F  ,F  8F                             <F  DF  PF  xF  dF                         |F  �F  �F  �F  �F                         �F  �F  �F  �F  �F                         �F  �F  �F  �F  �F                         �F   G  G   G  G                         $G  0G  8G  HG  @G                         LG  XG  `G  pG  hG                         tG  �G  �G  �G  �G                         �G  �G  �G  �G  �G                         �G  �G  �G  H  �G                         H  H  $H  DH  4H                         HH  PH  `H  �H  pH                         �H  �H  �H  �H  �H                         �H  �H  �H  �H  �H          �H             �H   I  I  8I  $I          <I             PI  XI  `I  xI  lI          |I             �I  �I  �I  �I  �I                         �I  �I  �I  �I                             �I  �I  �I  �I                               J  J  J  J                             J  (J  0J  <J                             @J  LJ  TJ  `J                                                                          AftIgv  Si/No   I.G.V.  I.G.V.  Si  AftIsc  Si/No   I.S.C.  I.S.C.  No  CanPed  >,>>>,>>9.9999  Cantidad    Cantidad    0   Cantidad despachada CodCia  999 Cia Cia 0   C�digo de compa�ia  CodCli  x(8)    Codigo  Codigo      C�digo del Cliente  CodDiv  x(5)    Division    Division    00000   Ingrese el Codigo de Division   CodDoc  x(3)    Codigo  Codigo      codmat  X(14)   Codigo Articulo Codigo Articulo     Factor  >>,>>9.9999 Factor  Factor  0   Factor  FchPed  99/99/9999  Fecha   Fch.Pedido  today   FlgEst  X(1)    FlgEst  P   Hora    X(5)    Hora    Hora    string(TIME,"HH:MM")    ImpDto  >,>>>,>>9.9999  Importe Descuento   Importe!Descuento   0   ImpDto2 ->>>,>>>,>>9.99 ImpDto2 0   ImpIgv  >,>>>,>>9.9999  Importe Igv Importe Igv 0   ImpIsc  >,>>>,>>9.9999  Importe Isc Importe Isc 0   ImpLin  ->>,>>>,>>9.99  Importe Importe 0   Libre_c05   x(60)   Libre_c05       MrgUti  ->>,>>9.99  Margen de Utilidad  Margen de! Utilidad 0   NroItm  >>9 No.Item No.Item 0   NroPed  X(12)   No. Pedido  Numero!Pedido       PorDto  >>9.99  % Dscto.    % Dscto.    0   PorDto2 >>9.99  % Dscto.    % Dscto.    0   Por_Dsctos1 ->>9.99 % Dscto % Dscto 0   Por_Dsctos2 ->>9.99 % Dscto % Dscto 0   Por_Dsctos3 ->>9.99 % Dscto % Dscto 0   PreBas  >,>>>,>>9.9999  Precio Base Precio Base 0   PreUni  >,>>>,>>9.99999 Precio Unitario Precio Unitario 0   PreVta1 >>,>>>,>>9.99   Precio Venta    Precio Venta    0   PreVta2 >>,>>>,>>9.99   Precio Venta    Precio Venta    0   PreVta3 >>,>>>,>>9.99   Precio Venta    Precio Venta    0   UndVta  x(8)    Unidad  Und     Unidad de movimiento    CanApr  >,>>>,>>9.9999  Cantidad Aprobada   Cantidad!Aprobada   0   Cantidad aprobada   DesMat  X(60)   Descripci�n Descripci�n     Descripci�n del material    DesMar  X(20)   Marca   Marca       RowNum  ->,>>>,>>9  RowNum  0   RowIdent    x(8)    RowIdent        RowMod  x(8)    RowMod      RowIdentIdx x(8)    RowIdentIdx     RowUserProp x(8)    RowUserProp     �  : J�  ���)������    00000   �Pstring(TIME,"HH:MM")                                 #        *#        1#                �     i     i     i    % 	' 	( 	    �   �   d   3   �     ,   O   :   �   �   �   �   )  �   �   �   n  �   �   ]   r   y   �"  �"  �"  �   k   �"  �"  #  A   "  #  #  #  !#  *#  1#  =#                                                                                                                                     	                  
                                                                                                                                                                                                                                                                                                                                                                                                                               !                  "                  #                  $                  %                  &                  '                  (                  )                  *                                 <V  DV  LV  \V  TV                         `V  hV  pV  �V  xV                         �V  �V  �V  �V  �V          �V             �V  �V  �V  �V  �V          �V             �V   W  W  W  W          W             0W  8W  @W  XW  LW          `W             �W  �W  �W  �W  �W                         �W  �W  �W  �W  �W                         �W  �W  �W  �W  �W           X             X  X  X  0X  $X                         8X  @X  HX  PX                             TX  \X  dX  tX  lX                         �X  �X  �X  �X  �X                         �X  �X  �X  �X                             �X  �X  Y  $Y  Y                         (Y  0Y  @Y  XY  LY                         \Y  dY  tY  �Y  |Y                         �Y  �Y  �Y  �Y                             �Y  �Y  �Y  �Y  �Y                         �Y  �Y  �Y  Z   Z                         Z  Z  Z  8Z  (Z                         <Z  DZ  LZ  dZ  XZ                         hZ  pZ  xZ  �Z  �Z                         �Z  �Z  �Z  �Z  �Z                         �Z  �Z  �Z  �Z  �Z                         �Z  �Z  �Z  [   [                         [  [  $[  <[  0[                         @[  H[  X[  x[  h[                         |[  �[  �[  �[  �[                         �[  �[  �[  �[  �[                         �[  �[  \  ,\  \                         0\  8\  @\  L\  H\          P\             h\  p\  �\  �\  �\          �\             �\  �\  �\  �\  �\          �\             ]  ]  ]  (]   ]                         ,]  4]  @]  H]                             L]  X]  `]  l]                              p]  x]  �]  �]                             �]  �]  �]  �]                             �]  �]  �]  �]                              �]  �]  �]  �]                                                                          AftIgv  Si/No   I.G.V.  I.G.V.  Si  AftIsc  Si/No   I.S.C.  I.S.C.  No  CanPed  >,>>>,>>9.9999  Cantidad    Cantidad    0   Cantidad despachada CodCia  999 Cia Cia 0   C�digo de compa�ia  CodCli  x(8)    Codigo  Codigo      C�digo del Cliente  CodDiv  x(5)    Division    Division    00000   Ingrese el Codigo de Division   CodDoc  x(3)    Codigo  Codigo      codmat  X(14)   Codigo Articulo Codigo Articulo     Factor  >>,>>9.9999 Factor  Factor  0   Factor  FchPed  99/99/9999  Fecha   Fch.Pedido  today   FlgEst  X(1)    FlgEst  P   Hora    X(5)    Hora    Hora    string(TIME,"HH:MM")    ImpDto  >,>>>,>>9.9999  Importe Descuento   Importe!Descuento   0   ImpDto2 ->>>,>>>,>>9.99 ImpDto2 0   ImpIgv  >,>>>,>>9.9999  Importe Igv Importe Igv 0   ImpIsc  >,>>>,>>9.9999  Importe Isc Importe Isc 0   ImpLin  ->>,>>>,>>9.99  Importe Importe 0   Libre_c05   x(60)   Libre_c05       MrgUti  ->>,>>9.99  Margen de Utilidad  Margen de! Utilidad 0   NroItm  >>9 No.Item No.Item 0   NroPed  X(12)   No. Pedido  Numero!Pedido       PorDto  >>9.99  % Dscto.    % Dscto.    0   PorDto2 >>9.99  % Dscto.    % Dscto.    0   Por_Dsctos1 ->>9.99 % Dscto % Dscto 0   Por_Dsctos2 ->>9.99 % Dscto % Dscto 0   Por_Dsctos3 ->>9.99 % Dscto % Dscto 0   PreBas  >,>>>,>>9.9999  Precio Base Precio Base 0   PreUni  >,>>>,>>9.99999 Precio Unitario Precio Unitario 0   PreVta1 >>,>>>,>>9.99   Precio Venta    Precio Venta    0   PreVta2 >>,>>>,>>9.99   Precio Venta    Precio Venta    0   PreVta3 >>,>>>,>>9.99   Precio Venta    Precio Venta    0   UndVta  x(8)    Unidad  Und     Unidad de movimiento    CanApr  >,>>>,>>9.9999  Cantidad Aprobada   Cantidad!Aprobada   0   Cantidad aprobada   DesMat  X(60)   Descripci�n Descripci�n     Descripci�n del material    DesMar  X(20)   Marca   Marca       RowNum  ->,>>>,>>9  RowNum  0   RowIdent    x(8)    RowIdent        RowMod  x(8)    RowMod      RowIdentIdx x(8)    RowIdentIdx     RowUserProp x(8)    RowUserProp     ChangedFields   x(8)    ChangedFields       �  : J�  ���*������    00000   �Pstring(TIME,"HH:MM")                                 #        *#        1#                �     i     i     i    % 	' 	( 	    �   �   d   3   �     ,   O   :   �   �   �   �   )  �   �   �   n  �   �   ]   r   y   �"  �"  �"  �   k   �"  �"  #  A   "  #  #  #  !#  *#  1#  =#  I#    ��                            ����                            V"    ��                    GI    �$   ��                    ��    undefined                                                               �        �  �   l   �  ��                    �����               ,�d                    O   ����    e�          O   ����    R�          O   ����    ��      t       �   �              4   ����     /                                    3   ����       $     H  ���                       8      
                       � ߱        �  �      D       �     @          ��    �   �  4      d       4   ����d                 D                      ��                  �   �                   �ʫ                       �   �  �  	  �   x                                        3   ����|       O   �   ��  ��  �   batchServices                               4        ��                  m  p  L              �1�                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �             d               ��                  �           ��                            ����                            clientSendRows                              �  p      ��                  r  x  �              x��                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �             �               ��                �               ��   <                            ��   d             0               ��                  X           ��                            ����                            commitTransaction                               X  @      ��                  z  {  p              p�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            copyColumns                             X  @      ��                  }  �  p              �                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �             �               �� 
                 �  
         ��                            ����                            dataAvailable                               �  �      ��                  �  �  �              \r                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �           ��                            ����                            describeSchema                              �	  �	      ��                  �  �  �	              ��                    O   ����    e�          O   ����    R�          O   ����    ��            ��   <
             
               �� 
          �       0
  
         ��                            ����                            destroyServerObject                             0        ��                  �  �  H              ^                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            endClientDataRequest                                <  $      ��                  �  �  T              �^                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            fetchBatch                              <  $      ��                  �  �  T              l_                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  l           ��                            ����                            fetchFirst                              d  L      ��                  �  �  |              �q                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            fetchLast                               d  L      ��                  �  �  |              �AI                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            fetchNext                               d  L      ��                  �  �  |              ,BI                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            fetchPrev                               d  L      ��                  �  �  |              �p                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            genContextList                              h  P      ��                  �  �  �              p                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �           ��                            ����                            home                                �  t      ��                  �  �  �              �p                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeObject                                �  |      ��                  �  �  �              x��                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeServerObject                              �  �      ��                  �  �  �              ܷ�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            isUpdatePending                             �  �      ��                  �  �  �              ���                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �           ��                            ����                            printToCrystal                              �  �      ��                  �  �  �              (�.                    O   ����    e�          O   ����    R�          O   ����    ��            ��   4                             ��   \             (               ��                  P           ��                            ����                            refreshRow                              H  0      ��                  �  �  `              ԋe                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            remoteSendRows                              L  4      ��                  �  �  d              �^�                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �             |               ��   �             �               ��                 �               ��   (             �               ��   P                            ��   x             D               �� 
  �      �       l  
             ��                  �           ��                            ����                            restartServerObject                             �  |      ��                  �  �  �              P�L                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            retrieveFilter                              �  �      ��                  �  �  �              P��                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            rowObjectState                              �  �      ��                  �  �  �              0�L                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �           ��                            ����                            saveContextAndDestroy                               �  �      ��                  �  �  �              �L                    O   ����    e�          O   ����    R�          O   ����    ��            ��                               ��                            ����                            serverSendRows                              �   �       ��                  �  �  !              ��L                    O   ����    e�          O   ����    R�          O   ����    ��            ��   `!             ,!               ��   �!             T!               ��   �!             |!               ��   �!             �!               ��    "             �!               �� 
          �       �!  
         ��                            ����                            serverFetchRowObjUpdTable                               �"  �"      ��                  �  �  #              Ǭ                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
          �       ,#  
         ��                            ����                            setPropertyList                             ($  $      ��                  �  �  @$              hˬ                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  X$           ��                            ����                            serverSendRows                              T%  <%      ��                  �  �  l%              ��$                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �%             �%               ��   �%             �%               ��   &             �%               ��   0&             �%               ��   X&             $&               �� 
          �       L&  
         ��                            ����                            startServerObject                               L'  4'      ��                  �     d'              ��O                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            submitCommit                                P(  8(      ��                      h(              0�O                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �(             �(               ��                  �(           ��                            ����                            submitForeignKey                                �)  �)      ��                      �)              �h�                    O   ����    e�          O   ����    R�          O   ����    ��            ��   *             �)               ��   4*              *               ��                  (*           ��                            ����                            submitValidation                                (+  +      ��                      @+              �k                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �+             X+               ��                  �+           ��                            ����                            synchronizeProperties                               �,  l,      ��                      �,              dk                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �,             �,               ��                  �,           ��                            ����                            transferToExcel                             �-  �-      ��                    $  �-              �fU                    O   ����    e�          O   ����    R�          O   ����    ��            ��   <.             .               ��   d.             0.               ��   �.             X.               ��                  �.           ��                            ����                            undoTransaction                             |/  d/      ��                  &  '  �/              ��~                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            updateAddQueryWhere                             �0  l0      ��                  )  ,  �0              �~                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �0             �0               ��                  �0           ��                            ����                            updateQueryPosition                             �1  �1      ��                  .  /  �1              <�                     O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            updateState                             �2  �2      ��                  1  3  �2              4�                     O   ����    e�          O   ����    R�          O   ����    ��            ��                  3           ��                            ����                            addRow          t3      �3     �      CHARACTER,INPUT pcViewColList CHARACTER cancelRow   |3      �3      �3   	 �      CHARACTER,  canNavigate �3      �3      (4    �      LOGICAL,    closeQuery  4      44      `4   
 �      LOGICAL,    columnProps @4      l4      �4    �      CHARACTER,INPUT pcColList CHARACTER,INPUT pcPropList CHARACTER  colValues   x4      �4      5   	 �      CHARACTER,INPUT pcViewColList CHARACTER copyRow �4      ,5      T5    �      CHARACTER,INPUT pcViewColList CHARACTER createRow   45      |5      �5   	 �      LOGICAL,INPUT pcValueList CHARACTER deleteRow   �5      �5      �5   	 �      LOGICAL,INPUT pcRowIdent CHARACTER  fetchRow    �5      6      H6  	  �      CHARACTER,INPUT piRow INTEGER,INPUT pcViewColList CHARACTER fetchRowIdent   (6      �6      �6  
  �      CHARACTER,INPUT pcRowIdent CHARACTER,INPUT pcViewColList CHARACTER  findRow �6      �6       7    �      LOGICAL,INPUT pcKeyValues CHARACTER findRowWhere     7      D7      t7    �      LOGICAL,INPUT pcColumns CHARACTER,INPUT pcValues CHARACTER,INPUT pcOperators CHARACTER  firstRowIds T7      �7      �7          CHARACTER,INPUT pcQueryString CHARACTER getLastCommitErrorType  �7       8      X8          CHARACTER,  hasForeignKeyChanged    88      d8      �8    )      LOGICAL,    openDataQuery   |8      �8      �8    >      LOGICAL,INPUT pcPosition CHARACTER  openQuery   �8      �8      (9   	 L      LOGICAL,    prepareQuery    9      49      d9    V      LOGICAL,INPUT pcQuery CHARACTER rowAvailable    D9      �9      �9    c      LOGICAL,INPUT pcDirection CHARACTER rowValues   �9      �9      :   	 p      CHARACTER,INPUT pcColumns CHARACTER,INPUT pcFormat CHARACTER,INPUT pcDelimiter CHARACTER    submitRow   �9      `:      �:   	 z      LOGICAL,INPUT pcRowIdent CHARACTER,INPUT pcValueList CHARACTER  updateRow   l:      �:      �:   	 �      LOGICAL,INPUT pcKeyValues CHARACTER,INPUT pcValueList CHARACTER getObjectType   �:      8;      h;    �      CHARACTER,  assignDBRow                             <  �;      ��                      <              �n�                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
                 4<  
         ��                            ����                            bufferCopyDBToRO                                4=  =      ��                    "  L=              To�                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  �=             d=  
             �� 
  �=             �=  
             ��   �=             �=               ��                  �=           ��                            ����                            compareDBRow                                �>  �>      ��                  $  %  �>              pY                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            confirmContinue                             �?  �?      ��                  '  )  �?              �!Y                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  @           ��                            ����                            dataAvailable                               A  �@      ��                  +  -   A              P��                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  8A           ��                            ����                            fetchDBRowForUpdate                             8B   B      ��                  /  0  PB              �cH                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            fetchFirst                              8C   C      ��                  2  3  PC              TfH                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            fetchLast                               8D   D      ��                  5  6  PD              �iH                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            fetchNext                               8E   E      ��                  8  9  PE              �jH                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            fetchPrev                               8F   F      ��                  ;  <  PF              �                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            filterContainerHandler                              DG  ,G      ��                  >  @  \G              (                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
                 tG  
         ��                            ����                            initializeObject                                tH  \H      ��                  B  C  �H              ȶ|                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            refetchDBRow                                xI  `I      ��                  E  G  �I              �|                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
                 �I  
         ��                            ����                            releaseDBRow                                �J  �J      ��                  I  J  �J              0�;                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            startFilter                             �K  �K      ��                  L  M  �K              ��;                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            transferDBRow                               �L  �L      ��                  O  R  �L              ��;                    O   ����    e�          O   ����    R�          O   ����    ��            ��   M             �L               ��                   M           ��                            ����                            addQueryWhere   H;      hM      �M    �      LOGICAL,INPUT pcWhere CHARACTER,INPUT pcBuffer CHARACTER,INPUT pcAndOr CHARACTER    assignQuerySelection    xM      �M      $N    �      LOGICAL,INPUT pcColumns CHARACTER,INPUT pcValues CHARACTER,INPUT pcOperators CHARACTER  bufferCompareDBToRO N      |N      �N    �      LOGICAL,INPUT phRowObjUpd HANDLE,INPUT phBuffer HANDLE,INPUT pcExcludes CHARACTER,INPUT pcAssigns CHARACTER bufferWhereClause   �N      O      PO    �      CHARACTER,INPUT pcBuffer CHARACTER,INPUT pcWhere CHARACTER  columnDataType  0O      �O      �O    �      CHARACTER,INPUT pcColumn CHARACTER  columnDbColumn  �O      �O      P          CHARACTER,INPUT pcColumn CHARACTER  columnQuerySelection    �O      4P      lP          CHARACTER,INPUT pcColumn CHARACTER  columnTable LP      �P      �P    +      CHARACTER,INPUT pcColumn CHARACTER  columnValMsg    �P      �P      Q     7      CHARACTER,INPUT pcColumn CHARACTER  dbColumnDataName    �P      4Q      hQ  !  D      CHARACTER,INPUT pcDbColumn CHARACTER    dbColumnHandle  HQ      �Q      �Q  "  U      HANDLE,INPUT pcColumn CHARACTER excludeColumns  �Q      �Q      R  #  d      CHARACTER,INPUT iTable INTEGER  getDataColumns  �Q      0R      `R  $  s      CHARACTER,  getForeignValues    @R      lR      �R  %  �      CHARACTER,  getQueryPosition    �R      �R      �R  &  �      CHARACTER,  getQuerySort    �R      �R      S  '  �      CHARACTER,  getQueryString  �R      (S      XS  (  �      CHARACTER,  getQueryWhere   8S      dS      �S  )  �      CHARACTER,  getTargetProcedure  tS      �S      �S  *  �      HANDLE, indexInformation    �S      �S      T  +  �      CHARACTER,INPUT pcQuery CHARACTER,INPUT plUseTableSep LOGICAL,INPUT pcIndexInfo CHARACTER   insertExpression    �S      lT      �T  ,  �      CHARACTER,INPUT pcWhere CHARACTER,INPUT pcExpression CHARACTER,INPUT pcAndOr CHARACTER  newQueryString  �T      �T      (U  -        CHARACTER,INPUT pcColumns CHARACTER,INPUT pcValues CHARACTER,INPUT pcOperators CHARACTER,INPUT pcQueryString CHARACTER,INPUT pcAndOr CHARACTER  newQueryValidate    U      �U      �U  .        CHARACTER,INPUT pcQueryString CHARACTER,INPUT pcExpression CHARACTER,INPUT pcBuffer CHARACTER,INPUT pcAndOr CHARACTER   newQueryWhere   �U      dV      �V  /  #      CHARACTER,INPUT pcWhere CHARACTER   newWhereClause  tV      �V      �V  0  1      CHARACTER,INPUT pcBuffer CHARACTER,INPUT pcExpression CHARACTER,INPUT pcWhere CHARACTER,INPUT pcAndOr CHARACTER refreshRowident �V      XW      �W  1  @      CHARACTER,INPUT pcRowident CHARACTER    removeForeignKey    hW      �W      �W  2  P      LOGICAL,    removeQuerySelection    �W      �W      (X  3  a      LOGICAL,INPUT pcColumns CHARACTER,INPUT pcOperators CHARACTER   resolveBuffer   X      hX      �X  4  v      CHARACTER,INPUT pcBuffer CHARACTER  rowidWhere  xX      �X      �X  5 
 �      CHARACTER,INPUT pcWhere CHARACTER   rowidWhereCols  �X      Y      <Y  6  �      CHARACTER,INPUT pcColumns CHARACTER,INPUT pcValues CHARACTER,INPUT pcOperators CHARACTER    setQueryPosition    Y      �Y      �Y  7  �      LOGICAL,INPUT pcPosition CHARACTER  setQuerySort    �Y      �Y       Z  8  �      LOGICAL,INPUT pcSort CHARACTER  setQueryString   Z      @Z      pZ  9  �      LOGICAL,INPUT pcQueryString CHARACTER   setQueryWhere   PZ      �Z      �Z  :  �      LOGICAL,INPUT pcWhere CHARACTER whereClauseBuffer   �Z      �Z      [  ;  �      CHARACTER,INPUT pcWhere CHARACTER   bindServer                              �[  �[      ��                  �  �  �[              ��H                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            destroyObject                               �\  �\      ��                  �  �  �\              p�H                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            destroyServerObject                             �]  �]      ��                  �  �  �]              �H                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            disconnectObject                                �^  �^      ��                  �  �  �^              l�H                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeServerObject                              �_  �_      ��                  �  �  `              �H                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            restartServerObject                             �`  �`      ��                  �  �  a              �                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            runServerObject                             �a  �a      ��                  �     b              h�                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
                 ,b  
         ��                            ����                            startServerObject                               ,c  c      ��                      Dc              �
�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            unbindServer                                0d  d      ��                      Hd              ��P                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  `d           ��                            ����                            getAppService   �Z      �d      �d  <  �      CHARACTER,  getASBound  �d      e      0e  = 
 �      LOGICAL,    getAsDivision   e      <e      le  >        CHARACTER,  getASHandle Le      xe      �e  ?        HANDLE, getASHasStarted �e      �e      �e  @        LOGICAL,    getASInfo   �e      �e      f  A 	 .      CHARACTER,  getASInitializeOnRun    �e       f      Xf  B  8      LOGICAL,    getASUsePrompt  8f      df      �f  C  M      LOGICAL,    getServerFileName   tf      �f      �f  D  \      CHARACTER,  getServerOperatingMode  �f      �f      g  E  n      CHARACTER,  runServerProcedure  �f      $g      Xg  F  �      HANDLE,INPUT pcServerFileName CHARACTER,INPUT phAppService HANDLE   setAppService   8g      �g      �g  G  �      LOGICAL,INPUT pcAppService CHARACTER    setASDivision   �g      �g      $h  H  �      LOGICAL,INPUT pcDivision CHARACTER  setASHandle h      Hh      th  I  �      LOGICAL,INPUT phASHandle HANDLE setASInfo   Th      �h      �h  J 	 �      LOGICAL,INPUT pcInfo CHARACTER  setASInitializeOnRun    �h      �h      i  K  �      LOGICAL,INPUT plInitialize LOGICAL  setASUsePrompt  �h      <i      li  L  �      LOGICAL,INPUT plFlag LOGICAL    setServerFileName   Li      �i      �i  M  �      LOGICAL,INPUT pcFileName CHARACTER  setServerOperatingMode  �i      �i      j  N         LOGICAL,INPUT pcServerOperatingMode CHARACTER   addLink                             �j  �j      ��                  �  �  �j              ��l                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  <k             k  
             ��   dk             0k               �� 
                 Xk  
         ��                            ����                            addMessage                              Pl  8l      ��                  �  �  hl              ��l                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �l             �l               ��   �l             �l               ��                  �l           ��                            ����                            adjustTabOrder                              �m  �m      ��                  �  �  �m              �AN                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  0n             �m  
             �� 
  Xn             $n  
             ��                  Ln           ��                            ����                            applyEntry                              Do  ,o      ��                  �  �  \o              �IN                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  to           ��                            ����                            changeCursor                                pp  Xp      ��                  �  �  �p              �nP                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �p           ��                            ����                            createControls                              �q  �q      ��                  �  �  �q              \NN                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            destroyObject                               �r  �r      ��                  �  �  �r              ��R                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            displayLinks                                �s  �s      ��                  �  �  �s              `�R                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            editInstanceProperties                              �t  �t      ��                  �  �  �t              ��R                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            exitObject                              �u  �u      ��                  �  �  �u              ��R                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            hideObject                              �v  �v      ��                  �  �  �v              ��R                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeObject                                �w  �w      ��                  �  �  �w              ��R                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            modifyListProperty                              �x  �x      ��                  �  �  �x              ��R                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  $y             �x  
             ��   Ly             y               ��   ty             @y               ��                  hy           ��                            ����                            modifyUserLinks                             dz  Lz      ��                       |z              �>?                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �z             �z               ��   �z             �z               �� 
                 �z  
         ��                            ����                            removeAllLinks                              �{  �{      ��                      �{              �I?                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            removeLink                              �|  �|      ��                  	    �|              LJ?                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  D}             }  
             ��   l}             8}               �� 
                 `}  
         ��                            ����                            repositionObject                                `~  H~      ��                      x~              ܧC                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �~             �~               ��                  �~           ��                            ����                            returnFocus                             �  �      ��                      �              ЮC                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
                 �  
         ��                            ����                            showMessageProcedure                                �  ̀      ��                      ��              �{�                    O   ����    e�          O   ����    R�          O   ����    ��            ��   H�             �               ��                  <�           ��                            ����                            toggleData                              4�  �      ��                      L�              t|�                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  d�           ��                            ����                            viewObject                              \�  D�      ��                  !  "  t�              L�L                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            anyMessage  �i      ̃      ��  O 
 e      LOGICAL,    assignLinkProperty  ؃      �      8�  P  p      LOGICAL,INPUT pcLink CHARACTER,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER   fetchMessages   �      ��      ��  Q  �      CHARACTER,  getChildDataKey ��      ̄      ��  R  �      CHARACTER,  getContainerHandle  ܄      �      <�  S  �      HANDLE, getContainerHidden  �      D�      x�  T  �      LOGICAL,    getContainerSource  X�      ��      ��  U  �      HANDLE, getContainerSourceEvents    ��      ��      ��  V  �      CHARACTER,  getContainerType    ܅      �      <�  W  �      CHARACTER,  getDataLinksEnabled �      H�      |�  X  	      LOGICAL,    getDataSource   \�      ��      ��  Y  	      HANDLE, getDataSourceEvents ��      ��      �  Z  &	      CHARACTER,  getDataSourceNames  Ԇ       �      4�  [  :	      CHARACTER,  getDataTarget   �      @�      p�  \  M	      CHARACTER,  getDataTargetEvents P�      |�      ��  ]  [	      CHARACTER,  getDBAware  ��      ��      �  ^ 
 o	      LOGICAL,    getDesignDataObject ȇ      �      (�  _  z	      CHARACTER,  getDynamicObject    �      4�      h�  `  �	      LOGICAL,    getInstanceProperties   H�      t�      ��  a  �	      CHARACTER,  getLogicalObjectName    ��      ��      ��  b  �	      CHARACTER,  getLogicalVersion   Ј      ��      0�  c  �	      CHARACTER,  getObjectHidden �      <�      l�  d  �	      LOGICAL,    getObjectInitialized    L�      x�      ��  e  �	      LOGICAL,    getObjectName   ��      ��      �  f  
      CHARACTER,  getObjectPage   ̉      ��      (�  g  
      INTEGER,    getObjectParent �      4�      d�  h  
      HANDLE, getObjectVersion    D�      l�      ��  i  -
      CHARACTER,  getObjectVersionNumber  ��      ��      �  j  >
      CHARACTER,  getParentDataKey    Ċ      ��      $�  k  U
      CHARACTER,  getPassThroughLinks �      0�      d�  l  f
      CHARACTER,  getPhysicalObjectName   D�      p�      ��  m  z
      CHARACTER,  getPhysicalVersion  ��      ��      �  n  �
      CHARACTER,  getPropertyDialog   ȋ      �      (�  o  �
      CHARACTER,  getQueryObject  �      4�      d�  p  �
      LOGICAL,    getRunAttribute D�      p�      ��  q  �
      CHARACTER,  getSupportedLinks   ��      ��      ��  r  �
      CHARACTER,  getTranslatableProperties   ��      �      (�  s  �
      CHARACTER,  getUIBMode  �      4�      `�  t 
        CHARACTER,  getUserProperty @�      l�      ��  u        CHARACTER,INPUT pcPropName CHARACTER    instancePropertyList    |�      č      ��  v        CHARACTER,INPUT pcPropList CHARACTER    linkHandles ܍      $�      P�  w  0      CHARACTER,INPUT pcLink CHARACTER    linkProperty    0�      t�      ��  x  <      CHARACTER,INPUT pcLink CHARACTER,INPUT pcPropName CHARACTER mappedEntry ��      ��      �  y  I      CHARACTER,INPUT pcEntry CHARACTER,INPUT pcList CHARACTER,INPUT plFirst LOGICAL,INPUT pcDelimiter CHARACTER  messageNumber   �      x�      ��  z  U      CHARACTER,INPUT piMessage INTEGER   propertyType    ��      ̏      ��  {  c      CHARACTER,INPUT pcPropName CHARACTER    reviewMessages  ܏      $�      T�  |  p      CHARACTER,  setChildDataKey 4�      `�      ��  }        LOGICAL,INPUT cChildDataKey CHARACTER   setContainerHidden  p�      ��      �  ~  �      LOGICAL,INPUT plHidden LOGICAL  setContainerSource  ̐      �      @�    �      LOGICAL,INPUT phObject HANDLE   setContainerSourceEvents     �      `�      ��  �  �      LOGICAL,INPUT pcEvents CHARACTER    setDataLinksEnabled |�      ��      ��  �  �      LOGICAL,INPUT lDataLinksEnabled LOGICAL setDataSource   ԑ      �      L�  �  �      LOGICAL,INPUT phObject HANDLE   setDataSourceEvents ,�      l�      ��  �  �      LOGICAL,INPUT pcEventsList CHARACTER    setDataSourceNames  ��      Ȓ      ��  �        LOGICAL,INPUT pcSourceNames CHARACTER   setDataTarget   ܒ      $�      T�  �        LOGICAL,INPUT pcTarget CHARACTER    setDataTargetEvents 4�      x�      ��  �  %      LOGICAL,INPUT pcEvents CHARACTER    setDBAware  ��      Г      ��  � 
 9      LOGICAL,INPUT lAware LOGICAL    setDesignDataObject ܓ      �      P�  �  D      LOGICAL,INPUT pcDataObject CHARACTER    setDynamicObject    0�      x�      ��  �  X      LOGICAL,INPUT lTemp LOGICAL setInstanceProperties   ��      Ȕ       �  �  i      LOGICAL,INPUT pcPropList CHARACTER  setLogicalObjectName    ��      $�      \�  �        LOGICAL,INPUT c CHARACTER   setLogicalVersion   <�      x�      ��  �  �      LOGICAL,INPUT cVersion CHARACTER    setObjectName   ��      Е       �  �  �      LOGICAL,INPUT pcName CHARACTER  setObjectParent ��       �      P�  �  �      LOGICAL,INPUT phParent HANDLE   setObjectVersion    0�      p�      ��  �  �      LOGICAL,INPUT cObjectVersion CHARACTER  setParentDataKey    ��      ̖       �  �  �      LOGICAL,INPUT cParentDataKey CHARACTER  setPassThroughLinks ��      (�      \�  �  �      LOGICAL,INPUT pcLinks CHARACTER setPhysicalObjectName   <�      |�      ��  �  �      LOGICAL,INPUT cTemp CHARACTER   setPhysicalVersion  ��      ԗ      �  �        LOGICAL,INPUT cVersion CHARACTER    setRunAttribute �      ,�      \�  �  #      LOGICAL,INPUT cRunAttribute CHARACTER   setSupportedLinks   <�      ��      ��  �  3      LOGICAL,INPUT pcLinkList CHARACTER  setTranslatableProperties   ��      ܘ      �  �  E      LOGICAL,INPUT pcPropList CHARACTER  setUIBMode  ��      <�      h�  � 
 _      LOGICAL,INPUT pcMode CHARACTER  setUserProperty H�      ��      ��  �  j      LOGICAL,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER  showmessage ��      ��      $�  �  z      LOGICAL,INPUT pcMessage CHARACTER   Signature   �      H�      t�  � 	 �      CHARACTER,INPUT pcName CHARACTER    l�    8  ��  0�      �       4   �����                 @�                      ��                  9  f                  �[                       9  Ě        :  \�  ؛      �       4   �����                 �                      ��                  ;  e                  ��[                       ;  l�  �    R  �  ��      �       4   �����                 ��                      ��                  ^  `                  ��[                       ^  �         _                                  ,     
                    � ߱        �  $  b  ��  ���                           $  d  @�  ���                       x                         � ߱        x�    j  ��  �      �      4   �����                �                      ��                  k  /	                  ��[                       k  ��  H�  o   n      ,                                 ��  $   o  t�  ���                       �  @         �              � ߱        ��  �   p        Ȟ  �   q  �      ܞ  �   s        �  �   u  x      �  �   w  �      �  �   y  `      ,�  �   z  �      @�  �   {        T�  �   ~  �      h�  �   �         |�  �   �  |      ��  �   �  �      ��  �   �  t      ��  �   �  �      ̟  �   �  ,      ��  �   �  �      ��  �   �  �      �  �   �  P	      �  �   �  �	      0�  �   �   
      D�  �   �  t
      X�  �   �  �
      l�  �   �  l      ��  �   �  �      ��  �   �  \      ��  �   �  �      ��  �   �  D      Р  �   �  �      �  �   �  �      ��  �   �  0      �  �   �  �       �  �   �  �      4�  �   �        H�  �   �  X      \�  �   �  �      p�  �   �        ��  �   �  L      ��  �   �  �      ��  �   �  �      ��  �   �         ԡ  �   �  <      �  �   �  x      ��  �   �  �      �  �   �  �          �   �  ,                      <�          ��  ��      ��                  V	  �	  ��              ���                    O   ����    e�          O   ����    R�          O   ����    ��      �     
                                     (                         � ߱        h�  $ j	  آ  ���                           O   �	  ��  ��  h               ԣ          ģ  ̣    ��                                             ��                            ����                                8;      $�      ��     V     ܣ                       أ  �                     8�    �	  ��  �      t      4   ����t                 �                      ��                  �	  +
                  ���                       �	  ��  4�  �   �	  �      H�  �   �	  H      \�  �   �	  �      p�  �   �	  @      ��  �   �	  �      ��  �   �	  8      ��  �   �	  �      ��  �   �	  (      ԥ  �   �	  �      �  �   �	         ��  �   �	  �      �  �   �	        $�  �   �	  �          �   �	        ��    [
  T�  Ц      x      4   ����x                �                      ��                  \
  �
                  �{�                       \
  d�  ��  �   ^
  �      �  �   _
  T      �  �   `
  �      0�  �   a
  D      D�  �   b
  �      X�  �   c
  �      l�  �   e
  p      ��  �   f
  �      ��  �   g
  X      ��  �   h
  �      ��  �   i
  �      Ч  �   j
  D       �  �   k
  �       ��  �   l
  �       �  �   m
  x!       �  �   n
  �!      4�  �   o
  h"      H�  �   p
  �"      \�  �   q
  `#      p�  �   r
  �#      ��  �   s
  X$      ��  �   t
  �$      ��  �   u
  �$      ��  �   v
  L%      Ԩ  �   w
  �%      �  �   x
  <&      ��  �   y
  �&      �  �   z
  4'      $�  �   {
  �'      8�  �   |
  ,(      L�  �   }
  h(      `�  �   
  �(      t�  �   �
  X)      ��  �   �
  �)      ��  �   �
  *      ��  �   �
  �*      ĩ  �   �
  �*      ة  �   �
  l+      �  �   �
  �+       �  �   �
  \,      �  �   �
  �,      (�  �   �
  L-      <�  �   �
  �-      P�  �   �
  <.      d�  �   �
  �.      x�  �   �
  4/      ��  �   �
  �/          �   �
  $0      d�      ��  8�      T0      4   ����T0                H�                      ��                  	  �                  t�J                       	  ̪  \�  �     �0      p�  �     (1      ��  �     �1      ��  �     2      ��  �     �2      ��  �     3      ԫ  �     |3      �  �     �3      ��  �     t4      �  �     �4      $�  �     l5      8�  �     �5      L�  �     d6      `�  �     �6      t�  �     L7      ��  �     �7      ��  �     <8      ��  �     �8      Ĭ  �     ,9      ج  �      �9      �  �   !  :       �  �   "  X:      �  �   #  �:      (�  �   $  H;      <�  �   %  �;      P�  �   &  8<          �   '  �<      x�    �  ��  ��      =      4   ����=  	              �                      ��             	     �  G                  tw�                       �  ��   �  �   �  |=      4�  �   �  �=      H�  �   �  t>      \�  �   �  �>      p�  �   �  l?      ��  �   �  �?      ��  �   �  \@      ��  �   �  �@      ��  �   �  TA      Ԯ  �   �  �A      �  �   �  DB      ��  �   �  �B      �  �   �  <C      $�  �   �  �C      8�  �   �  ,D      L�  �   �  �D      `�  �   �  $E      t�  �   �  �E      ��  �   �  F      ��  �   �  �F      ��  �   �  G      į  �   �  �G      د  �   �  �G      �  �   �  8H       �  �   �  �H      �  �   �  0I      (�  �   �  �I      <�  �   �   J          �   �  �J      getRowObjUpdStatic  deleteRecordStatic  �    �  ��  ��      K      4   ����K      /   �  а     �                          3   ����K             �                      3   ����<K  Զ      ,�  ��  �  XK      4   ����XK  
              ��                      ��             
       i                  �O                         <�  ̱  �     �K      $�  $    ��  ���                       �K     
                    � ߱        8�  �     L      ��  $     d�  ���                       ,L  @         L              � ߱        L�  $    ��  ���                       �L       	       	           � ߱        �M     
                N                     \O  @        
 O              � ߱        ܳ  V     �  ���                        hO       	       	       �O       
       
       �O       	       	           � ߱        l�  $  8  x�  ���                       �P     
                Q                     dR  @        
 $R              � ߱            V   J  �  ���                                      ̵                      ��                  k                    h�O                       k  ��  pR     
                �R                     <T  @        
 �S          �T  @        
 dT          U  @        
 �T          dU  @        
 $U              � ߱            V   �  �  ���                        adm-clone-props �  ��              �     W     `                          \  �                     start-super-proc    �  d�  �           �     X                                  �                     l�       �   �      �X      4   �����X      /   !  ,�     <�                          3   ���� Y            \�                      3   ���� Y  ķ  $  ;  ��  ���                       @Y                         � ߱        ��    K  �  \�  ��  \Y      4   ����\Y                и                      ��                  L  P                  ��1                       L  �  pY                     �Y                     �Y                         � ߱            $  M  l�  ���                             Q  �  T�      �Y      4   �����Y  �Y                         � ߱            $  R  (�  ���                       |�    Y  ��  ��  �  �Y      4   �����Y      $  Z  ع  ���                       Z                         � ߱            �   w  Z      XZ     
                �Z                     $\  @        
 �[              � ߱        ��  V   �  �  ���                        ��  �   �  0\      ��    =  غ  �      p\      4   ����p\      /   >  �     $�                          3   �����\            D�                      3   �����\  �\     
                <]                     �^  @        
 L^              � ߱        H�  V   J  T�  ���                        �^     
                T_                     �`  @        
 d`              � ߱        t�  V   n  �  ���                        ��    �  ��  �      �`      4   �����`                �                      ��                  �  �                  �J                       �  ��  ��  /   �  H�     X�                          3   �����`            x�                      3   �����`      /   �  ��     Ľ                          3   ����a            �                      3   ����$a  p�  /  [   �         Xa                      3   ����@a  initProps   x�  0�              �     Y     \             �          X  a"  	                                   t�          �  �      ��                �  �  4�              ��I                    O   ����    e�          O   ����    R�          O   ����    ��      k"                      L�          H�  p   �  h~  ��      �  ��  �     t~                �                      ��                  �  �                  $                       �  ��  4�  :  �                 $  �  `�  ���                       �~                         � ߱        ��  �     �~                �                      ��                  �                    t%                       �  ��  0�  :  �                 $  �  \�  ���                       �~                         � ߱        �  �     �~                                        ��                                      �&                         ��  ��  ��     �~                                        ��                     <                  (                          $�  ,�  �     �~                                        ��                  =  Y                  �(                       =  ��  ��  ��                                             ��                  Z  v                  |)                       Z  <�  D�  4�                                             ��                  w  �                  L*                       w  ��  ��  ��     0                                        ��                  �  �                  +                       �  T�  \�  L�     D  	                                      ��             	     �  �                  ,                       �  ��  ��  ��     X  
                                      ��             
     �  �                  �,                       �  l�  t�  d�     l                                        ��                  �                    �-                       �  ��   �  ��     �                                        ��                    $                  �.                         ��  ��  |�     �                                        ��                  %  A                  \/                       %  �  �  �     �                                        ��                  B  ^                  `0                       B  ��  ��  ��     �                                        ��                  _  {                  01                       _  (�  0�   �     �                                        ��                  |  �                   2                       |  ��  ��  ��     �                                        ��                  �  �                  �2                       �  @�      8�     �                                        ��                  �  �                  4                       �  ��      O   �  ��  ��  �               ��          ��  ��   , ��                                                       �     ��                            ����                            <�  d�  X�  ��      `�     Z     ��                      � ��  }"                     ,�    �  ��  �      �      4   �����                �                      ��                  �                      5                       �  ��  ��  /   �  D�     T�                          3   ����(�            t�                      3   ����H�  ��  /   �  ��     ��                          3   ����`�            ��                      3   ������  \�  /   �  �     ,�                          3   ������            L�                      3   ������      /   �  ��     ��                          3   ����܀            ��                      3   ������  �     
                ��                     �  @        
 ��              � ߱        X�  V   m  ��  ���                        �  $  �  ��  ���                       ��                         � ߱        $�     
                ��                     ��  @        
 ��              � ߱        @�  V   �  ��  ���                        ��  $  �  l�  ���                       ��     
                    � ߱        �     
                ��                     ܆  @        
 ��              � ߱        (�  V   �  ��  ���                        ��  $  �  T�  ���                       �     
                    � ߱        ��     
                x�                     Ȉ  @        
 ��              � ߱        �  V   �  ��  ���                        ��  $  �  <�  ���                       ��                         � ߱        �     
                ��                     Ԋ  @        
 ��              � ߱        ��  V   �  h�  ���                        �  �     �      ��  $    8�  ���                       �     
                    � ߱         �     
                ��                     �  @        
 ��              � ߱        ��  V   #  d�  ���                        L�  $  =   �  ���                       ��     
                    � ߱        `�  �   W  �      ��  $  a  ��  ���                       L�     
                    � ߱        ��  �   {  `�      $�  $  �  ��  ���                       ��                         � ߱              �  @�  P�      ��      4   ������      /   �  |�     ��                          3   ����܍  ��     
   ��                      3   ������  ��        ��                      3   �����  �        �                      3   �����            <�                      3   ����4�  pushRowObjUpdTable  �  L�  �                   [      �                               �#                     pushTableAndValidate    `�  ��  �           |     \     �                          �  �#                     remoteCommit    ��  0�  �           p     ]     �                          �  J$                     serverCommit    @�  ��  �           l     ^     �                          �  W$                                     ��          ��  t�      ��                  �  �  ��              lcq                    O   ����    e�          O   ����    R�          O   ����    ��          O   �  ��  ��  d�    ��                            ����                            ��  P�      �              _      ��                      
�     d$                     disable_UI  �  p�                      `      �                               w$  
                    �  �    ����  �       ��          �  8   ����   0�  8   ����   @�  8   ����   P�  8   ����   `�  8   ����   p�  8   ����       8   ����       8   ����       ��  ��      viewObject  ,   ��  ��  ��      toggleData  ,INPUT plEnabled LOGICAL    ��  ��   �      showMessageProcedure    ,INPUT pcMessage CHARACTER,OUTPUT plAnswer LOGICAL  ��  D�  P�      returnFocus ,INPUT hTarget HANDLE   4�  x�  ��      repositionObject    ,INPUT pdRow DECIMAL,INPUT pdCol DECIMAL    h�  ��  ��      removeLink  ,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE ��  (�  8�      removeAllLinks  ,   �  L�  \�      modifyUserLinks ,INPUT pcMod CHARACTER,INPUT pcLinkName CHARACTER,INPUT phObject HANDLE <�  ��  ��      modifyListProperty  ,INPUT phCaller HANDLE,INPUT pcMode CHARACTER,INPUT pcListName CHARACTER,INPUT pcListValue CHARACTER    ��  @�  L�      hideObject  ,   0�  `�  l�      exitObject  ,   P�  ��  ��      editInstanceProperties  ,   p�  ��  ��      displayLinks    ,   ��  ��  ��      createControls  ,   ��  ��  �      changeCursor    ,INPUT pcCursor CHARACTER   ��  0�  <�      applyEntry  ,INPUT pcField CHARACTER     �  h�  x�      adjustTabOrder  ,INPUT phObject HANDLE,INPUT phAnchor HANDLE,INPUT pcPosition CHARACTER X�  ��  ��      addMessage  ,INPUT pcText CHARACTER,INPUT pcField CHARACTER,INPUT pcTable CHARACTER ��  4�  <�      addLink ,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE $�  ��  ��      unbindServer    ,INPUT pcMode CHARACTER ��  ��  ��      runServerObject ,INPUT phAppService HANDLE  ��  �  �      disconnectObject    ,   ��  ,�  <�      destroyObject   ,   �  P�  \�      bindServer  ,   @�  p�  ��      transferDBRow   ,INPUT pcRowIdent CHARACTER,INPUT piRowNum INTEGER  `�  ��  ��      startFilter ,   ��  ��  ��      releaseDBRow    ,   ��  �  �      refetchDBRow    ,INPUT phRowObjUpd HANDLE   ��  D�  \�      filterContainerHandler  ,INPUT phFilterContainer HANDLE 4�  ��  ��      fetchDBRowForUpdate ,   |�  ��  ��      confirmContinue ,INPUT-OUTPUT pioCancel LOGICAL ��  ��  �      compareDBRow    ,   ��  �  ,�      bufferCopyDBToRO    ,INPUT phRowObj HANDLE,INPUT phBuffer HANDLE,INPUT pcExcludes CHARACTER,INPUT pcAssigns CHARACTER   �  ��  ��      assignDBRow ,INPUT phRowObjUpd HANDLE   ��  ��  ��      updateState ,INPUT pcState CHARACTER    ��  �  $�      updateQueryPosition ,    �  8�  L�      updateAddQueryWhere ,INPUT pcWhere CHARACTER,INPUT pcField CHARACTER    (�  ��  ��      undoTransaction ,   ��  ��  ��      transferToExcel ,INPUT pcFieldList CHARACTER,INPUT plIncludeObj LOGICAL,INPUT plUseExisting LOGICAL,INPUT piMaxRecords INTEGER  ��  D�  \�      synchronizeProperties   ,INPUT pcPropertiesForServer CHARACTER,OUTPUT pcPropertiesForClient CHARACTER   4�  ��  ��      submitValidation    ,INPUT pcValueList CHARACTER,INPUT pcUpdColumns CHARACTER   ��  �  0�      submitForeignKey    ,INPUT pcRowIdent CHARACTER,INPUT-OUTPUT pcValueList CHARACTER,INPUT-OUTPUT pcUpdColumns CHARACTER  �  ��  ��      submitCommit    ,INPUT pcRowIdent CHARACTER,INPUT plReopen LOGICAL  ��  ��  �      startServerObject   ,   ��   �  0�      setPropertyList ,INPUT pcProperties CHARACTER   �  `�  |�      serverFetchRowObjUpdTable   ,OUTPUT TABLE-HANDLE phRowObjUpd    P�  ��  ��      serverSendRows  ,INPUT piStartRow INTEGER,INPUT pcRowIdent CHARACTER,INPUT plNext LOGICAL,INPUT piRowsToReturn INTEGER,OUTPUT piRowsReturned INTEGER,OUTPUT TABLE-HANDLE phRowObject    ��  x�  ��      saveContextAndDestroy   ,OUTPUT pcContext CHARACTER h�  ��  ��      rowObjectState  ,INPUT pcState CHARACTER    ��  ��  �      retrieveFilter  ,   ��  �  0�      restartServerObject ,   �  D�  T�      remoteSendRows  ,INPUT-OUTPUT piocContext CHARACTER,INPUT piStartRow INTEGER,INPUT pcRowIdent CHARACTER,INPUT plNext LOGICAL,INPUT piRowsToReturn INTEGER,OUTPUT pioRowsReturned INTEGER,OUTPUT TABLE-HANDLE phRowObject,OUTPUT pocMessages CHARACTER   4�  L�  X�      refreshRow  ,   <�  l�  |�      printToCrystal  ,INPUT pcFieldList CHARACTER,INPUT plIncludeObj LOGICAL,INPUT piMaxRecords INTEGER  \�  ��  ��      isUpdatePending ,INPUT-OUTPUT plUpdate LOGICAL  ��   �  8�      initializeServerObject  ,   �  L�  `�      initializeObject    ,   <�  t�  |�      home    ,   d�  ��  ��      genContextList  ,OUTPUT pcContext CHARACTER ��  ��  ��      fetchPrev   ,   ��  ��  ��      fetchNext   ,   ��  �  �      fetchLast   ,   ��  ,�  8�      fetchFirst  ,   �  L�  X�      fetchBatch  ,INPUT plForwards LOGICAL   <�  ��  ��      endClientDataRequest    ,   t�  ��  ��      destroyServerObject ,   ��  ��  ��      describeSchema  ,INPUT pcSdoName CHARACTER,OUTPUT TABLE-HANDLE hTtSchema    ��  4�  D�      dataAvailable   ,INPUT pcRelative CHARACTER $�  p�  |�      copyColumns ,INPUT pcViewColList CHARACTER,INPUT phDataQuery HANDLE `�  ��  ��      commitTransaction   ,   ��  ��  ��      clientSendRows  ,INPUT piStartRow INTEGER,INPUT pcRowIdent CHARACTER,INPUT plNext LOGICAL,INPUT piRowsToReturn INTEGER,OUTPUT piRowsReturned INTEGER    ��  ��  ��      batchServices   ,INPUT pcServices CHARACTER,OUTPUT pcValues CHARACTER        � 
"     
 =%     adecomm/as-utils.w  
"   
   �    }        �
"     
    �     }        �� .  Q   %               � 
" 
   
 � %              h �P  \         (          
�                          
�            � �   t
" 
   
 [
�H T   %              �     }        �GG %              � 
"   
   P �L 
�H T   %              �     }        �GG %              
"   
   �        �    7%               
"   
 ��               1� �  
 �� �   � %               o%   o           � �    �
"   
 ��           �    1� �   �� �   � %               o%   o           � �   �
"   
 ��           �    1� �  
 �� �   � %               o%   o           � �   �
"   
 ��           l    1� �   �� �   � %               o%   o           � �    �
"   
 ��           �    1� �   �� �   � %               o%   o           � �   �
"   
 ��           T    1�    ��    � %               o%   o           %               
"   
 � �          �    1� '   � � 7     
"   
 ��               1� >   �� �   � %               o%   o           � Q  �
"   
 ��           �    1� S   �� �   � %               o%   o           � b  S �
"   
 ��           �    1� �   ��    � %               o%   o           %               
"   
 ��           p    1� �   ��    � %               o%   o           %               
"   
 ��           �    1� �   ��    � %               o%   o           %              
"   
 � �          h    1� �   � �      
"   
 ��           �    1� �  
 ��    � %               o%   o           %               
"   
 ��                1� �   �� �   � %               o%   o           � �    �
"   
 � �          �    1�    � � 7     
"   
 ��           �    1�    �� �   � %               o%   o           � -  t �
"   
 � �          D	    1� �  
 � � 7     
"   
 ��           �	    1� �   �� �   � %               o%   o           � �  � �
"   
 ��           �	    1� K   �� �   � %               o%   o           � �    �
"   
 ��           h
    1� b  
 �� m   � %               o%   o           %               
"   
 ��           �
    1� q   ��    � %               o%   o           %              
"   
 [�           `    1� y   [� �   � %               o%   o           � �    �
"   
 [�           �    1� �   [� �   � %               o%   o           o%   o           
"   
 [�           P    1� �  
 [� �   � %               o%   o           � �    �
"   
 [�           �    1� �   [� �  	 � %               o%   o           � �  / [
"   
 � �          8    1� �   � � �  	   
"   
 ��           t    1�    �� �  	 � o%   o           o%   o           � �    �
"   
 � �          �    1�    � � �  	   
"   
 [�           $    1� $   [� �  	 � o%   o           o%   o           � �    [
"   
 � �          �    1� 4   � �      
"   
 � �          �    1� B   � � �  	   
"   
 � �              1� O   � � �  	   
"   
 � �          L    1� \   � � �  	   
"   
 [�           �    1� j   [�    � o%   o           o%   o           %              
"   
 � �              1� {   � � �  	   
"   
 � �          @    1� �  
 � � �     
"   
 � �          |    1� �   � � �  	   
"   
 � �          �    1� �   � � �  	   
"   
 � �          �    1� �   � � �  	   
"   
 � �          0    1� �   � � �  	   
"   
 � �          l    1� �  	 � � �  	   
"   
 � �          �    1� �   � � �  	   
"   
 � �          �    1� �   � � �  	   
"   
 [�                1�    [� �   � %               o%   o           o%   o           
�H T   %              �     }        �GG %              
"   
   
"   
 Y
"   
   
"   
 t(�  L ( l       �        �    �� "   � P   �        �    �@    
� @  , 
�            �� +     p�               �L
�    %              � 8          � $         � 2          
�    � L     
"   
 �� @  , 
�           �� �  
 �p�               �L"      P �L 
�H T   %              �     }        �GG %              
"   
 J�           �    1� O  
 J� �   � %               o%   o           � �    J
"   
 J�           <    1� Z  
 J� �   � %               o%   o           o%   o           
"   
 Y�           �    1� e   Y� 7   � %               o%   o           o%   o           
"   
 [�           4    1� n   [�    � %               o%   o           %               
"   
 ��           �    1� }   ��    � %               o%   o           %               
"   
 =�           ,    1� �   =� �   � %               o%   o           � �    �
"   
 [�           �    1� �   [�    � %               o%   o           %              
"   
 [�               1� �   [�    � %               o%   o           o%   o           
"   
 [�           �    1� �   [� �   � %               o%   o           o%   o           
"   
 Y�               1� �  	 Y� �   � %               o%   o           � �    �
"   
 Y�           �    1� �   Y� �   � %               o%   o           o%   o           
"   
 Y�               1� �   Y� �   � %               o%   o           o%   o           
"   
 ��           �    1� �   ��    � %               o%   o           %               
"   
 ��           �    1� �   ��    � %               o%   o           o%   o           P �L 
�H T   %              �     }        �GG %              
"   
 Y�           �    1�   
 Y�    � %               o%   o           %              
"   
 Y�           H    1�    Y� �   � %               o%   o           o%   o           
"   
 J�           �    1�    J� �   � %               o%   o           � �    [
"   
 J�           8    1� +   J� �   � %               o%   o           o%   o           
"   
 � �          �    1� 7   � � 7     
"   
 [�           �    1� D   [� �   � %               o%   o           � W  ! Y
"   
 [�           d    1� y   [� �   � %               o%   o           � �    [
"   
 ��           �    1� �   �� �   � %               o%   o           � �   [
"   
 � �          L    1� �   � � �     
"   
 � �          �    1� �   � � 7     
"   
 ��           �    1� �   �� �   � %               o%   o           � �    Y
"   
 � �          8     1� �  
 � � 7     
"   
 =�           t     1� �   =�    � %               o%   o           o%   o           
"   
 [�           �     1� �   [�    � %               o%   o           %               
"   
 Y�           l!    1�    Y�    � %               o%   o           %               
"   
 [�           �!    1�    [� �   � %               o%   o           � �    Y
"   
 [�           \"    1� "   [� �   � %               o%   o           o%   o           
"   
 J�           �"    1� .   J�    � %               o%   o           %              
"   
 [�           T#    1� ?   [�    � %               o%   o           %               
"   
 =�           �#    1� L   =�    � %               o%   o           %               
"   
 � �          L$    1� \   � � 7     
"   
 � �          �$    1� i   � � �     
"   
 [�           �$    1� v   [� m   � %               o%   o           o%   o           
"   
 [�           @%    1� �   [� �   � %               o%   o           � �    �
"   
 [�           �%    1� �   [� �   � %               o%   o           o%   o           
"   
 Y�           0&    1� �   Y�    � o%   o           o%   o           o%   o           
"   
 Y�           �&    1� �   Y� �  	 � %               o%   o           o%   o           
"   
 J�           ('    1� �   J� �   � %               o%   o           o%   o           
"   
 Y�           �'    1� �  
 Y� m   � %               o%   o           o%   o           
"   
 � �           (    1� �   � � �     
"   
 [�           \(    1� �   [� �   � %               o%   o           �   4 Y
"   
 [�           �(    1� 9  
 [�    � %               o%   o           %              
"   
 � �          L)    1� D   � � 7     
"   
 Y�           �)    1� U   Y� �   � %               o%   o           � �    =
"   
 ��           �)    1� c   ��    � %               o%   o           %              
"   
 [�           x*    1� r   [� �   � %               o%   o           � �    �
"   
 ��           �*    1�    �� �   � %               o%   o           � �    [
"   
 Y�           `+    1� �   Y� �   � %               o%   o           � �    �
"   
 Y�           �+    1� �   Y�    � %               o%   o           %               
"   
 Y�           P,    1� �  	 Y� 7   � %               o%   o           o%   o           
"   
 =�           �,    1� �   =� �   � %               o%   o           � �  	 [
"   
 Y�           @-    1� �   Y� m   � %               o%   o           %       �       
"   
 ��           �-    1� �   �� �   � %               o%   o           � �    Y
"   
 ��           0.    1� �   ��    � o%   o           o%   o           %              
"   
 ��           �.    1� �   ��    � %               o%   o           %               
"   
 ��           (/    1�    �� �   � %               o%   o           o%   o           
"   
 Y�           �/    1�    Y� �  	 � %               o%   o           � �    J
"   
 � �          0    1� )   � � �  	   P �L 
�H T   %              �     }        �GG %              
"   
 =�           �0    1� 6  
 =� �   � %               o%   o           � �    =
"   
 Y�           1    1� A   Y�    � %               o%   o           %               
"   
 ��           �1    1� N  	 �� �   � %               o%   o           � �    Y
"   
 [�           2    1� X   [� �   � %               o%   o           � �    �
"   
 ��           �2    1� f   ��    � %               o%   o           %               
"   
 Y�           �2    1� v   Y� �   � %               o%   o           � �    �
"   
 Y�           p3    1� �   Y� �   � %               o%   o           o%   o           
"   
 J�           �3    1� �   J� �   � %               o%   o           o%   o           
"   
 [�           h4    1� �   [�    � %               o%   o           o%   o           
"   
 =�           �4    1� �   =�    � %               o%   o           o%   o           
"   
 Y�           `5    1� �   Y�    � %               o%   o           o%   o           
"   
 ��           �5    1� �   �� �   � %               o%   o           o%   o           
"   
 Y�           X6    1� �  	 Y� �  	 � %               o%   o           � �    [
"   
 [�           �6    1� �  
 [� �  	 � %               o%   o           � �    Y
"   
 Y�           @7    1� �   Y� �   � %               o%   o           � �    [
"   
 Y�           �7    1�     Y� �   � %               o%   o           o%   o           
"   
 ��           08    1�    �� �   � %               o%   o           o%   o           
"   
 J�           �8    1�    J� �   � %               o%   o           � �    Y
"   
 ��            9    1� 0   �� �   � %               o%   o           � �    J
"   
 ��           �9    1� ?   �� �  	 � %               o%   o           o%   o           
"   
 � �          :    1� Q   � � 7     
"   
 [�           L:    1� ]   [� �   � %               o%   o           � �    Y
"   
 [�           �:    1� k   [� �   � %               o%   o           o%   o           
"   
 =�           <;    1� ~   =�    � %               o%   o           o%   o           
"   
 Y�           �;    1� �  
 Y� �   � %               o%   o           � �    �
"   
 Y�           ,<    1� �   Y� �   � %               o%   o           � �    Y
"   
 [�           �<    1� �   [�    � %               o%   o           %               P �L 
�H T   %              �     }        �GG %              
"   
 [�           p=    1� �  	 [� 7   � %               o%   o           o%   o           
"   
 [�           �=    1� �   [� 7   � %               o%   o           o%   o           
"   
 Y�           h>    1� �   Y� 7   � %               o%   o           o%   o           
"   
 =�           �>    1� �   =�    � %               o%   o           %              
"   
 ��           `?    1�     �� �   � %               o%   o           �   M =
"   
 Y�           �?    1� g   Y�    � %               o%   o           %              
"   
 Y�           P@    1� x   Y�    � %               o%   o           %               
"   
 ��           �@    1� �   ��    � %               o%   o           %               
"   
 Y�           HA    1� �   Y� �  	 � %               o%   o           � �   �
"   
 Y�           �A    1� �   Y�    � %               o%   o           %               
"   
 Y�           8B    1� �   Y� �  	 � %               o%   o           o%   o           
"   
 J�           �B    1� �   J�    � o%   o           o%   o           %              
"   
 =�           0C    1� �   =� �  	 � o%   o           o%   o           � �    =
"   
 ��           �C    1�    �� 7   � o%   o           o%   o           o%   o           
"   
 ��            D    1�    �� 7   � o%   o           o%   o           o%   o           
"   
 ��           �D    1� ,   �� �  	 � o%   o           o%   o           o%   o           
"   
 ��           E    1� <   �� 7   � o%   o           o%   o           o%   o           
"   
 ��           �E    1� K   �� �  	 � o%   o           o%   o           � Y   �
"   
 Y�           F    1� [   Y� �  	 � o%   o           o%   o           � j   Y
"   
 ��           |F    1� v   ��    � %               o%   o           %               
"   
 [�           �F    1� �   [�    � %               o%   o           %               
"   
 � �          tG    1� �   � � �  	   
"   
 [�           �G    1� �   [�    � %               o%   o           %               
"   
 [�           ,H    1� �   [� �   � %               o%   o           o%   o           
"   
 Y�           �H    1� �   Y� �   � %               o%   o           o%   o           
"   
 Y�           $I    1� �   Y�    � %               o%   o           o%   o           
"   
 Y�           �I    1� �   Y� �   � %               o%   o           � �    Y
"   
 J�           J    1�    J�    � %               o%   o           %               
"   
 [�           �J    1�   	 [�    � %               o%   o           %                "    � %     start-super-proc � %     adm2/smart.p �tP �L 
�H T   %              �     }        �GG %              
"   
   �       �K    6� "     
"   
   
�        �K    8
"   
   �        �K    ��     }        �G 4              
"   
 ߱G %              G %              %�   AppService,ASInfo,ASUsePrompt,CacheDuration,CheckCurrentChanged,DestroyStateless,DisconnectAppServer,ServerOperatingMode,ShareData,UpdateFromSource,ForeignFields,ObjectName,OpenOnInit,PromptColumns,PromptOnDelete,RowsToBatch,RebuildOnRepos,ToggleDataTargets  
�H T   %              �     }        �GG %              
"   
 t
"   
 � 
"   
 t
"   
   (�  L ( l       �        �M    �� "   � P   �        �M    �@    
� @  , 
�       �M    �� +   tp�               �L
�    %              � 8       N    � $         � 2          
�    � L   t
"   
 �p� @  , 
�       O    �� >   �p�               �L"  	  , �   � S   Y� U   � �     }        �A      |    "  	    � S   J%              (<   \ (    |    �     }        �A� W   �A"  
  Y    "  	  t"  
  Y  < "  	  t"  
  Y(    |    �     }        �A� W   �A"  
  Y
�H T   %              �     }        �GG %              
"   
 t
"   
 � 
"   
 t
"   
   (�  L ( l       �        �P    �� "   � P   �        �P    �@    
� @  , 
�       �P    �� +   tp�               �L
�    %              � 8      Q    � $         � 2          
�    � L   t
"   
 �p� @  , 
�       R    �� �  
 �p�               �L"  	  , 
�H T   %              �     }        �GG %              
"   
   
"   
 k
"   
   
"   
   (�  L ( l       �        �R    �� "   � P   �        �R    �@    
� @  , 
�       �R    �� +     p�               �L
�    %              � 8      �R    � $         � 2          
�    � L     
"   
 �p� @  , 
�       �S    �� �  
 �p�               �L%     SmartDataObject 
"   
   p� @  , 
�       XT    �� �     p�               �L%               
"   
  p� @  , 
�       �T    �� $    p�               �L%               
"   
  p� @  , 
�       U    ��     p�               �L(        � �      � �      � �      �     }        �A
�H T   %              �     }        �GG %              
"   
 Y (   � 
"   
 t    �        �U    �� "   �
"   
   � 8      DV    � $         � 2          
�    � L   t
"   
   �        �V    �
"   
   �       �V    /
"   
   
"   
   �       �V    6� "     
"   
   
�        W    8
"   
   �        4W    �
"   
   �       TW    �
"   
   p�    � �   J
�    �     }        �G 4              
"   
 ߱G %              G %              
�     }        �
"   
    (   � 
"   
 t    �        X    �A"    �A
"   
   
�        dX    �@ � 
"   
 Y"      �       }        �
"   
 � %              %                "    � %     start-super-proc � %     adm2/appserver.p �J�    �      
�    �     }        �%               %      Server  - �     }        �    "    [� �    � %                   "    [� �    � %      NONE    p�,  8         $     "    �        �    t
�    
�H T   %              �     }        �GG %              
"   
 t
"   
 � 
"   
 t
"   
   (�  L ( l       �        �Z    �� "   � P   �        �Z    �@    
� @  , 
�       �Z    �� +   tp�               �L
�    %              � 8      �Z    � $         � 2          
�    � L   t
"   
 �p� @  , 
�       �[    �� �   �p�               �L"    , p�,  8         $     "    �        � )   t
�     "    � %     start-super-proc � %     adm2/dataquery.p m=
�H T   %              �     }        �GG %              
"   
 t
"   
 � 
"   
 t
"   
 t(�  L ( l       �        ]    �� "   � P   �        ]    �@    
� @  , 
�       $]    �� +   tp�               �L
�    %              � 8      0]    � $         � 2   t     
�    � L   t
"   
 �p� @  , 
�       @^    �� �   �p�               �L%H > 8   dataAvailable,confirmContinue,isUpdatePending,buildDataRequest  
�H T   %              �     }        �GG %              
"   
 t
"   
 � 
"   
 t
"   
 t(�  L ( l       �        $_    �� "   � P   �        0_    �@    
� @  , 
�       <_    �� +   tp�               �L
�    %              � 8      H_    � $         � 2   t     
�    � L   t
"   
 �p� @  , 
�       X`    ��    �p�               �L%               "    � %     start-super-proc � %     adm2/query.p �t%     start-super-proc � %     adm2/queryext.p % 	    initProps t
�    %X M H   FOR EACH ITEM NO-LOCK,       EACH Almmmatg OF ITEM NO-LOCK INDEXED-REPOSITION � �   � �     � �     � �     
�     	         �G
"   
 Y�        �a    �G
"   
   
"   
    x    (0 4      �        b    �G%                   �        (b    �GG %              � �    t� �         %              %                   "      %              
"   
       "      �        c    �
"   
   �        Lc    �
"   
   
�       lc    �"       \      H   "    t((       "      %              � �      � �   t     
"   
   
"   
 �  \      H   "      ((       "      %              � �     � �   ��        d    �%                   %              %                   "  (    %                  "  (        
"   
 t
"   
 �0 T       m � "  (  =�        e    �A @   "      $         � "  (  �� W   � �        (e    �� "  (    
"   
 �  \ H     H   "      ((       "    t%              � �    � � �     (        "  !  t� �    ��        �e    �"  !  �
�H T   %              �     }        �GG %              
"   
 b
"   
 b
"   
   
"   
   (�  L ( l       �        �f    �� "   � P   �        �f    �@    
� @  , 
�       �f    �� +     p�               �L
�    %              � 8      �f    � $         � 2          
�    � L     
"   
 �p� @  , 
�       h    �� �   �p�               �L%               
"   
   p� @  , 
�       hh    ��      p�               �L"    , �,  8         $     "    �L        � �  
  
�    
�H T   %              �     }        �GG %              
"   
 $ 
"   
   
"   
 t
"   
 t(�  L ( l       �        Li    �� "   � P   �        Xi    �@    
� @  , 
�       di    �� +   tp�               �L
�    %              � 8      pi    � $         � 2   t     
�    � L     
"   
 �p� @  , 
�       �j    �� Q   �p�               �L
"   
 , 
"   
   p� @  , 
�       �j    �� 0     p�               �L"    , 
"   
  p� @  , 
�       0k    �� �    p�               �L"    ,     "    Y� �    � %p d `   OPEN QUERY Query-Main FOR EACH ITEM NO-LOCK,       EACH Almmmatg OF ITEM NO-LOCK INDEXED-REPOSITION. Ek    "    M � $     ((        "    EM%                   "    N.� *     "    t (   "           "    N.%              @ �,  8         $     "    t        � 6    
�    p�,  8         $     � C   k        � E   t
�    %� � �   rowObject.Por_Dsctos1 = ITEM.Por_Dsctos[1]  rowObject.Por_Dsctos2 = ITEM.Por_Dsctos[2]  rowObject.Por_Dsctos3 = ITEM.Por_Dsctos[3]  rowObject.PreVta1 = ITEM.PreVta[1]  rowObject.PreVta2 = ITEM.PreVta[2]  rowObject.PreVta3 = ITEM.PreVta[3]  �    "      � �         %              %                   "      %                  "      "      "     T(        "    k%              "    k� �   � "      �       "    t�    "    k� W   � � �      � W   t�    "     � W    S    "      "    �     "    k%                � @    �     t T     P   4       � "      (0       4       Y"      � �      � �    t� �   YT ,  %              T   "    Y"    � � �     � W   t� �   YT    �    "    Y� W   � "      � W   t"      %                   %              %                   "      %                  "      �     "      �     "       \      H   "      ((       "    t%              � �    � � @      4  [     "      
�H T   %              �     }        �GG %              
"   
 t
"   
 � 
"   
 t
"   
   (�  L ( l       �        �r    �� "   � P   �        �r    �@    
� @  , 
�       �r    �� +   tp�               �L
�    %              � 8      �r    � $         � 2          
�    � L   t
"   
 �p� @  , 
�       �s    �� 6  
 �p�               �L"    ,       "  
  O�    � B   � [� �   �       "  	    �    � B   � � � �   [�   � �     � �     � B   � t�   � �     � �   t� B   � [      "  
  ��    � �    �� �   �       "  	    �    � >!   � � �   �   ,        "    t� @    ��   � �   t� �   �� �    �    ,        "      � @      �   � �   �� �   � � >!   ��   � �     � �     � L!  	  
�H T   %              �     }        �GG %              
"   
 � 
"   
 t
"   
 � 
"   
 � (�  L ( l       �        �v    �� "   � P   �        �v    �@    
� @  , 
�       �v    �� +   � p�               �L
�    %              � 8      �v    � $         � 2          
�    � L     
"   
 �p� @  , 
�       �w    �� �   �p�               �L"    , 
"   
   p� @  , 
�       x    �� �     p�               �L"    , 
"   
  p� @  , 
�       hx    �� v    p�               �L"    ,     %              %                   "      %                  "      �     "      �     "      4 (        "  
    �    � B   �   � �         "  	  [�     "    kT    "      "      @ A,    �   � �   � � @      "    t"       T      @   "    � (        "      � �    t� �      � �   t"    �     "  	   %              D H   @ A,    �   � �   t� @      "    t"    a,    S   "    t� B   � a� �   � %                T      @   "    � (        "      � �    t� �      � �   t"    a     "  
   %                         "    � � @      "    t           "      � @    t"      
�H T   %              �     }        �GG %              
"   
 O
"   
   
"   
 O
"   
 t(�  L ( l       �        �|    �� "   � P   �        �|    �@    
� @  , 
�       �|    �� +   Op�               �L
�    %              � 8      �|    � $         � 2   t     
�    � L   � 
"   
 �p� @  , 
�       �}    �� �   �p�               �L"    , 
"   
   p� @  , 
�       ~    �� v     p�               �L"    , "      %               �     }        �%               �     }        �%              %              %              %              %              %              %       	       %       
       %              %              %              %              %              %              %              %              "       "    � %     start-super-proc � %     adm2/data.p %     start-super-proc � %     adm2/dataext.p %     start-super-proc � %     adm2/dataextcols.p %     start-super-proc � %     adm2/dataextapi.p �
�H T   %              �     }        �GG %              
"   
 t
"   
 � 
"   
 t
"   
 t(�  L ( l       �        h�    �� "   � P   �        t�    �@    
� @  , 
�       ��    �� +   tp�               �L
�    %              � 8      ��    � $         � 2   t     
�    � L   t
"   
 �p� @  , 
�       ��    �� �   �p�               �L%               %$     "pruebas/dtcotcreditodet.i" 
�H T   %              �     }        �GG %              
"   
 t
"   
 � 
"   
 t
"   
   (�  L ( l       �        p�    �� "   � P   �        |�    �@    
� @  , 
�       ��    �� +   tp�               �L
�    %              � 8      ��    � $         � 2          
�    � L   t
"   
 �p� @  , 
�       ��    �� �   �p�               �L"    , 
�     	        �G
�H T   %              �     }        �GG %              
"   
 t
"   
 � 
"   
 t
"   
   (�  L ( l       �        \�    �� "   � P   �        h�    �@    
� @  , 
�       t�    �� +   tp�               �L
�    %              � 8      ��    � $         � 2          
�    � L   t
"   
 �p� @  , 
�       ��    �� �  
 �p�               �L
"   
 , 
�     
        �G
�H T   %              �     }        �GG %              
"   
 t
"   
 � 
"   
 t
"   
   (�  L ( l       �        H�    �� "   � P   �        T�    �@    
� @  , 
�       `�    �� +   tp�               �L
�    %              � 8      l�    � $         � 2          
�    � L   t
"   
 �p� @  , 
�       |�    �� �  	 �p�               �L
"   
 , 
"   
 �      � �#  	   �        Ԉ    �
�H T   %              �     }        �GG %              
"   
 t
"   
 � 
"   
 t
"   
   (�  L ( l       �        T�    �� "   � P   �        `�    �@    
� @  , 
�       l�    �� +   tp�               �L
�    %              � 8      x�    � $         � 2          
�    � L   t
"   
 �p� @  , 
�       ��    ��    �p�               �L"    , 
"   
   �       ��    �"      
�     
        �G
�H T   %              �     }        �GG %              
"   
 t
"   
 � 
"   
 t
"   
   (�  L ( l       �        l�    �� "   � P   �        x�    �@    
� @  , 
�       ��    �� +   tp�               �L
�    %              � 8      ��    � $         � 2          
�    � L   t
"   
 �p� @  , 
�       ��    �� �  	 �p�               �L
"   
 , 
�             �Gp�,  8         $     
"   
 J        � �#   t
�    
�             �Gp�,  8         $     
"   
 J        � �#   t
�    �    � �#     
�        "    Y� �    � %     modifyListProperty  
�    %      REMOVE  %     SupportedLinks %     Update-Target  %     bufferValidate  
�    "      �  %      setContextAndInitialize 
�    "      %     bufferCommit    
�    "      "      �    � 3$     
�    %               %     bufferCommit    
�    "      "      
�     
        �G�     }        �
�                    �           �   l       ��                 x  �  �               �Hk                    O   ����    e�          O   ����    R�          O   ����    ��        $  �  �   ���                       �U     
                    � ߱              �  (  �      V      4   ����V                �                      ��                  �  �                  ��1                       �  8  �  �  �  PV            �  �  `      �V      4   �����V                p                      ��                  �  �                  |�1                       �  �  �  o   �      ,                                 �  �   �  �V      �  �   �  �V      $  $  �  �  ���                        W     
                    � ߱        8  �   �  @W      L  �   �  `W      `  �   �  �W          $   �  �  ���                       �W  @         �W              � ߱                     T          ,  @   T �            
             
             
             
                 $   4   D          $   4   D   ����        ��                            ����                                            �           �   l       ��                 �    �               ̮1                    O   ����    e�          O   ����    R�          O   ����    ��      �                      �          �  $  �    ���                       X     
                    � ߱                  �  �                      ��                   �  �                  @�1                     �  4      4   ����$X      $  �  �  ���                       pX     
                    � ߱        �    �  4  D      �X      4   �����X      /  �  p                               3   �����X  �  �   �  �X          O   �  ��  ��  �X                               , �                          
                               �      ��                            ����                                            �           �   l       ��            	     g  �  �               �J                    O   ����    e�          O   ����    R�          O   ����    ��        $  �  �   ���                       `a                         � ߱        �  $  �  8  ���                       �a                         � ߱        �a     
                4b  @         b              � ߱        D  $   �  d  ���                         T      �  x                      ��        0         �  �                  ��o      Xc     L     �  �      $  �  �  ���                       �b                         � ߱          $  �  �  ���                       �b                         � ߱            4   ����$c  xc     
                �c                     d                         � ߱          $  �    ���                                                            ��                  �  �                  |p                �     �  �  �  $  �  L  ���                       �d       !       !           � ߱          �      L  �                      ��        0         �  �                  dp     ( e            �  x      $  �     ���                       �d       (       (           � ߱        �  $  �  x  ���                       �d       (       (           � ߱            4   ���� e        �  �  L      4e      4   ����4e                \                      ��                  �  �                  d
p                       �  �  �  $  �  �  ���                       �e       !       !           � ߱            O   �  �� ��          $  �  �  ���                       �e                         � ߱        �f     
                g                     Th  @        
 h          �h  @        
 th          �h                      i     
                |i                     �j  @        
 �j          $k  @        
 �j          |k  @        
 <k              � ߱        x  V   �  $  ���                        P	    �  �  $	      �k      4   �����k  �k                     l                     <l                     �l                         � ߱            $  �  �  ���                       �	    �  l	  |	      �l      4   �����l      �   �  m      �	  $  �  �	  ���                       Xm                         � ߱        �
  $  �  
  ���                       Tn                         � ߱          �
                              ��        0         �  �                  tp      �n     �     �  @
      $  �  �
  ���                       tn                         � ߱        l  $  �  @  ���                       �n                         � ߱            4   �����n  �n                     @o                     Lo                     �o                     �o                         � ߱        D  $  �  |  ���                             �  `  p      �o      4   �����o      $  �  �  ���                       p          0q             � ߱        �  $  �  �  ���                       <q                         � ߱          �      �  \                      ��        0         �  �                  ��K      �q          �         $  �  �  ���                       Pq                         � ߱        L  $  �     ���                       �q                         � ߱            4   �����q      $  �  �  ���                       �q                         � ߱        dr     
                �r                     0t  @        
 �s              � ߱        �  V   �  �  ���                        <t       
       
       pt       	       	       �t                     �t                         � ߱        p  $  1  D  ���                       �t       
       
       0u       	       	       du                     �u                         � ߱        �  $  X  �  ���                       �  $  �  �  ���                       v                         � ߱        8v     
                �v                     x  @        
 �w          \x  @        
 x          �x  @        
 tx              � ߱        (  V   �  �  ���                          8      �                        ��        0    	     E  Z                  |�I      @y     �     E  �      $  E  d  ���                       �x                         � ߱        �  $  E  �  ���                       �x                         � ߱        �  4   ����y      4   ����Ty  `  $  J  4  ���                       �y                         � ߱        |    L  |  �      �y      4   �����y                P                      ��                  M  Q                  ��I                       M  �  z                     �z       	       	           � ߱            $  N    ���                             S  �        �z      4   �����z  	              l                      ��             	     U  Y                  \�I                       U  �  @{                     �{       
       
           � ߱            $  V  $  ���                       �{                     |                         � ߱        �  $  `  �  ���                       8|     
                �|                     ~  @        
 �}          \~  @        
 ~              � ߱            V   n    ���                                    J           |  D  � X�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            
             
                                                                                                                                                                                                                               ) �   �   �   �       (  8  H  X  h  x  �  �  �  �  �  �      (  8  H  X  h  x  �  �  �  �  �  �  �  �      (  8  H  8HXhx��������(8HX     ) �   �   �   �      (  8  H  X  h  x  �  �  �  �  �   �     (  8  H  X  h  x  �  �  �  �  �  �  �  �      (  8  H  8HXhx��������(8HX  �   :                  � �                     �    ��                      ��                            ����                            V"                          GI                                �   l       ��                  ;  F  �               4�>                    O   ����    e�          O   ����    R�          O   ����    ��              !                    ��                            ����                                            �           �   l       ��                  P  _  �               \�>                    O   ����    e�          O   ����    R�          O   ����    ��      �#       �              �                  $                  d  /  \  $     4  l�                      3   ����P�            T                      3   ����t�      O   ]  ��  ��  ��               �          �  �    �                                             ��                            ����                                            H          �   l       ��                  i  �  �               ?                    O   ����    e�          O   ����    R�          O   ����    ��      $       �              �                $                  $       ,             �          )$                                �  /  �  t     �  ��                      3   ������            �                      3   ������     /  �  �     �  ؎                      3   ������  x                             3   ������      $   �  L  ���                                                   � ߱                  �  �                  3   �����      $   �  �  ���                                                   � ߱        X  $  �  ,  ���                       ��                         � ߱            O   �  ��  ��  �               �          �  �   @ �                                                              0              0           ��                            ����                                                       �   l       ��                  �  �  �               0s�                    O   ����    e�          O   ����    R�          O   ����    ��      �       $                  $                    �          )$                      �              /  �  L     \  D�                      3   ����(�  �        |  �                  3   ����L�      $   �  �  ���                                                   � ߱                                      3   ����X�      $   �  @  ���                                                   � ߱                     �          �  �   , �                                                                 ��                            ����                                            �           �   l       ��                  T  _  �               dT                    O   ����    e�          O   ����    R�          O   ����    ��            ^  �   �       x�      4   ����x�      �   ^  ��    ��                            ����                            TXS appSrvUtils ITEM Detalle pedido credito CodDoc CodCia Factor UndVta TipVta codmat CodAux NroPed CanPed PreUni PorDto PorDto2 ImpDto ImpLin NroItm AftIgv AftIsc PreBas PreVta ImpIgv ImpIsc canate Hora FlgEst FchPed MrgUti Pesmat CodCli AlmDes Por_Dsctos Flg_Factor CodDiv CanPick CanSol CanApr ImpDto2 AlmTrf CanTrf SdoCot Libre_c01 Libre_c02 Libre_c03 Libre_c04 Libre_c05 Libre_d01 Libre_d02 Libre_d03 Libre_d04 Libre_d05 Libre_f01 Libre_f02 Libre_f03 Libre_f04 Libre_f05 CodMatWeb DesMatWeb CanPedWeb PreUniWeb ImpLinWeb Almmmatg Cat�logo de Materiales C:\newsie\on_in_co\aplic\pruebas\dtcotcreditodet.w should only be RUN PERSISTENT. ADDROW CANCELROW CANNAVIGATE CLOSEQUERY COLUMNPROPS COLVALUES COPYROW CREATEROW DELETEROW FETCHROW FETCHROWIDENT FINDROW FINDROWWHERE FIRSTROWIDS GETLASTCOMMITERRORTYPE HASFOREIGNKEYCHANGED OPENDATAQUERY OPENQUERY PREPAREQUERY ROWAVAILABLE ROWVALUES SUBMITROW UPDATEROW GETOBJECTTYPE xiRocketIndexLimit ADDQUERYWHERE ASSIGNQUERYSELECTION BUFFERCOMPAREDBTORO BUFFERWHERECLAUSE COLUMNDATATYPE COLUMNDBCOLUMN COLUMNQUERYSELECTION COLUMNTABLE COLUMNVALMSG DBCOLUMNDATANAME DBCOLUMNHANDLE EXCLUDECOLUMNS GETDATACOLUMNS GETFOREIGNVALUES GETQUERYPOSITION GETQUERYSORT GETQUERYSTRING GETQUERYWHERE GETTARGETPROCEDURE INDEXINFORMATION INSERTEXPRESSION NEWQUERYSTRING NEWQUERYVALIDATE NEWQUERYWHERE NEWWHERECLAUSE REFRESHROWIDENT REMOVEFOREIGNKEY REMOVEQUERYSELECTION RESOLVEBUFFER ROWIDWHERE ROWIDWHERECOLS SETQUERYPOSITION SETQUERYSORT SETQUERYSTRING SETQUERYWHERE WHERECLAUSEBUFFER GETAPPSERVICE GETASBOUND GETASDIVISION GETASHANDLE GETASHASSTARTED GETASINFO GETASINITIALIZEONRUN GETASUSEPROMPT GETSERVERFILENAME GETSERVEROPERATINGMODE RUNSERVERPROCEDURE SETAPPSERVICE SETASDIVISION SETASHANDLE SETASINFO SETASINITIALIZEONRUN SETASUSEPROMPT SETSERVERFILENAME SETSERVEROPERATINGMODE gshAstraAppserver gshSessionManager gshRIManager gshSecurityManager gshProfileManager gshRepositoryManager gshTranslationManager gshWebManager gscSessionId gsdSessionObj gshFinManager gshGenManager gshAgnManager gsdTempUniqueID gsdUserObj gsdRenderTypeObj gsdSessionScopeObj ghProp ghADMProps ghADMPropsBuf glADMLoadFromRepos glADMOk ANYMESSAGE ASSIGNLINKPROPERTY FETCHMESSAGES GETCHILDDATAKEY GETCONTAINERHANDLE GETCONTAINERHIDDEN GETCONTAINERSOURCE GETCONTAINERSOURCEEVENTS GETCONTAINERTYPE GETDATALINKSENABLED GETDATASOURCE GETDATASOURCEEVENTS GETDATASOURCENAMES GETDATATARGET GETDATATARGETEVENTS GETDBAWARE GETDESIGNDATAOBJECT GETDYNAMICOBJECT GETINSTANCEPROPERTIES GETLOGICALOBJECTNAME GETLOGICALVERSION GETOBJECTHIDDEN GETOBJECTINITIALIZED GETOBJECTNAME GETOBJECTPAGE GETOBJECTPARENT GETOBJECTVERSION GETOBJECTVERSIONNUMBER GETPARENTDATAKEY GETPASSTHROUGHLINKS GETPHYSICALOBJECTNAME GETPHYSICALVERSION GETPROPERTYDIALOG GETQUERYOBJECT GETRUNATTRIBUTE GETSUPPORTEDLINKS GETTRANSLATABLEPROPERTIES GETUIBMODE GETUSERPROPERTY INSTANCEPROPERTYLIST LINKHANDLES LINKPROPERTY MAPPEDENTRY MESSAGENUMBER PROPERTYTYPE REVIEWMESSAGES SETCHILDDATAKEY SETCONTAINERHIDDEN SETCONTAINERSOURCE SETCONTAINERSOURCEEVENTS SETDATALINKSENABLED SETDATASOURCE SETDATASOURCEEVENTS SETDATASOURCENAMES SETDATATARGET SETDATATARGETEVENTS SETDBAWARE SETDESIGNDATAOBJECT SETDYNAMICOBJECT SETINSTANCEPROPERTIES SETLOGICALOBJECTNAME SETLOGICALVERSION SETOBJECTNAME SETOBJECTPARENT SETOBJECTVERSION SETPARENTDATAKEY SETPASSTHROUGHLINKS SETPHYSICALOBJECTNAME SETPHYSICALVERSION SETRUNATTRIBUTE SETSUPPORTEDLINKS SETTRANSLATABLEPROPERTIES SETUIBMODE SETUSERPROPERTY SHOWMESSAGE SIGNATURE prepareInstance ObjectName CHAR  ObjectVersion ADM2.2 ObjectType SmartDataObject ContainerType PropertyDialog adm2/support/datad.w QueryObject LOGICAL ContainerHandle HANDLE InstanceProperties AppService,ASInfo,ASUsePrompt,CacheDuration,CheckCurrentChanged,DestroyStateless,DisconnectAppServer,ServerOperatingMode,ShareData,UpdateFromSource,ForeignFields,ObjectName,OpenOnInit,PromptColumns,PromptOnDelete,RowsToBatch,RebuildOnRepos,ToggleDataTargets SupportedLinks Data-Source,Data-Target,Navigation-Target,Update-Target,Commit-Target,Filter-Target ContainerHidden ObjectInitialized ObjectHidden ObjectsCreated HideOnInit UIBMode ContainerSource ContainerSourceEvents initializeObject,hideObject,viewObject,destroyObject,enableObject,confirmExit,confirmCancel,confirmOk,isUpdateActive DataSource DataSourceEvents dataAvailable,queryPosition,updateState,deleteComplete,fetchDataSet,confirmContinue,confirmCommit,confirmUndo,assignMaxGuess,isUpdatePending TranslatableProperties ObjectPage INT DBAware DesignDataObject DataSourceNames DataTarget DataTargetEvents CHARACTER updateState,rowObjectState,fetchBatch,LinkState LogicalObjectName PhysicalObjectName LogicalVersion PhysicalVersion DynamicObject RunAttribute ChildDataKey ParentDataKey DataLinksEnabled InactiveLinks InstanceId DECIMAL SuperProcedure SuperProcedureMode SuperProcedureHandle LayoutPosition ClassName RenderingProcedure ThinRenderingProcedure Label cType ADMProps Target WHERE Target = WIDGET-H(" ") AppService ASDivision ASHandle ASHasConnected ASHasStarted ASInfo ASInitializeOnRun ASUsePrompt BindSignature BindScope ServerOperatingMode ServerFileName ServerFirstCall NeedContext AutoCommit BLOBColumns BufferHandles CLOBColumns CommitSource CommitSourceEvents commitTransaction,undoTransaction CommitTarget CommitTargetEvents rowObjectState CurrentRowid ROWID CurrentUpdateSource DataColumns DataHandle DataIsFetched DataModified DataQueryBrowsed DataQueryString FetchOnOpen FillBatchOnRepos FilterActive FilterAvailable FilterSource FilterWindow FirstRowNum ForeignFields ForeignValues IgnoreTreeViewFilter IndexInformation LargeColumns LastRowNum NavigationSource NavigationSourceEvents fetchFirst,fetchNext,fetchPrev,fetchLast,startFilter OpenOnInit PrimarySDOSource PromptColumns PromptOnDelete QueryColumns QueryPosition QueryString RebuildOnRepos RowObject RowObjectState NoUpdates RowsToBatch Tables ToggleDataTargets TransferChildrenForAll UpdatableColumns UpdatableWhenNew UpdateSource AssignList AuditEnabled BaseQuery CalcFieldList CheckLastOnOpen DataColumnsByTable DBNames EntityFields FetchHasAudit FetchHasComment FetchAutoComment FirstResultRow KeyFields KeyTableId LastDBRowIdent LastResultRow NewBatchInfo NoLockReadOnlyTables PhysicalTables PositionForClient QueryHandle QueryRowIdent RequiredProperties SkipTransferDBRow TempTables UpdatableColumnsByTable UpdateFromSource RowObjUpd RowObjectTable RowObjUpdTable CheckCurrentChanged StatelessSavedProperties CheckCurrentChanged,RowObjectState,LastResultRow,FirstResultRow,QueryRowIdent DestroyStateless DisconnectAppServer ServerSubmitValidation DataFieldDefs "pruebas/dtcotcreditodet.i" QueryContainer QueryContext AsynchronousSDO DataLogicProcedure DataLogicObject DataReadHandler DataReadColumns DataReadBuffer DataDelimiter | DataReadFormat TrimNumeric IsRowObjectExternal IsRowObjUpdExternal ManualAddQueryWhere DynamicData LastCommitErrorType LastCommitErrorKeys RunDataLogicProxy SchemaLocation CacheDuration INTEGER ShareData ghContainer adm2/smart.p cObjectName iStart / \ . hReposBuffer hPropTable hBuffer hTable deleteProperties ADM-CLONE-PROPS pcProcName hProc START-SUPER-PROC cAppService cASDivision cServerOperatingMode adm2/appserver.p getAppService Server NONE setASDivision setAppService adm2/dataquery.p dataAvailable,confirmContinue,isUpdatePending,buildDataRequest adm2/query.p adm2/queryext.p cTable iTable cColumns cDataCols cUpdCols cCalcData cCalcUpd iNumData iNumUpd cBuffers cKeyFields cAssignList iAssigns iPos iEntry iCount cTables cTableAssign cDbEntry cField cKeyTable cQueryString FOR EACH ITEM NO-LOCK,       EACH Almmmatg OF ITEM NO-LOCK INDEXED-REPOSITION ,   ITEM Almmmatg hQuery cOpenQuery cDBNames cPhysicalTables cKeyTableEntityFields cKeyTableEntityValues cKeyTableEntityMnemonic cKeyTableEntityObjField cDBName cEntityFields lHasObjectField lHasAudit lHasComment lHasAutoComment iLookup iAlias  STATIC setDBNames OPEN QUERY Query-Main FOR EACH ITEM NO-LOCK,       EACH Almmmatg OF ITEM NO-LOCK INDEXED-REPOSITION.  FOR   PRESELECT  setOpenQuery 5 showMessage rowObject.Por_Dsctos1 = ITEM.Por_Dsctos[1]  rowObject.Por_Dsctos2 = ITEM.Por_Dsctos[2]  rowObject.Por_Dsctos3 = ITEM.Por_Dsctos[3]  rowObject.PreVta1 = ITEM.PreVta[1]  rowObject.PreVta2 = ITEM.PreVta[2]  rowObject.PreVta3 = ITEM.PreVta[3] ; AftIgv AftIsc CanPed CodCia CodCli CodDiv CodDoc codmat Factor FchPed FlgEst Hora ImpDto ImpDto2 ImpIgv ImpIsc ImpLin Libre_c05 MrgUti NroItm NroPed PorDto PorDto2 Por_Dsctos1 Por_Dsctos2 Por_Dsctos3 PreBas PreUni PreVta1 PreVta2 PreVta3 UndVta CanApr DesMat DesMar AftIgv AftIsc CanPed CodCia CodCli CodDiv CodDoc codmat Factor FchPed FlgEst Hora ImpDto ImpDto2 ImpIgv ImpIsc ImpLin Libre_c05 MrgUti NroItm NroPed PorDto PorDto2 Por_Dsctos1 Por_Dsctos2 Por_Dsctos3 PreBas PreUni PreVta1 PreVta2 PreVta3 UndVta CanApr DesMat DesMar Query-Main INITPROPS piTableIndex lRet DELETERECORDSTATIC adm2/data.p adm2/dataext.p adm2/dataextcols.p adm2/dataextapi.p Por_Dsctos1 Por_Dsctos2 Por_Dsctos3 PreVta1 PreVta2 PreVta3 DesMat DesMar RowNum RowIdent RowMod RowIdentIdx RowUserProp ChangedFields cContainerType hRowObject hDataQuery cDataFieldDefs FOR EACH  setRowObjectTable setRowObjUpdTable getUpdatableColumns REMOVE Update-Target PUSHROWOBJUPDTABLE pcValType PUSHTABLEANDVALIDATE pcContext pcMessages pcUndoIds obtainContextForClient REMOTECOMMIT SERVERCOMMIT GETROWOBJUPDSTATIC DISABLE_UI llave01 llave02 llave03 llave04 qDataQuery   H:  L  �H      / �<   ��      0         pcServices      ��      T         pcServices  �   ��      x         piStartRow  �   ��      �         piStartRow  �   ��      �         piStartRow  �   ��      �         piStartRow      ��              piStartRow  <  ��      ,        pcViewColList       ��      T       
 pcViewColList       ��      |        pcRelative  �  ��      �        pcSdoName       ��      �  �     
 pcSdoName       ��      �        plForwards      ��              pcContext       ��      0        plUpdate    `  ��      T        pcFieldList �  ��      x        pcFieldList     ��      �        pcFieldList �  ��      �        piocContext �  ��      �        piocContext   ��              piocContext 8  ��      ,        piocContext \  ��      P        piocContext �  ��      t        piocContext �  ��      �  �     
 piocContext     ��      �        piocContext     ��      �        pcState     ��               pcContext   0  ��      $        piStartRow  T  ��      H        piStartRow  x  ��      l        piStartRow  �  ��      �        piStartRow  �  ��      �        piStartRow      ��      �  �     
 piStartRow      ��      �  �     
 phRowObjUpd     ��               pcProperties    T  ��      H        piStartRow  x  ��      l        piStartRow  �  ��      �        piStartRow  �  ��      �        piStartRow  �  ��      �        piStartRow      ��      �  �     
 piStartRow  ,  ��               pcRowIdent      ��      D        pcRowIdent  t  ��      h        pcRowIdent  �  ��      �        pcRowIdent      ��      �        pcRowIdent  �  ��      �        pcValueList     ��      �        pcValueList 4  ��              pcPropertiesForServer       ��      L        pcPropertiesForServer   �  ��      |        pcFieldList �  ��      �        pcFieldList �  ��      �        pcFieldList     ��      �        pcFieldList   ��              pcWhere     ��      ,        pcWhere     ��      L        pcState     ��      l       
 phRowObjUpd �  ��      �       
 phRowObj    �  ��      �       
 phRowObj    �  ��      �        phRowObj        ��      �        phRowObj        ��       	        pioCancel       ��      D	        pcRelative      ��      h	       
 phFilterContainer       ��      �	       
 phRowObjUpd �	  ��      �	        pcRowIdent      ��      �	        pcRowIdent      ��       
       
 phAppService        ��      (
        pcMode  T
  ��      H
       
 phSource    x
  ��      l
        phSource        ��      �
       
 phSource    �
  ��      �
        pcText  �
  ��      �
        pcText      ��      �
        pcText     ��             
 phObject    D  ��      8       
 phObject        ��      \        phObject        ��      �        pcField     ��      �        pcCursor    �  ��      �       
 phCaller    �  ��      �        phCaller      ��              phCaller        ��      0        phCaller    \  ��      T        pcMod   |  ��      t        pcMod       ��      �       
 pcMod   �  ��      �       
 phSource    �  ��      �        phSource        ��      �       
 phSource    (  ��               pdRow       ��      @        pdRow       ��      `       
 hTarget �  ��      �        pcMessage       ��      �        pcMessage       ��      �        plEnabled             �     cType       0     V   �                             getObjectType   j	  �	  �	  `        P  
   hReposBuffer    �        t  
   hPropTable  �        �  
   hBuffer           �  
   hTable  �  �     W   <          �                  adm-clone-props �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �            P  
   hProc             p        pcProcName  �  �  	   X   <  X      �                  start-super-proc    �  �  �  �  �  �  �  �             �     cTable               iTable  <        0     cColumns    \        P     cDataCols   |        p     cUpdCols    �        �     cCalcData   �        �     cCalcUpd    �     	   �     iNumData    �     
   �     iNumUpd              cBuffers    8        ,     cKeyFields  X        L     cAssignList x        l     iAssigns    �        �     iPos    �        �     iEntry  �        �     iCount  �        �     cTables        �     cTableAssign    ,              cDbEntry    H        @     cField  h        \     cKeyTable   �        |     cQueryString    �        �  
   hQuery  �        �  
   hBuffer �        �     cOpenQuery          �     cDBNames    (             cPhysicalTables T        <     cKeyTableEntityFields   �        h     cKeyTableEntityValues   �        �     cKeyTableEntityMnemonic �         �     cKeyTableEntityObjField �     !   �     cDBName      "        cEntityFields   <     #   ,     lHasObjectField \     $   P     lHasAudit   |     %   p     lHasComment �     &   �     lHasAutoComment �     '   �     iLookup        (   �     iAlias  |    4   Y   �                            initProps   �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  1  X  �  �  E  J  L  M  N  Q  S  U  V  Y  Z  `  n  �            �     lRet                      piTableIndex    �  l  ,   Z   �         X                  deleteRecordStatic  �  �  �  �  �  �  �  �           <  =  Y  Z  v  w  �  �  �  �  �  �  �  �      $  %  A  B  ^  _  {  |  �  �  �  �  �  �  �  �                 !       (  x     [             d                  pushRowObjUpdTable  F  �        �        pcValType                  $       4        \       |      �                  pushTableAndValidate    \  ]  _  0        $        pcContext   H             $       l        `        pcMessages            �        pcUndoIds   �  �     ]             �                  remoteCommit    �  �  �  �  �  �             $                        pcMessages            8        pcUndoIds   �  �     ^       �      t                  serverCommit    �  �  D  �     _               �                  getRowObjUpdStatic  �  �  �       `                                 disable_UI  ^  _  �  �+  
     $&      ,+                      X  \  d  ;   ITEM    (         0         8         @         H         P         X         `         h         p         x         �         �         �         �         �        �         �         �         �         �         �         �         �         �         �         �                                             (         0         8         D         P         \         h         t         �         �         �         �         �         �         �         �         �         �         �         �         �                                    (         4         @         L         CodDoc  CodCia  Factor  UndVta  codmat  NroPed  CanPed  PreUni  PorDto  ImpDto  ImpLin  NroItm  AftIgv  AftIsc  PreBas  PreVta  ImpIgv  ImpIsc  canate  Hora    FlgEst  FchPed  CodAux  MrgUti  PorDto2 TipVta  Pesmat  CodCli  AlmDes  Por_Dsctos  Flg_Factor  CodDiv  CanPick Libre_c01   Libre_c02   Libre_c03   Libre_c04   Libre_c05   Libre_d01   Libre_d02   Libre_f01   Libre_f02   CanSol  CanApr  ImpDto2 AlmTrf  CanTrf  Libre_d03   Libre_d04   Libre_d05   Libre_f05   Libre_f03   Libre_f04   SdoCot  CodMatWeb   DesMatWeb   CanPedWeb   PreUniWeb   ImpLinWeb   �"  h  t  (   RowObject   T!         \!         d!         l!         t!         |!         �!         �!         �!         �!         �!         �!         �!         �!         �!         �!         �!         �!         �!         �!         �!          "         "         "         "         ("         4"         <"         D"         L"         T"         \"         d"         l"         t"         |"         �"         �"         �"         �"         AftIgv  AftIsc  CanPed  CodCia  CodCli  CodDiv  CodDoc  codmat  Factor  FchPed  FlgEst  Hora    ImpDto  ImpDto2 ImpIgv  ImpIsc  ImpLin  Libre_c05   MrgUti  NroItm  NroPed  PorDto  PorDto2 Por_Dsctos1 Por_Dsctos2 Por_Dsctos3 PreBas  PreUni  PreVta1 PreVta2 PreVta3 UndVta  CanApr  DesMat  DesMar  RowNum  RowIdent    RowMod  RowIdentIdx RowUserProp     �"  �"  )   RowObjUpd   �$         �$         �$         �$         �$         �$         �$         �$         �$          %         %         %         %          %         (%         0%         8%         @%         L%         T%         \%         d%         l%         t%         �%         �%         �%         �%         �%         �%         �%         �%         �%         �%         �%         �%         �%         �%         �%         &         &         AftIgv  AftIsc  CanPed  CodCia  CodCli  CodDiv  CodDoc  codmat  Factor  FchPed  FlgEst  Hora    ImpDto  ImpDto2 ImpIgv  ImpIsc  ImpLin  Libre_c05   MrgUti  NroItm  NroPed  PorDto  PorDto2 Por_Dsctos1 Por_Dsctos2 Por_Dsctos3 PreBas  PreUni  PreVta1 PreVta2 PreVta3 UndVta  CanApr  DesMat  DesMar  RowNum  RowIdent    RowMod  RowIdentIdx RowUserProp ChangedFields   D&          8&  
   appSrvUtils l&       X&     xiRocketIndexLimit  �&        �&  
   gshAstraAppserver   �&        �&  
   gshSessionManager   �&        �&  
   gshRIManager    '        �&  
   gshSecurityManager  0'  	 	     '  
   gshProfileManager   \'  
 
     D'  
   gshRepositoryManager    �'        p'  
   gshTranslationManager   �'        �'  
   gshWebManager   �'        �'     gscSessionId    �'        �'     gsdSessionObj   (        (  
   gshFinManager   <(        ,(  
   gshGenManager   `(        P(  
   gshAgnManager   �(        t(     gsdTempUniqueID �(        �(     gsdUserObj  �(        �(     gsdRenderTypeObj    �(        �(     gsdSessionScopeObj  )       )  
   ghProp  0)       $)  
   ghADMProps  T)       D)  
   ghADMPropsBuf   |)       h)     glADMLoadFromRepos  �)       �)     glADMOk �)       �)  
   ghContainer �)    	   �)     cObjectName �)    
   �)     iStart  *       *     cAppService 4*       (*     cASDivision `*       H*     cServerOperatingMode    �*       t*     cContainerType  �*       �*     cQueryString    �*       �*  
   hRowObject  �*       �*  
   hDataQuery  +       �*     cColumns             +     cDataFieldDefs  D+    �  <+  ITEM    `+       T+  Almmmatg    |+    X  p+  RowObject         X  �+  RowObjUpd            @   �   �   �   �   8  9  :  ;  R  ^  _  `  b  d  e  f  j  k  n  o  p  q  s  u  w  y  z  {  ~  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  /	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  +
  [
  \
  ^
  _
  `
  a
  b
  c
  e
  f
  g
  h
  i
  j
  k
  l
  m
  n
  o
  p
  q
  r
  s
  t
  u
  v
  w
  x
  y
  z
  {
  |
  }
  
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
    	                                           !  "  #  $  %  &  '  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  G  �  �                  8  J  i  k  �       !  ;  K  L  M  P  Q  R  Y  Z  w  �  �  =  >  J  n  �  �  �  �  �  [  �  �  �  �  �  �     m  �  �  �  �  �  �  �  �      #  =  W  a  {  �  �  �      ��  C:\Progress\OpenEdge\src\adm2\data.i �/  �) . %C:\Progress\OpenEdge\src\adm2\custom\datacustom.i    �/  �� - C:\Progress\OpenEdge\src\adm2\robjflds.i 00  �W , .\aplic\pruebas\dtcotcreditodet.i    d0  �:  C:\Progress\OpenEdge\src\adm2\query.i    �0  z + C:\Progress\OpenEdge\src\adm2\delrecst.i �0  `W * C:\Progress\OpenEdge\src\adm2\tblprep.i  �0  F� ) C:\Progress\OpenEdge\gui\fnarg   01   ( %C:\Progress\OpenEdge\src\adm2\custom\querycustom.i   \1  �   C:\Progress\OpenEdge\src\adm2\dataquery.i    �1  �Z ' %C:\Progress\OpenEdge\src\adm2\custom\dataquerycustom.i   �1  �< ! C:\Progress\OpenEdge\src\adm2\appserver.i    2  �� & %C:\Progress\OpenEdge\src\adm2\custom\appservercustom.i   P2  I� " C:\Progress\OpenEdge\src\adm2\smart.i    �2  Ds % C:\Progress\OpenEdge\gui\fn  �2  tw $ %C:\Progress\OpenEdge\src\adm2\custom\smartcustom.i   �2  Q. # C:\Progress\OpenEdge\gui\set 03  �>  C:\Progress\OpenEdge\src\adm2\dataprop.i X3  ��  %C:\Progress\OpenEdge\src\adm2\custom\datapropcustom.i    �3  ��  %C:\Progress\OpenEdge\src\adm2\custom\dataprtocustom.i    �3  �K  C:\Progress\OpenEdge\src\adm2\qryprop.i  4  -�  %C:\Progress\OpenEdge\src\adm2\custom\qrypropcustom.i H4  ��  %C:\Progress\OpenEdge\src\adm2\custom\qryprtocustom.i �4   	 C:\Progress\OpenEdge\src\adm2\dataqueryprop.i    �4  �d  %C:\Progress\OpenEdge\src\adm2\custom\dataquerypropcustom.i   5  ��  %C:\Progress\OpenEdge\src\adm2\custom\dataqueryprtocustom.i   L5  �l  C:\Progress\OpenEdge\src\adm2\appsprop.i �5  ɏ  %C:\Progress\OpenEdge\src\adm2\custom\appspropcustom.i    �5  V  %C:\Progress\OpenEdge\src\adm2\custom\appsprtocustom.i    6  i$  C:\Progress\OpenEdge\src\adm2\smrtprop.i P6  �j  C:\Progress\OpenEdge\gui\get �6  �  %C:\Progress\OpenEdge\src\adm2\custom\smrtpropcustom.i    �6  ��  %C:\Progress\OpenEdge\src\adm2\custom\smrtprtocustom.i    �6  ��  C:\Progress\OpenEdge\src\adm2\smrtprto.i 47  Su  C:\Progress\OpenEdge\src\adm2\globals.i  h7  M�  %C:\Progress\OpenEdge\src\adm2\custom\globalscustom.i �7  )a  %C:\Progress\OpenEdge\src\adm2\custom\smartdefscustom.i   �7  �  C:\Progress\OpenEdge\src\adm2\appsprto.i  8  ��  %C:\Progress\OpenEdge\src\adm2\custom\appserverdefscustom.i   T8  ��  C:\Progress\OpenEdge\src\adm2\dataqueryprto.i    �8  ª 
 %C:\Progress\OpenEdge\src\adm2\custom\dataquerydefscustom.i   �8  ��  C:\Progress\OpenEdge\src\adm2\qryprto.i   9  �  %C:\Progress\OpenEdge\src\adm2\custom\querydefscustom.i   T9  �`  C:\Progress\OpenEdge\src\adm2\dataprto.i �9  �  %C:\Progress\OpenEdge\src\adm2\custom\datadefscustom.i    �9  e�  !C:\Progress\OpenEdge\gui\adecomm\appserv.i   :  �    !C:\newsie\on_in_co\aplic\pruebas\dtcotcreditodet.w       �   �      �:  [  �     �:     �  %   �:  �        �:     �  .   �:  �   �     �:     �     �:  �   �     �:     c  #   ;  �   a     ;     ?  #   (;  �   =     8;       #   H;  �        X;     �  #   h;  �   �     x;     �  #   �;  �   �     �;     �  #   �;  �   �     �;     �  #   �;  �   �     �;     e  #   �;  �   X     �;     @  -   <  �   <     <       ,   (<  k   �     8<  �  �     H<     �  +   X<  �  �     h<     �  +   x<  �  �     �<     ~  +   �<  �  {     �<     a  +   �<  �  ^     �<     D  +   �<  �  A     �<     '  +   �<  �  $     =     
  +   =  �       (=     �  +   8=  �  �     H=     �  +   X=  �  �     h=     �  +   x=  �  �     �=     �  +   �=  �  �     �=     y  +   �=  �  v     �=     \  +   �=  �  Y     �=     ?  +   �=  �  <     >     "  +   >  �       (>       +   8>  �       H>     �  +   X>  �  �     h>     �  +   x>  �  �     �>     �  #   �>  �  �     �>     f  #   �>  k  A     �>       #   �>  j       �>     �  #   �>  i  �     ?     �  #   ?  _  �     (?     �  *   8?  ^  �     H?     �  *   X?  ]  �     h?     [  *   x?  \  Z     �?     4  *   �?  [  3     �?       *   �?  Z       �?     �  *   �?  Y  �     �?     �  *   �?  X  �     @     �  *   @  W  �     (@     q  *   8@  V  p     H@     J  *   X@  U  I     h@     #  *   x@  T  "     �@     �  *   �@  S  �     �@     �  *   �@  R  �     �@     �  *   �@  Q  �     �@     �  *   �@  P  �     A     `  *   A  O  _     (A     9  *   8A  N  8     HA       *   XA  @       hA     �  #   xA  	  �     �A     �  )   �A  �   �     �A     v  #   �A  �   u     �A     S  #   �A  �   R     �A     0  #   �A  �   /     B       #   B  �        (B     �  #   8B  �   �     HB     �  #   XB  �   W     hB     �  (   xB  g   �     �B  a   �      �B     �  '   �B  _   �      �B     f  #   �B  ]   d      �B     B  #   �B  I   .      �B  �   %  !   C     �  &   C  �   �  !   (C     �  #   8C  �   �  !   HC     �  #   XC  �   �  !   hC     _  #   xC  g   E  !   �C     &     �C  O     !   �C  �   �  "   �C     �  %   �C  �   f  "   �C       $   �C  �     "   �C     �  #   D  �   �  "   D     �  #   (D  �   �  "   8D     �  #   HD  �   �  "   XD     x  #   hD  �   d  "   xD     B  #   �D  }   6  "   �D       #   �D     �  "   �D     J  !   �D           �D     �     �D     P     �D  �   G     E  O   9     E     (     (E     �     8E  �   �     HE  �   �     XE  O   �     hE     y     xE     +     �E  y        �E  �   �
  	   �E  G   �
     �E     �
     �E     �
     �E  c   3
  	   �E  x   +
     �E  M   
     F     
     F     �	     (F  a   �	     8F  �  �	     HF     b	     XF  �  /	     hF  O   !	     xF     	     �F     �     �F  �   �     �F     �     �F          �F  x        �F     �     �F     }     �F     y     G     e     G     L     (G  Q   <     8G     �     HG     �     XG     �     hG     |     xG  ]   v  	   �G     l     �G     $  	   �G       
   �G       	   �G  Z   �     �G          �G     �     �G     �     H     �     H  c   �     (H     ^     8H          HH          XH     �      hH     �      xH     !       �H           
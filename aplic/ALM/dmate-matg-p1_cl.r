	��VԃBf$8   �                                              � 382400EFutf-8 MAIN D:\newsie\on_in_co\aplic\alm\dmate-matg-p1_cl.w,, PROCEDURE disable_UI,, PROCEDURE serverCommit,,INPUT-OUTPUT RowObjUpd TABLE,OUTPUT pcMessages CHARACTER,OUTPUT pcUndoIds CHARACTER PROCEDURE remoteCommit,,INPUT-OUTPUT pcContext CHARACTER,INPUT-OUTPUT RowObjUpd TABLE,OUTPUT pcMessages CHARACTER,OUTPUT pcUndoIds CHARACTER PROCEDURE pushTableAndValidate,,INPUT pcValType CHARACTER,INPUT-OUTPUT RowObjUpd TABLE PROCEDURE pushRowObjUpdTable,,INPUT RowObjUpd TABLE PROCEDURE initProps,, PROCEDURE start-super-proc,,INPUT pcProcName CHARACTER PROCEDURE adm-clone-props,, PROCEDURE viewObject,, PROCEDURE toggleData,,INPUT plEnabled LOGICAL PROCEDURE showMessageProcedure,,INPUT pcMessage CHARACTER,OUTPUT plAnswer LOGICAL PROCEDURE returnFocus,,INPUT hTarget HANDLE PROCEDURE repositionObject,,INPUT pdRow DECIMAL,INPUT pdCol DECIMAL PROCEDURE removeLink,,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE PROCEDURE removeAllLinks,, PROCEDURE modifyUserLinks,,INPUT pcMod CHARACTER,INPUT pcLinkName CHARACTER,INPUT phObject HANDLE PROCEDURE modifyListProperty,,INPUT phCaller HANDLE,INPUT pcMode CHARACTER,INPUT pcListName CHARACTER,INPUT pcListValue CHARACTER PROCEDURE hideObject,, PROCEDURE exitObject,, PROCEDURE editInstanceProperties,, PROCEDURE displayLinks,, PROCEDURE createControls,, PROCEDURE changeCursor,,INPUT pcCursor CHARACTER PROCEDURE applyEntry,,INPUT pcField CHARACTER PROCEDURE adjustTabOrder,,INPUT phObject HANDLE,INPUT phAnchor HANDLE,INPUT pcPosition CHARACTER PROCEDURE addMessage,,INPUT pcText CHARACTER,INPUT pcField CHARACTER,INPUT pcTable CHARACTER PROCEDURE addLink,,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE PROCEDURE unbindServer,,INPUT pcMode CHARACTER PROCEDURE runServerObject,,INPUT phAppService HANDLE PROCEDURE disconnectObject,, PROCEDURE destroyObject,, PROCEDURE bindServer,, PROCEDURE transferDBRow,,INPUT pcRowIdent CHARACTER,INPUT piRowNum INTEGER PROCEDURE startFilter,, PROCEDURE releaseDBRow,, PROCEDURE refetchDBRow,,INPUT phRowObjUpd HANDLE PROCEDURE filterContainerHandler,,INPUT phFilterContainer HANDLE PROCEDURE fetchDBRowForUpdate,, PROCEDURE confirmContinue,,INPUT-OUTPUT pioCancel LOGICAL PROCEDURE compareDBRow,, PROCEDURE bufferCopyDBToRO,,INPUT phRowObj HANDLE,INPUT phBuffer HANDLE,INPUT pcExcludes CHARACTER,INPUT pcAssigns CHARACTER PROCEDURE assignDBRow,,INPUT phRowObjUpd HANDLE PROCEDURE updateState,,INPUT pcState CHARACTER PROCEDURE updateQueryPosition,, PROCEDURE updateAddQueryWhere,,INPUT pcWhere CHARACTER,INPUT pcField CHARACTER PROCEDURE undoTransaction,, PROCEDURE transferToExcel,,INPUT pcFieldList CHARACTER,INPUT plIncludeObj LOGICAL,INPUT plUseExisting LOGICAL,INPUT piMaxRecords INTEGER PROCEDURE synchronizeProperties,,INPUT pcPropertiesForServer CHARACTER,OUTPUT pcPropertiesForClient CHARACTER PROCEDURE submitValidation,,INPUT pcValueList CHARACTER,INPUT pcUpdColumns CHARACTER PROCEDURE submitForeignKey,,INPUT pcRowIdent CHARACTER,INPUT-OUTPUT pcValueList CHARACTER,INPUT-OUTPUT pcUpdColumns CHARACTER PROCEDURE submitCommit,,INPUT pcRowIdent CHARACTER,INPUT plReopen LOGICAL PROCEDURE startServerObject,, PROCEDURE setPropertyList,,INPUT pcProperties CHARACTER PROCEDURE serverFetchRowObjUpdTable,,OUTPUT phRowObjUpd TABLE-HANDLE PROCEDURE serverSendRows,,INPUT piStartRow INTEGER,INPUT pcRowIdent CHARACTER,INPUT plNext LOGICAL,INPUT piRowsToReturn INTEGER,OUTPUT piRowsReturned INTEGER,OUTPUT phRowObject TABLE-HANDLE PROCEDURE saveContextAndDestroy,,OUTPUT pcContext CHARACTER PROCEDURE rowObjectState,,INPUT pcState CHARACTER PROCEDURE retrieveFilter,, PROCEDURE restartServerObject,, PROCEDURE remoteSendRows,,INPUT-OUTPUT piocContext CHARACTER,INPUT piStartRow INTEGER,INPUT pcRowIdent CHARACTER,INPUT plNext LOGICAL,INPUT piRowsToReturn INTEGER,OUTPUT pioRowsReturned INTEGER,OUTPUT phRowObject TABLE-HANDLE,OUTPUT pocMessages CHARACTER PROCEDURE refreshRow,, PROCEDURE printToCrystal,,INPUT pcFieldList CHARACTER,INPUT plIncludeObj LOGICAL,INPUT piMaxRecords INTEGER PROCEDURE isUpdatePending,,INPUT-OUTPUT plUpdate LOGICAL PROCEDURE initializeServerObject,, PROCEDURE initializeObject,, PROCEDURE home,, PROCEDURE genContextList,,OUTPUT pcContext CHARACTER PROCEDURE fetchPrev,, PROCEDURE fetchNext,, PROCEDURE fetchLast,, PROCEDURE fetchFirst,, PROCEDURE fetchBatch,,INPUT plForwards LOGICAL PROCEDURE endClientDataRequest,, PROCEDURE destroyServerObject,, PROCEDURE describeSchema,,INPUT pcSdoName CHARACTER,OUTPUT hTtSchema TABLE-HANDLE PROCEDURE dataAvailable,,INPUT pcRelative CHARACTER PROCEDURE copyColumns,,INPUT pcViewColList CHARACTER,INPUT phDataQuery HANDLE PROCEDURE commitTransaction,, PROCEDURE clientSendRows,,INPUT piStartRow INTEGER,INPUT pcRowIdent CHARACTER,INPUT plNext LOGICAL,INPUT piRowsToReturn INTEGER,OUTPUT piRowsReturned INTEGER PROCEDURE batchServices,,INPUT pcServices CHARACTER,OUTPUT pcValues CHARACTER FUNCTION deleteRecordStatic,logical,INPUT piTableIndex INTEGER FUNCTION getRowObjUpdStatic,widget-handle, FUNCTION Signature,CHARACTER,INPUT pcName CHARACTER FUNCTION showmessage,LOGICAL,INPUT pcMessage CHARACTER FUNCTION setUserProperty,LOGICAL,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER FUNCTION setUIBMode,LOGICAL,INPUT pcMode CHARACTER FUNCTION setTranslatableProperties,LOGICAL,INPUT pcPropList CHARACTER FUNCTION setSupportedLinks,LOGICAL,INPUT pcLinkList CHARACTER FUNCTION setRunAttribute,LOGICAL,INPUT cRunAttribute CHARACTER FUNCTION setPhysicalVersion,LOGICAL,INPUT cVersion CHARACTER FUNCTION setPhysicalObjectName,LOGICAL,INPUT cTemp CHARACTER FUNCTION setPassThroughLinks,LOGICAL,INPUT pcLinks CHARACTER FUNCTION setParentDataKey,LOGICAL,INPUT cParentDataKey CHARACTER FUNCTION setObjectVersion,LOGICAL,INPUT cObjectVersion CHARACTER FUNCTION setObjectParent,LOGICAL,INPUT phParent HANDLE FUNCTION setObjectName,LOGICAL,INPUT pcName CHARACTER FUNCTION setLogicalVersion,LOGICAL,INPUT cVersion CHARACTER FUNCTION setLogicalObjectName,LOGICAL,INPUT c CHARACTER FUNCTION setInstanceProperties,LOGICAL,INPUT pcPropList CHARACTER FUNCTION setDynamicObject,LOGICAL,INPUT lTemp LOGICAL FUNCTION setDesignDataObject,LOGICAL,INPUT pcDataObject CHARACTER FUNCTION setDBAware,LOGICAL,INPUT lAware LOGICAL FUNCTION setDataTargetEvents,LOGICAL,INPUT pcEvents CHARACTER FUNCTION setDataTarget,LOGICAL,INPUT pcTarget CHARACTER FUNCTION setDataSourceNames,LOGICAL,INPUT pcSourceNames CHARACTER FUNCTION setDataSourceEvents,LOGICAL,INPUT pcEventsList CHARACTER FUNCTION setDataSource,LOGICAL,INPUT phObject HANDLE FUNCTION setDataLinksEnabled,LOGICAL,INPUT lDataLinksEnabled LOGICAL FUNCTION setContainerSourceEvents,LOGICAL,INPUT pcEvents CHARACTER FUNCTION setContainerSource,LOGICAL,INPUT phObject HANDLE FUNCTION setContainerHidden,LOGICAL,INPUT plHidden LOGICAL FUNCTION setChildDataKey,LOGICAL,INPUT cChildDataKey CHARACTER FUNCTION reviewMessages,CHARACTER, FUNCTION propertyType,CHARACTER,INPUT pcPropName CHARACTER FUNCTION messageNumber,CHARACTER,INPUT piMessage INTEGER FUNCTION mappedEntry,CHARACTER,INPUT pcEntry CHARACTER,INPUT pcList CHARACTER,INPUT plFirst LOGICAL,INPUT pcDelimiter CHARACTER FUNCTION linkProperty,CHARACTER,INPUT pcLink CHARACTER,INPUT pcPropName CHARACTER FUNCTION linkHandles,CHARACTER,INPUT pcLink CHARACTER FUNCTION instancePropertyList,CHARACTER,INPUT pcPropList CHARACTER FUNCTION getUserProperty,CHARACTER,INPUT pcPropName CHARACTER FUNCTION getUIBMode,CHARACTER, FUNCTION getTranslatableProperties,CHARACTER, FUNCTION getSupportedLinks,CHARACTER, FUNCTION getRunAttribute,CHARACTER, FUNCTION getQueryObject,LOGICAL, FUNCTION getPropertyDialog,CHARACTER, FUNCTION getPhysicalVersion,CHARACTER, FUNCTION getPhysicalObjectName,CHARACTER, FUNCTION getPassThroughLinks,CHARACTER, FUNCTION getParentDataKey,CHARACTER, FUNCTION getObjectVersionNumber,CHARACTER, FUNCTION getObjectVersion,CHARACTER, FUNCTION getObjectParent,HANDLE, FUNCTION getObjectPage,INTEGER, FUNCTION getObjectName,CHARACTER, FUNCTION getObjectInitialized,LOGICAL, FUNCTION getObjectHidden,LOGICAL, FUNCTION getLogicalVersion,CHARACTER, FUNCTION getLogicalObjectName,CHARACTER, FUNCTION getInstanceProperties,CHARACTER, FUNCTION getDynamicObject,LOGICAL, FUNCTION getDesignDataObject,CHARACTER, FUNCTION getDBAware,LOGICAL, FUNCTION getDataTargetEvents,CHARACTER, FUNCTION getDataTarget,CHARACTER, FUNCTION getDataSourceNames,CHARACTER, FUNCTION getDataSourceEvents,CHARACTER, FUNCTION getDataSource,HANDLE, FUNCTION getDataLinksEnabled,LOGICAL, FUNCTION getContainerType,CHARACTER, FUNCTION getContainerSourceEvents,CHARACTER, FUNCTION getContainerSource,HANDLE, FUNCTION getContainerHidden,LOGICAL, FUNCTION getContainerHandle,HANDLE, FUNCTION getChildDataKey,CHARACTER, FUNCTION fetchMessages,CHARACTER, FUNCTION assignLinkProperty,LOGICAL,INPUT pcLink CHARACTER,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER FUNCTION anyMessage,LOGICAL, FUNCTION setServerOperatingMode,LOGICAL,INPUT pcServerOperatingMode CHARACTER FUNCTION setServerFileName,LOGICAL,INPUT pcFileName CHARACTER FUNCTION setASUsePrompt,LOGICAL,INPUT plFlag LOGICAL FUNCTION setASInitializeOnRun,LOGICAL,INPUT plInitialize LOGICAL FUNCTION setASInfo,LOGICAL,INPUT pcInfo CHARACTER FUNCTION setASHandle,LOGICAL,INPUT phASHandle HANDLE FUNCTION setASDivision,LOGICAL,INPUT pcDivision CHARACTER FUNCTION setAppService,LOGICAL,INPUT pcAppService CHARACTER FUNCTION runServerProcedure,HANDLE,INPUT pcServerFileName CHARACTER,INPUT phAppService HANDLE FUNCTION getServerOperatingMode,CHARACTER, FUNCTION getServerFileName,CHARACTER, FUNCTION getASUsePrompt,LOGICAL, FUNCTION getASInitializeOnRun,LOGICAL, FUNCTION getASInfo,CHARACTER, FUNCTION getASHasStarted,LOGICAL, FUNCTION getASHandle,HANDLE, FUNCTION getAsDivision,CHARACTER, FUNCTION getASBound,LOGICAL, FUNCTION getAppService,CHARACTER, FUNCTION whereClauseBuffer,CHARACTER,INPUT pcWhere CHARACTER FUNCTION setQueryWhere,LOGICAL,INPUT pcWhere CHARACTER FUNCTION setQueryString,LOGICAL,INPUT pcQueryString CHARACTER FUNCTION setQuerySort,LOGICAL,INPUT pcSort CHARACTER FUNCTION setQueryPosition,LOGICAL,INPUT pcPosition CHARACTER FUNCTION rowidWhereCols,CHARACTER,INPUT pcColumns CHARACTER,INPUT pcValues CHARACTER,INPUT pcOperators CHARACTER FUNCTION rowidWhere,CHARACTER,INPUT pcWhere CHARACTER FUNCTION resolveBuffer,CHARACTER,INPUT pcBuffer CHARACTER FUNCTION removeQuerySelection,LOGICAL,INPUT pcColumns CHARACTER,INPUT pcOperators CHARACTER FUNCTION removeForeignKey,LOGICAL, FUNCTION refreshRowident,CHARACTER,INPUT pcRowident CHARACTER FUNCTION newWhereClause,CHARACTER,INPUT pcBuffer CHARACTER,INPUT pcExpression CHARACTER,INPUT pcWhere CHARACTER,INPUT pcAndOr CHARACTER FUNCTION newQueryWhere,CHARACTER,INPUT pcWhere CHARACTER FUNCTION newQueryValidate,CHARACTER,INPUT pcQueryString CHARACTER,INPUT pcExpression CHARACTER,INPUT pcBuffer CHARACTER,INPUT pcAndOr CHARACTER FUNCTION newQueryString,CHARACTER,INPUT pcColumns CHARACTER,INPUT pcValues CHARACTER,INPUT pcOperators CHARACTER,INPUT pcQueryString CHARACTER,INPUT pcAndOr CHARACTER FUNCTION insertExpression,CHARACTER,INPUT pcWhere CHARACTER,INPUT pcExpression CHARACTER,INPUT pcAndOr CHARACTER FUNCTION indexInformation,CHARACTER,INPUT pcQuery CHARACTER,INPUT plUseTableSep LOGICAL,INPUT pcIndexInfo CHARACTER FUNCTION getTargetProcedure,HANDLE, FUNCTION getQueryWhere,CHARACTER, FUNCTION getQueryString,CHARACTER, FUNCTION getQuerySort,CHARACTER, FUNCTION getQueryPosition,CHARACTER, FUNCTION getForeignValues,CHARACTER, FUNCTION getDataColumns,CHARACTER, FUNCTION excludeColumns,CHARACTER,INPUT iTable INTEGER FUNCTION dbColumnHandle,HANDLE,INPUT pcColumn CHARACTER FUNCTION dbColumnDataName,CHARACTER,INPUT pcDbColumn CHARACTER FUNCTION columnValMsg,CHARACTER,INPUT pcColumn CHARACTER FUNCTION columnTable,CHARACTER,INPUT pcColumn CHARACTER FUNCTION columnQuerySelection,CHARACTER,INPUT pcColumn CHARACTER FUNCTION columnDbColumn,CHARACTER,INPUT pcColumn CHARACTER FUNCTION columnDataType,CHARACTER,INPUT pcColumn CHARACTER FUNCTION bufferWhereClause,CHARACTER,INPUT pcBuffer CHARACTER,INPUT pcWhere CHARACTER FUNCTION bufferCompareDBToRO,LOGICAL,INPUT phRowObjUpd HANDLE,INPUT phBuffer HANDLE,INPUT pcExcludes CHARACTER,INPUT pcAssigns CHARACTER FUNCTION assignQuerySelection,LOGICAL,INPUT pcColumns CHARACTER,INPUT pcValues CHARACTER,INPUT pcOperators CHARACTER FUNCTION addQueryWhere,LOGICAL,INPUT pcWhere CHARACTER,INPUT pcBuffer CHARACTER,INPUT pcAndOr CHARACTER FUNCTION getObjectType,character, FUNCTION updateRow,LOGICAL,INPUT pcKeyValues CHARACTER,INPUT pcValueList CHARACTER FUNCTION submitRow,LOGICAL,INPUT pcRowIdent CHARACTER,INPUT pcValueList CHARACTER FUNCTION rowValues,CHARACTER,INPUT pcColumns CHARACTER,INPUT pcFormat CHARACTER,INPUT pcDelimiter CHARACTER FUNCTION rowAvailable,LOGICAL,INPUT pcDirection CHARACTER FUNCTION prepareQuery,LOGICAL,INPUT pcQuery CHARACTER FUNCTION openQuery,LOGICAL, FUNCTION openDataQuery,LOGICAL,INPUT pcPosition CHARACTER FUNCTION hasForeignKeyChanged,LOGICAL, FUNCTION getLastCommitErrorType,CHARACTER, FUNCTION firstRowIds,CHARACTER,INPUT pcQueryString CHARACTER FUNCTION findRowWhere,LOGICAL,INPUT pcColumns CHARACTER,INPUT pcValues CHARACTER,INPUT pcOperators CHARACTER FUNCTION findRow,LOGICAL,INPUT pcKeyValues CHARACTER FUNCTION fetchRowIdent,CHARACTER,INPUT pcRowIdent CHARACTER,INPUT pcViewColList CHARACTER FUNCTION fetchRow,CHARACTER,INPUT piRow INTEGER,INPUT pcViewColList CHARACTER FUNCTION deleteRow,LOGICAL,INPUT pcRowIdent CHARACTER FUNCTION createRow,LOGICAL,INPUT pcValueList CHARACTER FUNCTION copyRow,CHARACTER,INPUT pcViewColList CHARACTER FUNCTION colValues,CHARACTER,INPUT pcViewColList CHARACTER FUNCTION columnProps,CHARACTER,INPUT pcColList CHARACTER,INPUT pcPropList CHARACTER FUNCTION closeQuery,LOGICAL, FUNCTION canNavigate,LOGICAL, FUNCTION cancelRow,CHARACTER, FUNCTION addRow,CHARACTER,INPUT pcViewColList CHARACTER TEMP-TABLE RowObjUpd 0,RowNum:RowNum 0 NO,AlmDes character 0 0,CodAlm character 1 0,CodCia integer 2 0,codmat character 3 0,CodUbi character 4 0,StkAct decimal 5 0,CodCia-2 integer 6 0,codfam character 7 0,CodMar character 8 0,codmat-2 character 9 0,CodPr1 character 10 0,DesMar character 11 0,DesMat character 12 0,TpoArt character 13 0,UndStk character 14 0,subfam character 15 0,FchIng date 16 0,Pesmat decimal 17 0,RowNum integer 18 0,RowIdent character 19 0,RowMod character 20 0,RowIdentIdx character 21 0,RowUserProp character 22 0,ChangedFields character 23 0       �(                           � �(  �              `�              �=     +   d� �  W   � `  X   d� �  Y   �   [   ��   \   � <  ]   H�    ^   h� 0  `   ? �� |  iSO8859-1                                                                           X(     �                                      �                   ��                �(  �       �   T�              ��  �   �(      �(                                                         PROGRESS                         �           
    
                    �              �                                                                                                     
                                                                                                            ��         �             �                                                                                             ��         @        �  
    
                  �  p             ,                                                                                                    
  �        h  
    
                  T               �                                                                                                    
  �  )        
    
                     �             �                                                                                          )          
  D  6      �  
    
                  �  t             0                                                                                          6          
  �  I      l  
    
                  X     	           �                                                                                          I          
  �  [        
    
                    �  
           �                                                                                          [          
  H  p      �  
    
                  �  x             4                                                                                          p          
  �  �      p  
    
                  \  $             �                                                                                          �          
  �  �                                 �             �                                                                                          �            L	  �      �                        �  |	             8	                                                                                          �            �	  �      t	  
    
                  `	  (
             �	                                                                                          �          
  �
  �       
  
    
                  
  �
             �
                                                                                          �          
  P  �      �
  
    
                  �
  �             <                                                                                          �          
  �  �      x                        d  ,             �                                                                                          �            �  �      $                          �             �                                                                                          �            T  �      �                        �  �             @                                                                                          �                      |                        h                �                                                                                                      h         �       �  X  �  .   �  �  @I               �             �          �      �              �       �  X  l'  /   �'  �  ��      �'         �         �    l          `      �                 P�                                               T�          X  �  L l8                �         
             
             
                                         
                                                                                                               
             
                                          L   \   l   |   �   �   �   �   �   �   �   �       ,  <  L  \      L   \   l   |   �   �   �   �   �   �   �   �      ,  <  L  \                                                                                                                                     	                  
                                                                                                                                                                                                                                                                                             �  �                                       (  8  0          <             P  X  \  d  `          h             |  �  �  �  �                         �  �  �  �  �          �             �  �    ,            0             @  L  P  X  T          \             p  x  �  �  �          �             �  �  �  �  �                         �  �  �    �                               D  0          H             \  d  l  �  x                         �  �  �  �  �          �             �  �  �  �  �                         �      ,            0             @  H  P  h  \                         l  t  �  �  �          �             �  �  �  �  �                         �  �  �  �                             �                                       (  0  8                             <  H  P  \                             `  l  t  �                                                                          AlmDes  X(70)   Almacen Despacho        CodAlm  x(5)    Almac�n Almac�n     C�digo de almac�n   CodCia  999 Cia Cia 0   C�digo de compa�ia  codmat  X(6)    Codigo Articulo Codigo!Articulo     CodUbi  x(7)    Ubicaci�n   Ubicaci�n       C�digo de ubicaci�n StkAct  (ZZZ,ZZZ,ZZ9.99)    Stock actual    Stock actual    0   Stock actual    CodCia-2    999 Cia Cia 0   C�digo de compa�ia  codfam  X(3)    Familia C�digo!familia      Codigo de familia   CodMar  X(4)    Marca   Marca       codmat-2    X(6)    Codigo Articulo Codigo Articulo     CodPr1  x(11)   Proveedor principal Proveedor!principal     Proveedor principal DesMar  X(30)   Des.Marca   Descripcion!Marca       DesMat  X(60)   Descripci�n Descripci�n     Descripci�n del material    TpoArt  X(1)    Estado  Estado  A   UndStk  X(8)    Unidad de stock Unidad!stock        Unidad de stock subfam  X(3)    Sub-Familia Sub!Familia     FchIng  99/99/9999  Fecha Ing.  Fecha Ing.  ?   Fecha de Ingreso    Pesmat  ->>,>>9.9999    Peso!Kg Peso!Kg 0   RowNum  ->,>>>,>>9  RowNum  0   RowIdent    x(8)    RowIdent        RowMod  x(8)    RowMod      RowIdentIdx x(8)    RowIdentIdx     RowUserProp x(8)    RowUserProp     �   �  ���������             A  �            	                                 �     i     i     i     	 	 	    �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �    	         ,                                                                                                                                     	                  
                                                                                                                                                                                                                                                                                                               �"  �"  �"  �"                             �"  �"  �"  �"  �"          �"             #  #  #   #  #          $#             8#  @#  H#  h#  X#                         l#  t#  |#  �#  �#          �#             �#  �#  �#  �#  �#          �#             �#  $  $  $  $          $             ,$  4$  <$  T$  D$          X$             l$  t$  |$  �$  �$                         �$  �$  �$  �$  �$                         �$  �$  �$   %  �$          %             %   %  (%  H%  4%                         L%  T%  \%  t%  h%          x%             �%  �%  �%  �%  �%                         �%  �%  �%  �%  �%          �%             �%  &  &  $&  &                         (&  0&  <&  T&  H&          X&             l&  t&  �&  �&  �&                         �&  �&  �&  �&                             �&  �&  �&  �&                              �&  �&  �&  �&                             �&  '  '  '                             '  ('  0'  <'                              @'  P'  X'  h'                                                                          AlmDes  X(70)   Almacen Despacho        CodAlm  x(5)    Almac�n Almac�n     C�digo de almac�n   CodCia  999 Cia Cia 0   C�digo de compa�ia  codmat  X(6)    Codigo Articulo Codigo!Articulo     CodUbi  x(7)    Ubicaci�n   Ubicaci�n       C�digo de ubicaci�n StkAct  (ZZZ,ZZZ,ZZ9.99)    Stock actual    Stock actual    0   Stock actual    CodCia-2    999 Cia Cia 0   C�digo de compa�ia  codfam  X(3)    Familia C�digo!familia      Codigo de familia   CodMar  X(4)    Marca   Marca       codmat-2    X(6)    Codigo Articulo Codigo Articulo     CodPr1  x(11)   Proveedor principal Proveedor!principal     Proveedor principal DesMar  X(30)   Des.Marca   Descripcion!Marca       DesMat  X(60)   Descripci�n Descripci�n     Descripci�n del material    TpoArt  X(1)    Estado  Estado  A   UndStk  X(8)    Unidad de stock Unidad!stock        Unidad de stock subfam  X(3)    Sub-Familia Sub!Familia     FchIng  99/99/9999  Fecha Ing.  Fecha Ing.  ?   Fecha de Ingreso    Pesmat  ->>,>>9.9999    Peso!Kg Peso!Kg 0   RowNum  ->,>>>,>>9  RowNum  0   RowIdent    x(8)    RowIdent        RowMod  x(8)    RowMod      RowIdentIdx x(8)    RowIdentIdx     RowUserProp x(8)    RowUserProp     ChangedFields   x(8)    ChangedFields       �   �  ���������             A  �            	                                 �     i     i     i     	 	 	    �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �    	         ,  8    ��                            ����                            q    p�                    g�    undefined                                                               �       t�  �   l   ��  ��                    �����               x�r                    O   ����    e�          O   ����    R�          O   ����    ��      t       �   �              4   ����     /                                     3   ����       $      H  ���                       8      
                       � ߱        �  �   "   D       �     >          ��    �   �  4      d       4   ����d                 D                      ��                  �   �                   LZs                       �   �  �  	  �   x                                        3   ����|       O   �   ��  ��  �   batchServices                               4        ��                  \  _  L              tCs                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �             d               ��                  �           ��                            ����                            clientSendRows                              �  p      ��                  a  g  �              xUs                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �             �               ��                �               ��   <                            ��   d             0               ��                  X           ��                            ����                            commitTransaction                               X  @      ��                  i  j  p              h�o                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            copyColumns                             X  @      ��                  l  o  p              �o                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �             �               �� 
                 �  
         ��                            ����                            dataAvailable                               �  �      ��                  q  s  �              ��o                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �           ��                            ����                            describeSchema                              �	  �	      ��                  u  x  �	              d�o                    O   ����    e�          O   ����    R�          O   ����    ��            ��   <
             
               �� 
          �       0
  
         ��                            ����                            destroyServerObject                             0        ��                  z  {  H              0|o                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            endClientDataRequest                                <  $      ��                  }  ~  T              �|o                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            fetchBatch                              <  $      ��                  �  �  T              �}o                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  l           ��                            ����                            fetchFirst                              d  L      ��                  �  �  |              ��o                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            fetchLast                               d  L      ��                  �  �  |              0�o                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            fetchNext                               d  L      ��                  �  �  |              ܵo                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            fetchPrev                               d  L      ��                  �  �  |              ��o                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            genContextList                              h  P      ��                  �  �  �              (�o                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �           ��                            ����                            home                                �  t      ��                  �  �  �              ��o                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeObject                                �  |      ��                  �  �  �              ��o                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeServerObject                              �  �      ��                  �  �  �              �o                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            isUpdatePending                             �  �      ��                  �  �  �              ̙o                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �           ��                            ����                            printToCrystal                              �  �      ��                  �  �  �              �!r                    O   ����    e�          O   ����    R�          O   ����    ��            ��   4                             ��   \             (               ��                  P           ��                            ����                            refreshRow                              H  0      ��                  �  �  `              �_r                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            remoteSendRows                              L  4      ��                  �  �  d              .r                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �             |               ��   �             �               ��                 �               ��   (             �               ��   P                            ��   x             D               �� 
  �      �       l  
             ��                  �           ��                            ����                            restartServerObject                             �  |      ��                  �  �  �              �r                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            retrieveFilter                              �  �      ��                  �  �  �              �&r                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            rowObjectState                              �  �      ��                  �  �  �              `or                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �           ��                            ����                            saveContextAndDestroy                               �  �      ��                  �  �  �              �kr                    O   ����    e�          O   ����    R�          O   ����    ��            ��                               ��                            ����                            serverSendRows                              �   �       ��                  �  �  !              �r                    O   ����    e�          O   ����    R�          O   ����    ��            ��   `!             ,!               ��   �!             T!               ��   �!             |!               ��   �!             �!               ��    "             �!               �� 
          �       �!  
         ��                            ����                            serverFetchRowObjUpdTable                               �"  �"      ��                  �  �  #              �9r                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
          �       ,#  
         ��                            ����                            setPropertyList                             ($  $      ��                  �  �  @$              h�r                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  X$           ��                            ����                            serverSendRows                              T%  <%      ��                  �  �  l%              ��r                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �%             �%               ��   �%             �%               ��   &             �%               ��   0&             �%               ��   X&             $&               �� 
          �       L&  
         ��                            ����                            startServerObject                               L'  4'      ��                  �  �  d'              ��r                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            submitCommit                                P(  8(      ��                  �  �  h(              @�r                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �(             �(               ��                  �(           ��                            ����                            submitForeignKey                                �)  �)      ��                  �  �  �)              �r                    O   ����    e�          O   ����    R�          O   ����    ��            ��   *             �)               ��   4*              *               ��                  (*           ��                            ����                            submitValidation                                (+  +      ��                  �  �  @+               �r                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �+             X+               ��                  �+           ��                            ����                            synchronizeProperties                               �,  l,      ��                      �,              ��r                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �,             �,               ��                  �,           ��                            ����                            transferToExcel                             �-  �-      ��                      �-              Ļr                    O   ����    e�          O   ����    R�          O   ����    ��            ��   <.             .               ��   d.             0.               ��   �.             X.               ��                  �.           ��                            ����                            undoTransaction                             |/  d/      ��                      �/              ��r                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            updateAddQueryWhere                             �0  l0      ��                      �0               �r                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �0             �0               ��                  �0           ��                            ����                            updateQueryPosition                             �1  �1      ��                      �1              T�r                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            updateState                             �2  �2      ��                     "  �2              �=                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  3           ��                            ����                            addRow          t3      �3     n       CHARACTER,INPUT pcViewColList CHARACTER cancelRow   |3      �3      �3   	 u       CHARACTER,  canNavigate �3      �3      (4           LOGICAL,    closeQuery  4      44      `4   
 �       LOGICAL,    columnProps @4      l4      �4    �       CHARACTER,INPUT pcColList CHARACTER,INPUT pcPropList CHARACTER  colValues   x4      �4      5   	 �       CHARACTER,INPUT pcViewColList CHARACTER copyRow �4      ,5      T5    �       CHARACTER,INPUT pcViewColList CHARACTER createRow   45      |5      �5   	 �       LOGICAL,INPUT pcValueList CHARACTER deleteRow   �5      �5      �5   	 �       LOGICAL,INPUT pcRowIdent CHARACTER  fetchRow    �5      6      H6  	  �       CHARACTER,INPUT piRow INTEGER,INPUT pcViewColList CHARACTER fetchRowIdent   (6      �6      �6  
  �       CHARACTER,INPUT pcRowIdent CHARACTER,INPUT pcViewColList CHARACTER  findRow �6      �6       7    �       LOGICAL,INPUT pcKeyValues CHARACTER findRowWhere     7      D7      t7    �       LOGICAL,INPUT pcColumns CHARACTER,INPUT pcValues CHARACTER,INPUT pcOperators CHARACTER  firstRowIds T7      �7      �7    �       CHARACTER,INPUT pcQueryString CHARACTER getLastCommitErrorType  �7       8      X8           CHARACTER,  hasForeignKeyChanged    88      d8      �8          LOGICAL,    openDataQuery   |8      �8      �8    ,      LOGICAL,INPUT pcPosition CHARACTER  openQuery   �8      �8      (9   	 :      LOGICAL,    prepareQuery    9      49      d9    D      LOGICAL,INPUT pcQuery CHARACTER rowAvailable    D9      �9      �9    Q      LOGICAL,INPUT pcDirection CHARACTER rowValues   �9      �9      :   	 ^      CHARACTER,INPUT pcColumns CHARACTER,INPUT pcFormat CHARACTER,INPUT pcDelimiter CHARACTER    submitRow   �9      `:      �:   	 h      LOGICAL,INPUT pcRowIdent CHARACTER,INPUT pcValueList CHARACTER  updateRow   l:      �:      �:   	 r      LOGICAL,INPUT pcKeyValues CHARACTER,INPUT pcValueList CHARACTER getObjectType   �:      8;      h;    |      CHARACTER,  assignDBRow                             <  �;      ��                    
  <              \�=                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
                 4<  
         ��                            ����                            bufferCopyDBToRO                                4=  =      ��                      L=              ܒ=                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  �=             d=  
             �� 
  �=             �=  
             ��   �=             �=               ��                  �=           ��                            ����                            compareDBRow                                �>  �>      ��                      �>              ��=                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            confirmContinue                             �?  �?      ��                      �?              (�=                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  @           ��                            ����                            dataAvailable                               A  �@      ��                       A              ��=                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  8A           ��                            ����                            fetchDBRowForUpdate                             8B   B      ��                      PB               �=                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            fetchFirst                              8C   C      ��                  !  "  PC              ��=                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            fetchLast                               8D   D      ��                  $  %  PD              |#>                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            fetchNext                               8E   E      ��                  '  (  PE              �$>                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            fetchPrev                               8F   F      ��                  *  +  PF              <d>                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            filterContainerHandler                              DG  ,G      ��                  -  /  \G              �w>                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
                 tG  
         ��                            ����                            initializeObject                                tH  \H      ��                  1  2  �H              �G>                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            refetchDBRow                                xI  `I      ��                  4  6  �I              �>                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
                 �I  
         ��                            ����                            releaseDBRow                                �J  �J      ��                  8  9  �J              4'>                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            startFilter                             �K  �K      ��                  ;  <  �K              �'>                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            transferDBRow                               �L  �L      ��                  >  A  �L              |(>                    O   ����    e�          O   ����    R�          O   ����    ��            ��   M             �L               ��                   M           ��                            ����                            addQueryWhere   H;      hM      �M    �      LOGICAL,INPUT pcWhere CHARACTER,INPUT pcBuffer CHARACTER,INPUT pcAndOr CHARACTER    assignQuerySelection    xM      �M      $N    �      LOGICAL,INPUT pcColumns CHARACTER,INPUT pcValues CHARACTER,INPUT pcOperators CHARACTER  bufferCompareDBToRO N      |N      �N    �      LOGICAL,INPUT phRowObjUpd HANDLE,INPUT phBuffer HANDLE,INPUT pcExcludes CHARACTER,INPUT pcAssigns CHARACTER bufferWhereClause   �N      O      PO    �      CHARACTER,INPUT pcBuffer CHARACTER,INPUT pcWhere CHARACTER  columnDataType  0O      �O      �O    �      CHARACTER,INPUT pcColumn CHARACTER  columnDbColumn  �O      �O      P    �      CHARACTER,INPUT pcColumn CHARACTER  columnQuerySelection    �O      4P      lP          CHARACTER,INPUT pcColumn CHARACTER  columnTable LP      �P      �P          CHARACTER,INPUT pcColumn CHARACTER  columnValMsg    �P      �P      Q     %      CHARACTER,INPUT pcColumn CHARACTER  dbColumnDataName    �P      4Q      hQ  !  2      CHARACTER,INPUT pcDbColumn CHARACTER    dbColumnHandle  HQ      �Q      �Q  "  C      HANDLE,INPUT pcColumn CHARACTER excludeColumns  �Q      �Q      R  #  R      CHARACTER,INPUT iTable INTEGER  getDataColumns  �Q      0R      `R  $  a      CHARACTER,  getForeignValues    @R      lR      �R  %  p      CHARACTER,  getQueryPosition    �R      �R      �R  &  �      CHARACTER,  getQuerySort    �R      �R      S  '  �      CHARACTER,  getQueryString  �R      (S      XS  (  �      CHARACTER,  getQueryWhere   8S      dS      �S  )  �      CHARACTER,  getTargetProcedure  tS      �S      �S  *  �      HANDLE, indexInformation    �S      �S      T  +  �      CHARACTER,INPUT pcQuery CHARACTER,INPUT plUseTableSep LOGICAL,INPUT pcIndexInfo CHARACTER   insertExpression    �S      lT      �T  ,  �      CHARACTER,INPUT pcWhere CHARACTER,INPUT pcExpression CHARACTER,INPUT pcAndOr CHARACTER  newQueryString  �T      �T      (U  -  �      CHARACTER,INPUT pcColumns CHARACTER,INPUT pcValues CHARACTER,INPUT pcOperators CHARACTER,INPUT pcQueryString CHARACTER,INPUT pcAndOr CHARACTER  newQueryValidate    U      �U      �U  .         CHARACTER,INPUT pcQueryString CHARACTER,INPUT pcExpression CHARACTER,INPUT pcBuffer CHARACTER,INPUT pcAndOr CHARACTER   newQueryWhere   �U      dV      �V  /        CHARACTER,INPUT pcWhere CHARACTER   newWhereClause  tV      �V      �V  0        CHARACTER,INPUT pcBuffer CHARACTER,INPUT pcExpression CHARACTER,INPUT pcWhere CHARACTER,INPUT pcAndOr CHARACTER refreshRowident �V      XW      �W  1  .      CHARACTER,INPUT pcRowident CHARACTER    removeForeignKey    hW      �W      �W  2  >      LOGICAL,    removeQuerySelection    �W      �W      (X  3  O      LOGICAL,INPUT pcColumns CHARACTER,INPUT pcOperators CHARACTER   resolveBuffer   X      hX      �X  4  d      CHARACTER,INPUT pcBuffer CHARACTER  rowidWhere  xX      �X      �X  5 
 r      CHARACTER,INPUT pcWhere CHARACTER   rowidWhereCols  �X      Y      <Y  6  }      CHARACTER,INPUT pcColumns CHARACTER,INPUT pcValues CHARACTER,INPUT pcOperators CHARACTER    setQueryPosition    Y      �Y      �Y  7  �      LOGICAL,INPUT pcPosition CHARACTER  setQuerySort    �Y      �Y       Z  8  �      LOGICAL,INPUT pcSort CHARACTER  setQueryString   Z      @Z      pZ  9  �      LOGICAL,INPUT pcQueryString CHARACTER   setQueryWhere   PZ      �Z      �Z  :  �      LOGICAL,INPUT pcWhere CHARACTER whereClauseBuffer   �Z      �Z      [  ;  �      CHARACTER,INPUT pcWhere CHARACTER   bindServer                              �[  �[      ��                  �  �  �[              ��>                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            destroyObject                               �\  �\      ��                  �  �  �\              |�>                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            destroyServerObject                             �]  �]      ��                  �  �  �]               �>                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            disconnectObject                                �^  �^      ��                  �  �  �^              <^?                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeServerObject                              �_  �_      ��                  �  �  `              �^?                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            restartServerObject                             �`  �`      ��                  �  �  a              F?                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            runServerObject                             �a  �a      ��                  �  �  b              L5?                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
                 ,b  
         ��                            ����                            startServerObject                               ,c  c      ��                  �  �  Dc              �)?                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            unbindServer                                0d  d      ��                  �  �  Hd              �i?                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  `d           ��                            ����                            getAppService   �Z      �d      �d  <  �      CHARACTER,  getASBound  �d      e      0e  = 
 �      LOGICAL,    getAsDivision   e      <e      le  >  �      CHARACTER,  getASHandle Le      xe      �e  ?         HANDLE, getASHasStarted �e      �e      �e  @        LOGICAL,    getASInfo   �e      �e      f  A 	       CHARACTER,  getASInitializeOnRun    �e       f      Xf  B  &      LOGICAL,    getASUsePrompt  8f      df      �f  C  ;      LOGICAL,    getServerFileName   tf      �f      �f  D  J      CHARACTER,  getServerOperatingMode  �f      �f      g  E  \      CHARACTER,  runServerProcedure  �f      $g      Xg  F  s      HANDLE,INPUT pcServerFileName CHARACTER,INPUT phAppService HANDLE   setAppService   8g      �g      �g  G  �      LOGICAL,INPUT pcAppService CHARACTER    setASDivision   �g      �g      $h  H  �      LOGICAL,INPUT pcDivision CHARACTER  setASHandle h      Hh      th  I  �      LOGICAL,INPUT phASHandle HANDLE setASInfo   Th      �h      �h  J 	 �      LOGICAL,INPUT pcInfo CHARACTER  setASInitializeOnRun    �h      �h      i  K  �      LOGICAL,INPUT plInitialize LOGICAL  setASUsePrompt  �h      <i      li  L  �      LOGICAL,INPUT plFlag LOGICAL    setServerFileName   Li      �i      �i  M  �      LOGICAL,INPUT pcFileName CHARACTER  setServerOperatingMode  �i      �i      j  N  �      LOGICAL,INPUT pcServerOperatingMode CHARACTER   addLink                             �j  �j      ��                  �  �  �j              �Q?                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  <k             k  
             ��   dk             0k               �� 
                 Xk  
         ��                            ����                            addMessage                              Pl  8l      ��                  �  �  hl              �,?                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �l             �l               ��   �l             �l               ��                  �l           ��                            ����                            adjustTabOrder                              �m  �m      ��                  �  �  �m              �@                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  0n             �m  
             �� 
  Xn             $n  
             ��                  Ln           ��                            ����                            applyEntry                              Do  ,o      ��                  �  �  \o              �A<                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  to           ��                            ����                            changeCursor                                pp  Xp      ��                  �  �  �p              $<                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �p           ��                            ����                            createControls                              �q  �q      ��                  �  �  �q              0<                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            destroyObject                               �r  �r      ��                  �  �  �r              �<                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            displayLinks                                �s  �s      ��                  �  �  �s              �<                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            editInstanceProperties                              �t  �t      ��                  �  �  �t               <                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            exitObject                              �u  �u      ��                  �  �  �u              �<                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            hideObject                              �v  �v      ��                  �  �  �v              @<                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeObject                                �w  �w      ��                  �  �  �w              �O<                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            modifyListProperty                              �x  �x      ��                  �  �  �x              �P<                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  $y             �x  
             ��   Ly             y               ��   ty             @y               ��                  hy           ��                            ����                            modifyUserLinks                             dz  Lz      ��                  �  �  |z              �X<                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �z             �z               ��   �z             �z               �� 
                 �z  
         ��                            ����                            removeAllLinks                              �{  �{      ��                  �  �  �{              �[<                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            removeLink                              �|  �|      ��                  �  �  �|              �\<                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  D}             }  
             ��   l}             8}               �� 
                 `}  
         ��                            ����                            repositionObject                                `~  H~      ��                  �    x~              q<                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �~             �~               ��                  �~           ��                            ����                            returnFocus                             �  �      ��                      �              t<                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
                 �  
         ��                            ����                            showMessageProcedure                                �  ̀      ��                    
  ��              �t<                    O   ����    e�          O   ����    R�          O   ����    ��            ��   H�             �               ��                  <�           ��                            ����                            toggleData                              4�  �      ��                      L�              �<                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  d�           ��                            ����                            viewObject                              \�  D�      ��                      t�              <                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            anyMessage  �i      ̃      ��  O 
 S      LOGICAL,    assignLinkProperty  ؃      �      8�  P  ^      LOGICAL,INPUT pcLink CHARACTER,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER   fetchMessages   �      ��      ��  Q  q      CHARACTER,  getChildDataKey ��      ̄      ��  R        CHARACTER,  getContainerHandle  ܄      �      <�  S  �      HANDLE, getContainerHidden  �      D�      x�  T  �      LOGICAL,    getContainerSource  X�      ��      ��  U  �      HANDLE, getContainerSourceEvents    ��      ��      ��  V  �      CHARACTER,  getContainerType    ܅      �      <�  W  �      CHARACTER,  getDataLinksEnabled �      H�      |�  X  �      LOGICAL,    getDataSource   \�      ��      ��  Y        HANDLE, getDataSourceEvents ��      ��      �  Z        CHARACTER,  getDataSourceNames  Ԇ       �      4�  [  (      CHARACTER,  getDataTarget   �      @�      p�  \  ;      CHARACTER,  getDataTargetEvents P�      |�      ��  ]  I      CHARACTER,  getDBAware  ��      ��      �  ^ 
 ]      LOGICAL,    getDesignDataObject ȇ      �      (�  _  h      CHARACTER,  getDynamicObject    �      4�      h�  `  |      LOGICAL,    getInstanceProperties   H�      t�      ��  a  �      CHARACTER,  getLogicalObjectName    ��      ��      ��  b  �      CHARACTER,  getLogicalVersion   Ј      ��      0�  c  �      CHARACTER,  getObjectHidden �      <�      l�  d  �      LOGICAL,    getObjectInitialized    L�      x�      ��  e  �      LOGICAL,    getObjectName   ��      ��      �  f  �      CHARACTER,  getObjectPage   ̉      ��      (�  g  �      INTEGER,    getObjectParent �      4�      d�  h        HANDLE, getObjectVersion    D�      l�      ��  i        CHARACTER,  getObjectVersionNumber  ��      ��      �  j  ,      CHARACTER,  getParentDataKey    Ċ      ��      $�  k  C      CHARACTER,  getPassThroughLinks �      0�      d�  l  T      CHARACTER,  getPhysicalObjectName   D�      p�      ��  m  h      CHARACTER,  getPhysicalVersion  ��      ��      �  n  ~      CHARACTER,  getPropertyDialog   ȋ      �      (�  o  �      CHARACTER,  getQueryObject  �      4�      d�  p  �      LOGICAL,    getRunAttribute D�      p�      ��  q  �      CHARACTER,  getSupportedLinks   ��      ��      ��  r  �      CHARACTER,  getTranslatableProperties   ��      �      (�  s  �      CHARACTER,  getUIBMode  �      4�      `�  t 
 �      CHARACTER,  getUserProperty @�      l�      ��  u  �      CHARACTER,INPUT pcPropName CHARACTER    instancePropertyList    |�      č      ��  v  		      CHARACTER,INPUT pcPropList CHARACTER    linkHandles ܍      $�      P�  w  	      CHARACTER,INPUT pcLink CHARACTER    linkProperty    0�      t�      ��  x  *	      CHARACTER,INPUT pcLink CHARACTER,INPUT pcPropName CHARACTER mappedEntry ��      ��      �  y  7	      CHARACTER,INPUT pcEntry CHARACTER,INPUT pcList CHARACTER,INPUT plFirst LOGICAL,INPUT pcDelimiter CHARACTER  messageNumber   �      x�      ��  z  C	      CHARACTER,INPUT piMessage INTEGER   propertyType    ��      ̏      ��  {  Q	      CHARACTER,INPUT pcPropName CHARACTER    reviewMessages  ܏      $�      T�  |  ^	      CHARACTER,  setChildDataKey 4�      `�      ��  }  m	      LOGICAL,INPUT cChildDataKey CHARACTER   setContainerHidden  p�      ��      �  ~  }	      LOGICAL,INPUT plHidden LOGICAL  setContainerSource  ̐      �      @�    �	      LOGICAL,INPUT phObject HANDLE   setContainerSourceEvents     �      `�      ��  �  �	      LOGICAL,INPUT pcEvents CHARACTER    setDataLinksEnabled |�      ��      ��  �  �	      LOGICAL,INPUT lDataLinksEnabled LOGICAL setDataSource   ԑ      �      L�  �  �	      LOGICAL,INPUT phObject HANDLE   setDataSourceEvents ,�      l�      ��  �  �	      LOGICAL,INPUT pcEventsList CHARACTER    setDataSourceNames  ��      Ȓ      ��  �  �	      LOGICAL,INPUT pcSourceNames CHARACTER   setDataTarget   ܒ      $�      T�  �  
      LOGICAL,INPUT pcTarget CHARACTER    setDataTargetEvents 4�      x�      ��  �  
      LOGICAL,INPUT pcEvents CHARACTER    setDBAware  ��      Г      ��  � 
 '
      LOGICAL,INPUT lAware LOGICAL    setDesignDataObject ܓ      �      P�  �  2
      LOGICAL,INPUT pcDataObject CHARACTER    setDynamicObject    0�      x�      ��  �  F
      LOGICAL,INPUT lTemp LOGICAL setInstanceProperties   ��      Ȕ       �  �  W
      LOGICAL,INPUT pcPropList CHARACTER  setLogicalObjectName    ��      $�      \�  �  m
      LOGICAL,INPUT c CHARACTER   setLogicalVersion   <�      x�      ��  �  �
      LOGICAL,INPUT cVersion CHARACTER    setObjectName   ��      Е       �  �  �
      LOGICAL,INPUT pcName CHARACTER  setObjectParent ��       �      P�  �  �
      LOGICAL,INPUT phParent HANDLE   setObjectVersion    0�      p�      ��  �  �
      LOGICAL,INPUT cObjectVersion CHARACTER  setParentDataKey    ��      ̖       �  �  �
      LOGICAL,INPUT cParentDataKey CHARACTER  setPassThroughLinks ��      (�      \�  �  �
      LOGICAL,INPUT pcLinks CHARACTER setPhysicalObjectName   <�      |�      ��  �  �
      LOGICAL,INPUT cTemp CHARACTER   setPhysicalVersion  ��      ԗ      �  �  �
      LOGICAL,INPUT cVersion CHARACTER    setRunAttribute �      ,�      \�  �        LOGICAL,INPUT cRunAttribute CHARACTER   setSupportedLinks   <�      ��      ��  �  !      LOGICAL,INPUT pcLinkList CHARACTER  setTranslatableProperties   ��      ܘ      �  �  3      LOGICAL,INPUT pcPropList CHARACTER  setUIBMode  ��      <�      h�  � 
 M      LOGICAL,INPUT pcMode CHARACTER  setUserProperty H�      ��      ��  �  X      LOGICAL,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER  showmessage ��      ��      $�  �  h      LOGICAL,INPUT pcMessage CHARACTER   Signature   �      H�      t�  � 	 t      CHARACTER,INPUT pcName CHARACTER    l�    '  ��  0�      �       4   �����                 @�                      ��                  (  U                  ��<                       (  Ě        )  \�  ؛      �       4   �����                 �                      ��                  *  T                  ��<                       *  l�  �    A  �  ��      �       4   �����                 ��                      ��                  M  O                  ��<                       M  �         N                                  ,     
                    � ߱        �  $  Q  ��  ���                           $  S  @�  ���                       x                         � ߱        x�    Y  ��  �      �      4   �����                �                      ��                  Z  	                  p�<                       Z  ��  H�  o   ]      ,                                 ��  $   ^  t�  ���                       �  @         �              � ߱        ��  �   _        Ȟ  �   `  �      ܞ  �   b        �  �   d  x      �  �   f  �      �  �   h  `      ,�  �   i  �      @�  �   j        T�  �   m  �      h�  �   o         |�  �   p  |      ��  �   r  �      ��  �   s  t      ��  �   t  �      ̟  �   u  ,      ��  �   v  �      ��  �   |  �      �  �   ~  P	      �  �   �  �	      0�  �   �   
      D�  �   �  t
      X�  �   �  �
      l�  �   �  l      ��  �   �  �      ��  �   �  \      ��  �   �  �      ��  �   �  D      Р  �   �  �      �  �   �  �      ��  �   �  0      �  �   �  �       �  �   �  �      4�  �   �        H�  �   �  X      \�  �   �  �      p�  �   �        ��  �   �  L      ��  �   �  �      ��  �   �  �      ��  �   �         ԡ  �   �  <      �  �   �  x      ��  �   �  �      �  �   �  �          �   �  ,                      <�          ��  ��      ��                  E	  s	  ��              ��<                    O   ����    e�          O   ����    R�          O   ����    ��      �     
                                     (                         � ߱        h�  $ Y	  آ  ���                           O   q	  ��  ��  h               ԣ          ģ  ̣    ��                                             ��                            ����                                8;      $�      ��     V     ܣ                       أ  |                     8�    �	  ��  �      t      4   ����t                 �                      ��                  �	  
                  ,�<                       �	  ��  4�  �   �	  �      H�  �   �	  H      \�  �   �	  �      p�  �   �	  @      ��  �   �	  �      ��  �   �	  8      ��  �   �	  �      ��  �   �	  (      ԥ  �   �	  �      �  �   �	         ��  �   �	  �      �  �   �	        $�  �   �	  �          �   �	        ��    J
  T�  Ц      x      4   ����x                �                      ��                  K
  �
                  4?                       K
  d�  ��  �   M
  �      �  �   N
  T      �  �   O
  �      0�  �   P
  D      D�  �   Q
  �      X�  �   R
  �      l�  �   T
  p      ��  �   U
  �      ��  �   V
  X      ��  �   W
  �      ��  �   X
  �      Ч  �   Y
  D       �  �   Z
  �       ��  �   [
  �       �  �   \
  x!       �  �   ]
  �!      4�  �   ^
  h"      H�  �   _
  �"      \�  �   `
  `#      p�  �   a
  �#      ��  �   b
  X$      ��  �   c
  �$      ��  �   d
  �$      ��  �   e
  L%      Ԩ  �   f
  �%      �  �   g
  <&      ��  �   h
  �&      �  �   i
  4'      $�  �   j
  �'      8�  �   k
  ,(      L�  �   l
  h(      `�  �   n
  �(      t�  �   o
  X)      ��  �   p
  �)      ��  �   q
  *      ��  �   r
  �*      ĩ  �   s
  �*      ة  �   t
  l+      �  �   u
  �+       �  �   v
  \,      �  �   w
  �,      (�  �   x
  L-      <�  �   y
  �-      P�  �   z
  <.      d�  �   {
  �.      x�  �   |
  4/      ��  �   }
  �/          �   ~
  $0      d�    �
  ��  8�      T0      4   ����T0                H�                      ��                  �
  �                  L�<                       �
  ̪  \�  �   �
  �0      p�  �   �
  (1      ��  �   �
  �1      ��  �   �
  2      ��  �      �2      ��  �     3      ԫ  �     |3      �  �     �3      ��  �     t4      �  �     �4      $�  �     l5      8�  �     �5      L�  �     d6      `�  �   	  �6      t�  �   
  L7      ��  �     �7      ��  �     <8      ��  �     �8      Ĭ  �     ,9      ج  �     �9      �  �     :       �  �     X:      �  �     �:      (�  �     H;      <�  �     �;      P�  �     8<          �     �<      x�    �  ��  ��      =      4   ����=  	              �                      ��             	     �  6                  ��=                       �  ��   �  �   �  |=      4�  �   �  �=      H�  �   �  t>      \�  �   �  �>      p�  �   �  l?      ��  �   �  �?      ��  �   �  \@      ��  �   �  �@      ��  �   �  TA      Ԯ  �   �  �A      �  �   �  DB      ��  �   �  �B      �  �   �  <C      $�  �   �  �C      8�  �   �  ,D      L�  �   �  �D      `�  �   �  $E      t�  �   �  �E      ��  �   �  F      ��  �   �  �F      ��  �   �  G      į  �   �  �G      د  �   �  �G      �  �   �  8H       �  �   �  �H      �  �   �  0I      (�  �   �  �I      <�  �   �   J          �   �  �J      getRowObjUpdStatic  deleteRecordStatic  �    �  ��  ��      K      4   ����K      /   �  а     �                          3   ����K             �                      3   ����<K  Զ    �  ,�  ��  �  XK      4   ����XK  
              ��                      ��             
     �  X                  e�                       �  <�  ̱  �   �  �K      $�  $  �  ��  ���                       �K     
                    � ߱        8�  �   �  L      ��  $   �  d�  ���                       ,L  @         L              � ߱        L�  $    ��  ���                       �L       	       	           � ߱        �M     
                N                     \O  @        
 O              � ߱        ܳ  V     �  ���                        hO       	       	       �O       
       
       �O       	       	           � ߱        l�  $  '  x�  ���                       �P     
                Q                     dR  @        
 $R              � ߱            V   9  �  ���                                      ̵                      ��                  Z  �                  hf�                       Z  ��  pR     
                �R                     <T  @        
 �S          �T  @        
 dT          U  @        
 �T          dU  @        
 $U              � ߱            V   o  �  ���                        adm-clone-props �  ��              �     W     `                          \  y                     start-super-proc    �  d�  �           �     X                                  �                     l�      �   �      �X      4   �����X      /     ,�     <�                          3   ���� Y            \�                      3   ���� Y  ķ  $  *  ��  ���                       @Y                         � ߱        ��    :  �  \�  ��  \Y      4   ����\Y                и                      ��                  ;  ?                  t��                       ;  �  pY                     �Y                     �Y                         � ߱            $  <  l�  ���                             @  �  T�      �Y      4   �����Y  �Y                         � ߱            $  A  (�  ���                       �Y                         � ߱        ع  $  E  ��  ���                       Ժ    H  ��  �  \�  �Y      4   �����Y      $  I  0�  ���                       Z                         � ߱            �   f  ,Z      lZ     
                �Z                     8\  @        
 �[              � ߱         �  V   z  p�  ���                        �  �   �  D\      �    ,  0�  @�      �\      4   �����\      /   -  l�     |�                          3   �����\            ��                      3   �����\  �\     
                P]                     �^  @        
 `^              � ߱        ��  V   9  ��  ���                        �^     
                h_                     �`  @        
 x`              � ߱        ̼  V   ]  <�  ���                        L�    �  �  d�      �`      4   �����`                t�                      ��                  �  �                  \��                       �  ��  �  /   �  ��     ��                          3   �����`            н                      3   �����`      /   �  �     �                          3   ����a            <�                      3   ����8a  ��  /  J  x�         la                      3   ����Ta  initProps   x�  ��              H     Y     @                          <    	                                   ̿          t�  \�      ��                 �    ��              x~�                    O   ����    e�          O   ����    R�          O   ����    ��      "                      ��          ��  p   
  ht  �        t�  d�     tt                                        ��                    '                  |��                         ��   �  ��     �t                                        ��                  (  D                  ��                       (  ��  ��  |�     �t                                        ��                  E  a                  ��                       E  �  �  �     �t                                        ��                  b  ~                  ���                       b  ��  ��  ��     �t                                        ��                    �                  ��                         (�  0�   �     �t                                        ��                  �  �                  ���                       �  ��  ��  ��     �t                                        ��                  �  �                  ���                       �  @�  H�  8�      u                                        ��                  �  �                  ���                       �  ��  ��  ��     u  	                                      ��             	     �                    ���                       �  X�  `�  P�     (u  
                                      ��             
       ,                  X��                         ��  ��  ��     <u                                        ��                  -  I                  (��                       -  p�  x�  h�     Pu                                        ��                  J  f                  ���                       J  ��  �  ��     du                                        ��                  g  �                  ���                       g  ��  ��  ��     xu                                        ��                  �  �                  ���                       �  �  �  �     �u                                        ��                  �  �                  ���                       �  ��  ��  ��     �u                                        ��                  �  �                  ���                       �  ,�  4�  $�     �u                                        ��                  �  �                  d��                       �  ��      ��     �u                                        ��                  �                    4��                       �  D�      O     ��  ��  �u               D�          ,�  8�   , �                                                       �     ��                            ����                            ��  d�  ��  �      ��     Z     L�                      � H�  4                     ��    -  �  ��      �u      4   �����u                ��                      ��                  .  B                  H��                       .  �  ��  /   /  ��     ��                          3   �����u            ��                      3   ����v  h�  /   0  (�     8�                          3   ����0v            X�                      3   ����Pv  ��  /   5  ��     ��                          3   ����lv            ��                      3   �����v      /   ;   �     �                          3   �����v            0�                      3   �����v  �v     
                hw                     �x  @        
 xx              � ߱        ��  V   �  @�  ���                        ��  $  �  ��  ���                       �x                         � ߱        �x     
                ly                     �z  @        
 |z              � ߱        ��  V   �  (�  ���                        t�  $  �  ��  ���                       �z     
                    � ߱        �z     
                X{                     �|  @        
 h|              � ߱        ��  V   �  �  ���                        \�  $    ��  ���                       �|     
                    � ߱        �|     
                D}                     �~  @        
 T~              � ߱        ��  V     ��  ���                        D�  $  %  ��  ���                       �~                         � ߱        �~     
                P                     ��  @        
 `�              � ߱        p�  V   /  ��  ���                        ��  �   I  ��      @�  $  J  ��  ���                       ؀     
                    � ߱        �     
                h�                     ��  @        
 x�              � ߱        l�  V   T  ��  ���                        ��  $  n  ��  ���                       Ă     
                    � ߱        ��  �   �  ؂      0�  $  �  �  ���                       �     
                    � ߱        D�  �   �  ,�      ��  $  �  p�  ���                       l�                         � ߱              �  ��  ��      ��      4   ������      /   �  ��     �                          3   ������  4�     
   $�                      3   ����ȃ  d�        T�                      3   ����Ѓ  ��        ��                      3   �����            ��                      3   ���� �  pushRowObjUpdTable  ��  ��  �                   [      �                               �                     pushTableAndValidate    ��  4�  �           |     \     �                          �  �                     remoteCommit    L�  ��  �           p     ]     �                          �  9                     serverCommit    ��  �  �           l     ^     �                          �  F                                     4�          �  ��      ��                  �  
  �              �                     O   ����    e�          O   ����    R�          O   ����    ��          O     ��  ��  0�    ��                            ����                            $�  P�      ��              _      L�                      
�     S                     disable_UI  ��  ��                      `      �                               f  
                    �  �    ����  �       ��       ��  8   ����   ��  8   ����       8   ����       8   ����       ��  ��      viewObject  ,   ��  ��  ��      toggleData  ,INPUT plEnabled LOGICAL    ��  �  4�      showMessageProcedure    ,INPUT pcMessage CHARACTER,OUTPUT plAnswer LOGICAL  �  x�  ��      returnFocus ,INPUT hTarget HANDLE   h�  ��  ��      repositionObject    ,INPUT pdRow DECIMAL,INPUT pdCol DECIMAL    ��  ��  �      removeLink  ,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE ��  \�  l�      removeAllLinks  ,   L�  ��  ��      modifyUserLinks ,INPUT pcMod CHARACTER,INPUT pcLinkName CHARACTER,INPUT phObject HANDLE p�  ��  ��      modifyListProperty  ,INPUT phCaller HANDLE,INPUT pcMode CHARACTER,INPUT pcListName CHARACTER,INPUT pcListValue CHARACTER    ��  t�  ��      hideObject  ,   d�  ��  ��      exitObject  ,   ��  ��  ��      editInstanceProperties  ,   ��  ��  ��      displayLinks    ,   ��  �  �      createControls  ,   ��  (�  8�      changeCursor    ,INPUT pcCursor CHARACTER   �  d�  p�      applyEntry  ,INPUT pcField CHARACTER    T�  ��  ��      adjustTabOrder  ,INPUT phObject HANDLE,INPUT phAnchor HANDLE,INPUT pcPosition CHARACTER ��  �  �      addMessage  ,INPUT pcText CHARACTER,INPUT pcField CHARACTER,INPUT pcTable CHARACTER ��  h�  p�      addLink ,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE X�  ��  ��      unbindServer    ,INPUT pcMode CHARACTER ��  ��  �      runServerObject ,INPUT phAppService HANDLE  ��  8�  L�      disconnectObject    ,   (�  `�  p�      destroyObject   ,   P�  ��  ��      bindServer  ,   t�  ��  ��      transferDBRow   ,INPUT pcRowIdent CHARACTER,INPUT piRowNum INTEGER  ��  ��  �      startFilter ,   ��  �  (�      releaseDBRow    ,   �  <�  L�      refetchDBRow    ,INPUT phRowObjUpd HANDLE   ,�  x�  ��      filterContainerHandler  ,INPUT phFilterContainer HANDLE h�  ��  ��      fetchDBRowForUpdate ,   ��  ��  ��      confirmContinue ,INPUT-OUTPUT pioCancel LOGICAL ��  (�  8�      compareDBRow    ,   �  L�  `�      bufferCopyDBToRO    ,INPUT phRowObj HANDLE,INPUT phBuffer HANDLE,INPUT pcExcludes CHARACTER,INPUT pcAssigns CHARACTER   <�  ��  ��      assignDBRow ,INPUT phRowObjUpd HANDLE   ��  �  �      updateState ,INPUT pcState CHARACTER    ��  D�  X�      updateQueryPosition ,   4�  l�  ��      updateAddQueryWhere ,INPUT pcWhere CHARACTER,INPUT pcField CHARACTER    \�  ��  ��      undoTransaction ,   ��  ��  ��      transferToExcel ,INPUT pcFieldList CHARACTER,INPUT plIncludeObj LOGICAL,INPUT plUseExisting LOGICAL,INPUT piMaxRecords INTEGER  ��  x�  ��      synchronizeProperties   ,INPUT pcPropertiesForServer CHARACTER,OUTPUT pcPropertiesForClient CHARACTER   h�  ��  �      submitValidation    ,INPUT pcValueList CHARACTER,INPUT pcUpdColumns CHARACTER   ��  P�  d�      submitForeignKey    ,INPUT pcRowIdent CHARACTER,INPUT-OUTPUT pcValueList CHARACTER,INPUT-OUTPUT pcUpdColumns CHARACTER  @�  ��  ��      submitCommit    ,INPUT pcRowIdent CHARACTER,INPUT plReopen LOGICAL  ��  ,�  @�      startServerObject   ,   �  T�  d�      setPropertyList ,INPUT pcProperties CHARACTER   D�  ��  ��      serverFetchRowObjUpdTable   ,OUTPUT TABLE-HANDLE phRowObjUpd    ��  ��  ��      serverSendRows  ,INPUT piStartRow INTEGER,INPUT pcRowIdent CHARACTER,INPUT plNext LOGICAL,INPUT piRowsToReturn INTEGER,OUTPUT piRowsReturned INTEGER,OUTPUT TABLE-HANDLE phRowObject    ��  ��  ��      saveContextAndDestroy   ,OUTPUT pcContext CHARACTER ��  ��   �      rowObjectState  ,INPUT pcState CHARACTER    ��  ,�  <�      retrieveFilter  ,   �  P�  d�      restartServerObject ,   @�  x�  ��      remoteSendRows  ,INPUT-OUTPUT piocContext CHARACTER,INPUT piStartRow INTEGER,INPUT pcRowIdent CHARACTER,INPUT plNext LOGICAL,INPUT piRowsToReturn INTEGER,OUTPUT pioRowsReturned INTEGER,OUTPUT TABLE-HANDLE phRowObject,OUTPUT pocMessages CHARACTER   h�  ��  ��      refreshRow  ,   p�  ��  ��      printToCrystal  ,INPUT pcFieldList CHARACTER,INPUT plIncludeObj LOGICAL,INPUT piMaxRecords INTEGER  ��  �  $�      isUpdatePending ,INPUT-OUTPUT plUpdate LOGICAL  �  T�  l�      initializeServerObject  ,   D�  ��  ��      initializeObject    ,   p�  ��  ��      home    ,   ��  ��  ��      genContextList  ,OUTPUT pcContext CHARACTER ��   �  �      fetchPrev   ,   ��   �  ,�      fetchNext   ,   �  @�  L�      fetchLast   ,   0�  `�  l�      fetchFirst  ,   P�  ��  ��      fetchBatch  ,INPUT plForwards LOGICAL   p�  ��  ��      endClientDataRequest    ,   ��  ��  ��      destroyServerObject ,   ��  �  �      describeSchema  ,INPUT pcSdoName CHARACTER,OUTPUT TABLE-HANDLE hTtSchema    ��  h�  x�      dataAvailable   ,INPUT pcRelative CHARACTER X�  ��  ��      copyColumns ,INPUT pcViewColList CHARACTER,INPUT phDataQuery HANDLE ��  ��  �      commitTransaction   ,   ��   �  0�      clientSendRows  ,INPUT piStartRow INTEGER,INPUT pcRowIdent CHARACTER,INPUT plNext LOGICAL,INPUT piRowsToReturn INTEGER,OUTPUT piRowsReturned INTEGER    �  ��  ��      batchServices   ,INPUT pcServices CHARACTER,OUTPUT pcValues CHARACTER        � 
"     
 s%     adecomm/as-utils.w 
"   
   �    }        �
"     
    �     }        �� "   K   %               � 
" 
   
 %              h �P  \         (          
�                          
�            � ~   -
" 
   
 <
�H T   %              �     }        �GG %              � 
"   
   P �L 
�H T   %              �     }        �GG %              
"   
   �        �    7%               
"   
 �               1� �  
 � �   %               o%   o           � �    
"   
 �           �    1� �   � �   %               o%   o           � �   
"   
 �           �    1� �  
 � �   %               o%   o           � �   
"   
 �           l    1� �   � �   %               o%   o           � �    
"   
 �           �    1� �   � �   %               o%   o           � �   
"   
 �           T    1�    �    %               o%   o           %               
"   
 �          �    1�    � %     
"   
 �               1� ,   � �   %               o%   o           � ?  
"   
 �           �    1� A   � �   %               o%   o           � P  S 
"   
 �           �    1� �   �    %               o%   o           %               
"   
 �           p    1� �   �    %               o%   o           %               
"   
 �           �    1� �   �    %               o%   o           %              
"   
 �          h    1� �   �      
"   
 �           �    1� �  
 �    %               o%   o           %               
"   
 �                1� �   � �   %               o%   o           � �    
"   
 �          �    1� �   � %     
"   
 �           �    1�    � �   %               o%   o           �   t 
"   
 �          D	    1� �  
 � %     
"   
 �           �	    1� �   � �   %               o%   o           � �  � 
"   
 �           �	    1� 9   � �   %               o%   o           � �    
"   
 �           h
    1� P  
 � [   %               o%   o           %               
"   
 <�           �
    1� _   <�    %               o%   o           %              
"   
 <�           `    1� g   <� �   %               o%   o           � �    <
"   
 <�           �    1� x   <� �   %               o%   o           o%   o           
"   
 <�           P    1� �  
 <� �   %               o%   o           � �    <
"   
 <�           �    1� �   <� �  	 %               o%   o           � �  / <
"   
 �          8    1� �   � �  	   
"   
 <�           t    1� �   <� �  	 o%   o           o%   o           � �    <
"   
 �          �    1�    � �  	   
"   
 <�           $    1�    <� �  	 o%   o           o%   o           � �    <
"   
 �          �    1� "   �      
"   
 �          �    1� 0   � �  	   
"   
 �              1� =   � �  	   
"   
 �          L    1� J   � �  	   
"   
 <�           �    1� X   <�    o%   o           o%   o           %              
"   
 �              1� i   � �  	   
"   
 �          @    1� w  
 � �     
"   
 �          |    1� �   � �  	   
"   
 �          �    1� �   � �  	   
"   
 �          �    1� �   � �  	   
"   
 �          0    1� �   � �  	   
"   
 �          l    1� �  	 � �  	   
"   
 �          �    1� �   � �  	   
"   
 �          �    1� �   � �  	   
"   
 <�                1�    <� �   %               o%   o           o%   o           
�H T   %              �     }        �GG %              
"   
   
"   
 <
"   
   
"   
 -(�  L ( l       �        �    ��    � P   �        �    �@    
� @  , 
�            ��      p�               �L
�    %              � 8          � $         �            
�    � :     
"   
 �� @  , 
�           �� �  
 �p�               �L"      P �L 
�H T   %              �     }        �GG %              
"   
 <�           �    1� =  
 <� �   %               o%   o           � �    <
"   
 <�           <    1� H  
 <� �   %               o%   o           o%   o           
"   
 <�           �    1� S   <� %   %               o%   o           o%   o           
"   
 <�           4    1� \   <�    %               o%   o           %               
"   
 <�           �    1� k   <�    %               o%   o           %               
"   
 s�           ,    1� x   s� �   %               o%   o           � �    <
"   
 <�           �    1�    <�    %               o%   o           %              
"   
 <�               1� �   <�    %               o%   o           o%   o           
"   
 <�           �    1� �   <� �   %               o%   o           o%   o           
"   
 <�               1� �  	 <� �   %               o%   o           � �    <
"   
 <�           �    1� �   <� �   %               o%   o           o%   o           
"   
 <�               1� �   <� �   %               o%   o           o%   o           
"   
 <�           �    1� �   <�    %               o%   o           %               
"   
 <�           �    1� �   <�    %               o%   o           o%   o           P �L 
�H T   %              �     }        �GG %              
"   
 <�           �    1� �  
 <�    %               o%   o           %              
"   
 <�           H    1� �   <� �   %               o%   o           o%   o           
"   
 <�           �    1�    <� �   %               o%   o           � �    <
"   
 <�           8    1�    <� �   %               o%   o           o%   o           
"   
 �          �    1� %   � %     
"   
 <�           �    1� 2   <� �   %               o%   o           � E  ! <
"   
 <�           d    1� g   <� �   %               o%   o           � �    <
"   
 <�           �    1� t   <� �   %               o%   o           � �   <
"   
 �          L    1� �   � �     
"   
 �          �    1� �   � %     
"   
 <�           �    1� �   <� �   %               o%   o           � �    <
"   
 �          8     1� �  
 � %     
"   
 s�           t     1� �   s�    %               o%   o           o%   o           
"   
 <�           �     1� �   <�    %               o%   o           %               
"   
 <�           l!    1� �   <�    %               o%   o           %               
"   
 <�           �!    1�     <� �   %               o%   o           � �    <
"   
 <�           \"    1�    <� �   %               o%   o           o%   o           
"   
 <�           �"    1�    <�    %               o%   o           %              
"   
 <�           T#    1� -   <�    %               o%   o           %               
"   
 s�           �#    1� :   s�    %               o%   o           %               
"   
 �          L$    1� J   � %     
"   
 �          �$    1� W   � �     
"   
 <�           �$    1� d   <� [   %               o%   o           o%   o           
"   
 <�           @%    1� p   <� �   %               o%   o           � �    <
"   
 <�           �%    1� ~   <� �   %               o%   o           o%   o           
"   
 <�           0&    1� �   <�    o%   o           o%   o           o%   o           
"   
 <�           �&    1� �   <� �  	 %               o%   o           o%   o           
"   
 <�           ('    1� �   <� �   %               o%   o           o%   o           
"   
 <�           �'    1� �  
 <� [   %               o%   o           o%   o           
"   
 �           (    1� �   � �     
"   
 <�           \(    1� �   <� �   %               o%   o           � �  4 <
"   
 <�           �(    1� '  
 <�    %               o%   o           %              
"   
 �          L)    1� 2   � %     
"   
 <�           �)    1� C   <� �   %               o%   o           � �    s
"   
 <�           �)    1� Q   <�    %               o%   o           %              
"   
 <�           x*    1� `   <� �   %               o%   o           � �    <
"   
 <�           �*    1� m   <� �   %               o%   o           � �    <
"   
 <�           `+    1� {   <� �   %               o%   o           � �    <
"   
 <�           �+    1� �   <�    %               o%   o           %               
"   
 <�           P,    1� �  	 <� %   %               o%   o           o%   o           
"   
 s�           �,    1� �   s� �   %               o%   o           � �  	 <
"   
 <�           @-    1� �   <� [   %               o%   o           %       �       
"   
 <�           �-    1� �   <� �   %               o%   o           � �    <
"   
 <�           0.    1� �   <�    o%   o           o%   o           %              
"   
 <�           �.    1� �   <�    %               o%   o           %               
"   
 <�           (/    1� �   <� �   %               o%   o           o%   o           
"   
 <�           �/    1�    <� �  	 %               o%   o           � �    <
"   
 �          0    1�    � �  	   P �L 
�H T   %              �     }        �GG %              
"   
 s�           �0    1� $  
 s� �   %               o%   o           � �    s
"   
 <�           1    1� /   <�    %               o%   o           %               
"   
 <�           �1    1� <  	 <� �   %               o%   o           � �    <
"   
 <�           2    1� F   <� �   %               o%   o           � �    <
"   
 <�           �2    1� T   <�    %               o%   o           %               
"   
 <�           �2    1� d   <� �   %               o%   o           � �    <
"   
 <�           p3    1� w   <� �   %               o%   o           o%   o           
"   
 <�           �3    1�    <� �   %               o%   o           o%   o           
"   
 <�           h4    1� �   <�    %               o%   o           o%   o           
"   
 s�           �4    1� �   s�    %               o%   o           o%   o           
"   
 <�           `5    1� �   <�    %               o%   o           o%   o           
"   
 <�           �5    1� �   <� �   %               o%   o           o%   o           
"   
 <�           X6    1� �  	 <� �  	 %               o%   o           � �    <
"   
 <�           �6    1� �  
 <� �  	 %               o%   o           � �    <
"   
 <�           @7    1� �   <� �   %               o%   o           � �    <
"   
 <�           �7    1� �   <� �   %               o%   o           o%   o           
"   
 <�           08    1� �   <� �   %               o%   o           o%   o           
"   
 <�           �8    1� 	   <� �   %               o%   o           � �    <
"   
 <�            9    1�    <� �   %               o%   o           � �    <
"   
 <�           �9    1� -   <� �  	 %               o%   o           o%   o           
"   
 �          :    1� ?   � %     
"   
 <�           L:    1� K   <� �   %               o%   o           � �    <
"   
 <�           �:    1� Y   <� �   %               o%   o           o%   o           
"   
 s�           <;    1� l   s�    %               o%   o           o%   o           
"   
 <�           �;    1� ~  
 <� �   %               o%   o           � �    <
"   
 <�           ,<    1� �   <� �   %               o%   o           � �    <
"   
 <�           �<    1� �   <�    %               o%   o           %               P �L 
�H T   %              �     }        �GG %              
"   
 <�           p=    1� �  	 <� %   %               o%   o           o%   o           
"   
 <�           �=    1� �   <� %   %               o%   o           o%   o           
"   
 <�           h>    1� �   <� %   %               o%   o           o%   o           
"   
 s�           �>    1� �   s�    %               o%   o           %              
"   
 <�           `?    1� �   <� �   %               o%   o           �   M s
"   
 <�           �?    1� U   <�    %               o%   o           %              
"   
 <�           P@    1� f   <�    %               o%   o           %               
"   
 <�           �@    1� z   <�    %               o%   o           %               
"   
 <�           HA    1� �   <� �  	 %               o%   o           � �   <
"   
 <�           �A    1� �   <�    %               o%   o           %               
"   
 <�           8B    1� �   <� �  	 %               o%   o           o%   o           
"   
 <�           �B    1� �   <�    o%   o           o%   o           %              
"   
 s�           0C    1� �   s� �  	 o%   o           o%   o           � �    s
"   
 <�           �C    1� �   <� %   o%   o           o%   o           o%   o           
"   
 <�            D    1�    <� %   o%   o           o%   o           o%   o           
"   
 <�           �D    1�    <� �  	 o%   o           o%   o           o%   o           
"   
 <�           E    1� $   <� %   o%   o           o%   o           o%   o           
"   
 <�           �E    1� 3   <� �  	 o%   o           o%   o           � A   <
"   
 <�           F    1� C   <� �  	 o%   o           o%   o           � R   <
"   
 <�           |F    1� ^   <�    %               o%   o           %               
"   
 <�           �F    1� r   <�    %               o%   o           %               
"   
 �          tG    1� �   � �  	   
"   
 <�           �G    1� �   <�    %               o%   o           %               
"   
 <�           ,H    1� �   <� �   %               o%   o           o%   o           
"   
 <�           �H    1� �   <� �   %               o%   o           o%   o           
"   
 <�           $I    1� �   <�    %               o%   o           o%   o           
"   
 <�           �I    1� �   <� �   %               o%   o           � �    <
"   
 <�           J    1� �   <� �   %               o%   o           %               
"   
 <�           �J    1�   	 <�    %               o%   o           %                "    %     start-super-proc B%     adm2/smart.p -P �L 
�H T   %              �     }        �GG %              
"   
   �       �K    6�      
"   
   
�        �K    8
"   
   �        �K    ��     }        �G 4              
"   
 ߱G %              G %              %�   AppService,ASInfo,ASUsePrompt,CacheDuration,CheckCurrentChanged,DestroyStateless,DisconnectAppServer,ServerOperatingMode,ShareData,UpdateFromSource,ForeignFields,ObjectName,OpenOnInit,PromptColumns,PromptOnDelete,RowsToBatch,RebuildOnRepos,ToggleDataTargets  
�H T   %              �     }        �GG %              
"   
 -
"   
 
"   
 -
"   
   (�  L ( l       �        �M    ��    � P   �        �M    �@    
� @  , 
�       �M    ��    -p�               �L
�    %              � 8       N    � $         �            
�    � :   -
"   
 �p� @  , 
�       O    �� ,   �p�               �L"  	  , �   � ;   <� =   �     }        �A      |    "  	    � ;   <%              (<   \ (    |    �     }        �A� ?   �A"  
  <    "  	  -"  
  <  < "  	  -"  
  <(    |    �     }        �A� ?   �A"  
  <
�H T   %              �     }        �GG %              
"   
 -
"   
 
"   
 -
"   
   (�  L ( l       �        �P    ��    � P   �        �P    �@    
� @  , 
�       �P    ��    -p�               �L
�    %              � 8      Q    � $         �            
�    � :   -
"   
 �p� @  , 
�       R    �� �  
 �p�               �L"  	  , 
�H T   %              �     }        �GG %              
"   
   
"   
 �
"   
   
"   
   (�  L ( l       �        �R    ��    � P   �        �R    �@    
� @  , 
�       �R    ��      p�               �L
�    %              � 8      �R    � $         �            
�    � :     
"   
 �p� @  , 
�       �S    �� �  
 �p�               �L%     SmartDataObject 
"   
   p� @  , 
�       XT    �� �     p�               �L%               
"   
  p� @  , 
�       �T    ��     p�               �L%               
"   
  p� @  , 
�       U    �� �    p�               �L(        � �      � �      � �      �     }        �A
�H T   %              �     }        �GG %              
"   
 < (   � 
"   
 -    �        �U    ��    �
"   
   � 8      DV    � $         �            
�    � :   -
"   
   �        �V    �
"   
   �       �V    /
"   
   
"   
   �       �V    6�      
"   
   
�        W    8
"   
   �        4W    �
"   
   �       TW    �
"   
   p�    � h   <
�    �     }        �G 4              
"   
 ߱G %              G %              
�     }        �
"   
    (   � 
"   
 -    �        X    �A"    �A
"   
   
�        dX    �@ � 
"   
 <"      �       }        �
"   
 %              %                "    %     start-super-proc A%     adm2/appserver.p �<�    � �     
�    �     }        �%               %      Server  - �     }        �    "    <� �    %               %      Client      "    <� �    %      NONE    p�,  8         $     "    <        � 
   -
�    
�H T   %              �     }        �GG %              
"   
 -
"   
 
"   
 -
"   
   (�  L ( l       �        �Z    ��    � P   �        �Z    �@    
� @  , 
�       �Z    ��    -p�               �L
�    %              � 8      �Z    � $         �            
�    � :   -
"   
 �p� @  , 
�       �[    �� �   �p�               �L"    , p�,  8         $     "    <        �    -
�     "    %     start-super-proc A%     adm2/dataquery.p �<
�H T   %              �     }        �GG %              
"   
 -
"   
 
"   
 -
"   
 -(�  L ( l       �         ]    ��    � P   �        ,]    �@    
� @  , 
�       8]    ��    -p�               �L
�    %              � 8      D]    � $         �     -     
�    � :   -
"   
 �p� @  , 
�       T^    �� �   �p�               �L%H > 8   dataAvailable,confirmContinue,isUpdatePending,buildDataRequest  
�H T   %              �     }        �GG %              
"   
 -
"   
 
"   
 -
"   
 -(�  L ( l       �        8_    ��    � P   �        D_    �@    
� @  , 
�       P_    ��    -p�               �L
�    %              � 8      \_    � $         �     -     
�    � :   -
"   
 �p� @  , 
�       l`    ��    �p�               �L%               "    %     start-super-proc @%     adm2/query.p -%     start-super-proc @%     adm2/queryext.p % 	    initProps -
�    %` V P   FOR EACH Almmmate NO-LOCK,       FIRST Almmmatg OF Almmmate NO-LOCK INDEXED-REPOSITION �   � �     � �     � �         "    <� �    
�H T   %              �     }        �GG %              
"   
 -
"   
 
"   
 -
"   
   (�  L ( l       �        pb    ��    � P   �        |b    �@    
� @  , 
�       �b    ��    -p�               �L
�    %              � 8      �b    � $         �            
�    � :   -
"   
 �p� @  , 
�       �c    �� <  	 �p�               �L"    , %T J D   rowObject.CodCia-2 = Almmmatg.CodCia  rowObject.codmat-2 = Almmmatg.codmat  �    "      � �         %              %                   "      %                  "      "      "     T(        "    �%              "    �� �   "      �       "    -�    "    �� ?   � �      � ?   -�    "     � ?    S    "      "        "    <%                � @    �     t T     P   4       "      (0       4       <"      � �      � �    -� �   <T ,  %              T   "    <"    � �     � ?   -� �   <T    �    "    <� ?   "      � ?   -"      %                   %              %                   "      %                  "      �     "      �     "       \      H   "      ((       "    -%              � �    �      4  �     "      
�H T   %              �     }        �GG %              
"   
 -
"   
 
"   
 -
"   
   (�  L ( l       �        �h    ��    � P   �        �h    �@    
� @  , 
�       �h    ��    -p�               �L
�    %              � 8      �h    � $         �            
�    � :   -
"   
 �p� @  , 
�       �i    �� $  
 �p�               �L"    ,       "  
  ��    � �    <� �         "  	    �    �   ) � �   <�   � �     � �     � �    -�   � �     � �   -�   ) <      "  
  <�    � �    �� �         "  	    �    � >  W � �   �   ,        "    -�    ��   � �   -� �   �� �       ,        "      �      �   � �   �� �   � >  W <�   � �     � �     � �  �   
�H T   %              �     }        �GG %              
"   
 
"   
 -
"   
 
"   
 (�  L ( l       �        �l    ��    � P   �        �l    �@    
� @  , 
�       �l    ��    p�               �L
�    %              � 8      �l    � $         �            
�    � :     
"   
 �p� @  , 
�       �m    �� �   �p�               �L"    , 
"   
   p� @  , 
�       n    �� �     p�               �L"    , 
"   
  p� @  , 
�       hn    �� d    p�               �L"    ,     %              %                   "      %                  "      �     "      �     "      4 (        "  
    �    � �      � �         "  	  ��     "    <T    "      "      @ A,    �   � �   �      "    -"       T      @   "    (        "      � �    -� �      � �   -"    �     "  	   %              D H   @ A,    �   � �   -�      "    -"    �,    S   "    -� �    �� �   %                T      @   "    (        "      � �    -� �      � �   -"    <     "  
   %                         "    �      "    -           "      �    -"      
�H T   %              �     }        �GG %              
"   
 <
"   
   
"   
 <
"   
 -(�  L ( l       �        �r    ��    � P   �        �r    �@    
� @  , 
�       �r    ��    <p�               �L
�    %              � 8      �r    � $         �     -     
�    � :   
"   
 �p� @  , 
�       �s    �� �   �p�               �L"    , 
"   
   p� @  , 
�       t    �� d     p�               �L"    , "      %              %              %              %              %              %              %              %              %       	       %       
       %              %              %              %              %              %              %              %              "       "    %     start-super-proc ?%     adm2/data.p %     start-super-proc ?%     adm2/dataext.p %     start-super-proc ?%     adm2/dataextcols.p %     start-super-proc ?%     adm2/dataextapi.p �
�H T   %              �     }        �GG %              
"   
 -
"   
 
"   
 -
"   
 -(�  L ( l       �        8w    ��    � P   �        Dw    �@    
� @  , 
�       Pw    ��    -p�               �L
�    %              � 8      \w    � $         �     -     
�    � :   -
"   
 �p� @  , 
�       lx    �� �   �p�               �L%               %      "alm/dmate-matg-p1.i" 
�H T   %              �     }        �GG %              
"   
 -
"   
 
"   
 -
"   
   (�  L ( l       �        <y    ��    � P   �        Hy    �@    
� @  , 
�       Ty    ��    -p�               �L
�    %              � 8      `y    � $         �            
�    � :   -
"   
 �p� @  , 
�       pz    �� �   �p�               �L"    , 
�     	         �G
�H T   %              �     }        �GG %              
"   
 -
"   
 
"   
 -
"   
   (�  L ( l       �        ({    ��    � P   �        4{    �@    
� @  , 
�       @{    ��    -p�               �L
�    %              � 8      L{    � $         �            
�    � :   -
"   
 �p� @  , 
�       \|    �� �  
 �p�               �L
"   
 , 
�     
        �G
�H T   %              �     }        �GG %              
"   
 -
"   
 
"   
 -
"   
   (�  L ( l       �        }    ��    � P   �         }    �@    
� @  , 
�       ,}    ��    -p�               �L
�    %              � 8      8}    � $         �            
�    � :   -
"   
 �p� @  , 
�       H~    �� �  	 �p�               �L
"   
 , 
"   
      � z  	   �        �~    �
�H T   %              �     }        �GG %              
"   
 -
"   
 
"   
 -
"   
   (�  L ( l       �             ��    � P   �        ,    �@    
� @  , 
�       8    ��    -p�               �L
�    %              � 8      D    � $         �            
�    � :   -
"   
 �p� @  , 
�       T�    ��     �p�               �L"    , 
"   
   �       ��    �"      
�     
        �G
�H T   %              �     }        �GG %              
"   
 -
"   
 
"   
 -
"   
   (�  L ( l       �        8�    ��    � P   �        D�    �@    
� @  , 
�       P�    ��    -p�               �L
�    %              � 8      \�    � $         �            
�    � :   -
"   
 �p� @  , 
�       l�    �� �  	 �p�               �L
"   
 , 
�             �Gp�,  8         $     
"   
 <        � �   -
�    
�             �Gp�,  8         $     
"   
 <        � �   -
�    �    � �     
�        "    <� �    %     modifyListProperty 
�    %      REMOVE  %     SupportedLinks %     Update-Target  %     bufferValidate  
�    "      �  %      setContextAndInitialize 
�    "      %     bufferCommit    
�    "      "      �    � "     
�    %               %     bufferCommit    
�    "      "      
�     
        �G�     }        �
�                    �           �   l       ��                 g  �  �               |��                    O   ����    e�          O   ����    R�          O   ����    ��        $  v  �   ���                       �U     
                    � ߱              w  (  �      V      4   ����V                �                      ��                  x  �                  8��                       x  8  �  �  y  PV            {  �  `      �V      4   �����V                p                      ��                  |  �                  ̲�                       |  �  �  o   }      ,                                 �  �   ~  �V      �  �     �V      $  $  �  �  ���                        W     
                    � ߱        8  �   �  @W      L  �   �  `W      `  �   �  �W          $   �  �  ���                       �W  @         �W              � ߱                     T          ,  @   T �            
             
             
             
                 $   4   D          $   4   D   ����        ��                            ����                                            �           �   l       ��                 �  �  �               ��                    O   ����    e�          O   ����    R�          O   ����    ��      �                      �          �  $  �    ���                       X     
                    � ߱                  �  �                      ��                   �  �                  ���                     �  4      4   ����$X      $  �  �  ���                       pX     
                    � ߱        �    �  4  D      �X      4   �����X      /  �  p                               3   �����X  �  �   �  �X          O   �  ��  ��  �X                               , �                          
                               �      ��                            ����                                            �           �   l       ��                 V  �  �               p��                    O   ����    e�          O   ����    R�          O   ����    ��        $  v  �   ���                       ta                         � ߱        d  $  w  8  ���                       �a                         � ߱             �  �  �      b      4   ����b  $b     
                �b                     �c  @        
 �c              � ߱            V   �  �  ���                        x  $  �  L  ���                       �c                         � ߱        <  $  �  �  ���                       Td                         � ߱          L      �  �                      ��        0                             ���      �d     X       �      $    x  ���                       td                         � ߱        �  $    �  ���                       �d                         � ߱            4   �����d  �d                     @e                     Le                     �e                     �e                         � ߱        �  $      ���                               �         �e      4   �����e      $    ,  ���                       f          0g             � ߱          $    �  ���                       <g                         � ߱          ,      �  �                      ��        0           !                  L��      �g     �       �      $    X  ���                       Pg                         � ߱        �  $    �  ���                       �g                         � ߱            4   �����g      $      ���                       �g                         � ߱        dh     
                �h                     0j  @        
 �i              � ߱        T  V   ,  D  ���                        <j       
       
       pj       	       	       �j                     �j                         � ߱         	  $  s  �  ���                       �j       
       
       0k       	       	       dk                     �k                         � ߱        ,	  $  �  �  ���                        
  $    X	  ���                       l                         � ߱        8l     
                �l                     n  @        
 �m          \n  @        
 n          �n  @        
 tn              � ߱        �
  V   #  �	  ���                          �
         �                      ��        0         �  �                  �{�      @o     p     �  L
      $  �  �
  ���                       �n                         � ߱        x  $  �  L  ���                       �n                         � ߱        �  4   ����o      4   ����To  �  $  �  �  ���                       �o                         � ߱            �    �      �o      4   �����o                �                      ��                  �  �                  �|�                       �    p                     �p       	       	           � ߱            $  �  �  ���                             �  (  �      �p      4   �����p                �                      ��                  �  �                  }�                       �  8  @q                     �q       
       
           � ߱            $  �  �  ���                       �q                     r                         � ߱          $  �  (  ���                       8r     
                �r                     t  @        
 �s          \t  @        
 t              � ߱            V   �  �  ���                                    7           �  �  � |                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             �   �   �   �   �   �   �   �       $  4  D  T  d  t  �  \  �  �  �  �  8HXhx��������(8HX      �   �   �   �   �   �   �   �      $  4  D  T  d  t  �   \ �  �  �  �  8HXhx��������(8HX  �                    � �                     �          ��                            ����                                                        �   l       ��                  l  w  �               �                     O   ����    e�          O   ����    R�          O   ����    ��              !                    ��                            ����                                            �           �   l       ��                  �  �  �               �                      O   ����    e�          O   ����    R�          O   ����    ��      �       �              �                  $                  d  /  �  $     4  8�                      3   �����            T                      3   ����@�      O   �  ��  ��  L�               �          �  �    �                                             ��                            ����                                            H          �   l       ��                  �  �  �               �                     O   ����    e�          O   ����    R�          O   ����    ��             �              �                $                         ,             �                                          �  /  �  t     �  t�                      3   ����P�            �                      3   ����|�     /  �  �     �  ��                      3   ������  x                             3   ������      $   �  L  ���                                                   � ߱                  �  �                  3   ������      $   �  �  ���                                                   � ߱        X  $  �  ,  ���                       Ą                         � ߱            O   �  ��  ��  ��               �          �  �   @ �                                                              0              0           ��                            ����                                                       �   l       ��                  �  �  �               �9                     O   ����    e�          O   ����    R�          O   ����    ��      �       $                                      �                                �              /  �  L     \  �                      3   �����  �        |  �                  3   �����      $   �  �  ���                                                   � ߱                                      3   ����$�      $   �  @  ���                                                   � ߱                     �          �  �   , �                                                                 ��                            ����                                            �           �   l       ��                  d  o  �               �                     O   ����    e�          O   ����    R�          O   ����    ��            n  �   �       D�      4   ����D�      �   n  X�    ��                            ����                            TXS appSrvUtils s-codcia s-codalm D:\newsie\on_in_co\aplic\alm\dmate-matg-p1.w should only be RUN PERSISTENT. ADDROW CANCELROW CANNAVIGATE CLOSEQUERY COLUMNPROPS COLVALUES COPYROW CREATEROW DELETEROW FETCHROW FETCHROWIDENT FINDROW FINDROWWHERE FIRSTROWIDS GETLASTCOMMITERRORTYPE HASFOREIGNKEYCHANGED OPENDATAQUERY OPENQUERY PREPAREQUERY ROWAVAILABLE ROWVALUES SUBMITROW UPDATEROW GETOBJECTTYPE xiRocketIndexLimit ADDQUERYWHERE ASSIGNQUERYSELECTION BUFFERCOMPAREDBTORO BUFFERWHERECLAUSE COLUMNDATATYPE COLUMNDBCOLUMN COLUMNQUERYSELECTION COLUMNTABLE COLUMNVALMSG DBCOLUMNDATANAME DBCOLUMNHANDLE EXCLUDECOLUMNS GETDATACOLUMNS GETFOREIGNVALUES GETQUERYPOSITION GETQUERYSORT GETQUERYSTRING GETQUERYWHERE GETTARGETPROCEDURE INDEXINFORMATION INSERTEXPRESSION NEWQUERYSTRING NEWQUERYVALIDATE NEWQUERYWHERE NEWWHERECLAUSE REFRESHROWIDENT REMOVEFOREIGNKEY REMOVEQUERYSELECTION RESOLVEBUFFER ROWIDWHERE ROWIDWHERECOLS SETQUERYPOSITION SETQUERYSORT SETQUERYSTRING SETQUERYWHERE WHERECLAUSEBUFFER GETAPPSERVICE GETASBOUND GETASDIVISION GETASHANDLE GETASHASSTARTED GETASINFO GETASINITIALIZEONRUN GETASUSEPROMPT GETSERVERFILENAME GETSERVEROPERATINGMODE RUNSERVERPROCEDURE SETAPPSERVICE SETASDIVISION SETASHANDLE SETASINFO SETASINITIALIZEONRUN SETASUSEPROMPT SETSERVERFILENAME SETSERVEROPERATINGMODE gshAstraAppserver gshSessionManager gshRIManager gshSecurityManager gshProfileManager gshRepositoryManager gshTranslationManager gshWebManager gscSessionId gsdSessionObj gshFinManager gshGenManager gshAgnManager gsdTempUniqueID gsdUserObj gsdRenderTypeObj gsdSessionScopeObj ghProp ghADMProps ghADMPropsBuf glADMLoadFromRepos glADMOk ANYMESSAGE ASSIGNLINKPROPERTY FETCHMESSAGES GETCHILDDATAKEY GETCONTAINERHANDLE GETCONTAINERHIDDEN GETCONTAINERSOURCE GETCONTAINERSOURCEEVENTS GETCONTAINERTYPE GETDATALINKSENABLED GETDATASOURCE GETDATASOURCEEVENTS GETDATASOURCENAMES GETDATATARGET GETDATATARGETEVENTS GETDBAWARE GETDESIGNDATAOBJECT GETDYNAMICOBJECT GETINSTANCEPROPERTIES GETLOGICALOBJECTNAME GETLOGICALVERSION GETOBJECTHIDDEN GETOBJECTINITIALIZED GETOBJECTNAME GETOBJECTPAGE GETOBJECTPARENT GETOBJECTVERSION GETOBJECTVERSIONNUMBER GETPARENTDATAKEY GETPASSTHROUGHLINKS GETPHYSICALOBJECTNAME GETPHYSICALVERSION GETPROPERTYDIALOG GETQUERYOBJECT GETRUNATTRIBUTE GETSUPPORTEDLINKS GETTRANSLATABLEPROPERTIES GETUIBMODE GETUSERPROPERTY INSTANCEPROPERTYLIST LINKHANDLES LINKPROPERTY MAPPEDENTRY MESSAGENUMBER PROPERTYTYPE REVIEWMESSAGES SETCHILDDATAKEY SETCONTAINERHIDDEN SETCONTAINERSOURCE SETCONTAINERSOURCEEVENTS SETDATALINKSENABLED SETDATASOURCE SETDATASOURCEEVENTS SETDATASOURCENAMES SETDATATARGET SETDATATARGETEVENTS SETDBAWARE SETDESIGNDATAOBJECT SETDYNAMICOBJECT SETINSTANCEPROPERTIES SETLOGICALOBJECTNAME SETLOGICALVERSION SETOBJECTNAME SETOBJECTPARENT SETOBJECTVERSION SETPARENTDATAKEY SETPASSTHROUGHLINKS SETPHYSICALOBJECTNAME SETPHYSICALVERSION SETRUNATTRIBUTE SETSUPPORTEDLINKS SETTRANSLATABLEPROPERTIES SETUIBMODE SETUSERPROPERTY SHOWMESSAGE SIGNATURE prepareInstance ObjectName CHAR  ObjectVersion ADM2.2 ObjectType SmartDataObject ContainerType PropertyDialog adm2/support/datad.w QueryObject LOGICAL ContainerHandle HANDLE InstanceProperties AppService,ASInfo,ASUsePrompt,CacheDuration,CheckCurrentChanged,DestroyStateless,DisconnectAppServer,ServerOperatingMode,ShareData,UpdateFromSource,ForeignFields,ObjectName,OpenOnInit,PromptColumns,PromptOnDelete,RowsToBatch,RebuildOnRepos,ToggleDataTargets SupportedLinks Data-Source,Data-Target,Navigation-Target,Update-Target,Commit-Target,Filter-Target ContainerHidden ObjectInitialized ObjectHidden ObjectsCreated HideOnInit UIBMode ContainerSource ContainerSourceEvents initializeObject,hideObject,viewObject,destroyObject,enableObject,confirmExit,confirmCancel,confirmOk,isUpdateActive DataSource DataSourceEvents dataAvailable,queryPosition,updateState,deleteComplete,fetchDataSet,confirmContinue,confirmCommit,confirmUndo,assignMaxGuess,isUpdatePending TranslatableProperties ObjectPage INT DBAware DesignDataObject DataSourceNames DataTarget DataTargetEvents CHARACTER updateState,rowObjectState,fetchBatch,LinkState LogicalObjectName PhysicalObjectName LogicalVersion PhysicalVersion DynamicObject RunAttribute ChildDataKey ParentDataKey DataLinksEnabled InactiveLinks InstanceId DECIMAL SuperProcedure SuperProcedureMode SuperProcedureHandle LayoutPosition ClassName RenderingProcedure ThinRenderingProcedure Label cType ADMProps Target WHERE Target = WIDGET-H(" ") AppService ASDivision ASHandle ASHasConnected ASHasStarted ASInfo ASInitializeOnRun ASUsePrompt BindSignature BindScope ServerOperatingMode ServerFileName ServerFirstCall NeedContext AutoCommit BLOBColumns BufferHandles CLOBColumns CommitSource CommitSourceEvents commitTransaction,undoTransaction CommitTarget CommitTargetEvents rowObjectState CurrentRowid ROWID CurrentUpdateSource DataColumns DataHandle DataIsFetched DataModified DataQueryBrowsed DataQueryString FetchOnOpen FillBatchOnRepos FilterActive FilterAvailable FilterSource FilterWindow FirstRowNum ForeignFields ForeignValues IgnoreTreeViewFilter IndexInformation LargeColumns LastRowNum NavigationSource NavigationSourceEvents fetchFirst,fetchNext,fetchPrev,fetchLast,startFilter OpenOnInit PrimarySDOSource PromptColumns PromptOnDelete QueryColumns QueryPosition QueryString RebuildOnRepos RowObject RowObjectState NoUpdates RowsToBatch Tables ToggleDataTargets TransferChildrenForAll UpdatableColumns UpdatableWhenNew UpdateSource AssignList AuditEnabled BaseQuery CalcFieldList CheckLastOnOpen DataColumnsByTable DBNames EntityFields FetchHasAudit FetchHasComment FetchAutoComment FirstResultRow KeyFields KeyTableId LastDBRowIdent LastResultRow NewBatchInfo NoLockReadOnlyTables PhysicalTables PositionForClient QueryHandle QueryRowIdent RequiredProperties SkipTransferDBRow TempTables UpdatableColumnsByTable UpdateFromSource RowObjUpd RowObjectTable RowObjUpdTable CheckCurrentChanged StatelessSavedProperties CheckCurrentChanged,RowObjectState,LastResultRow,FirstResultRow,QueryRowIdent DestroyStateless DisconnectAppServer ServerSubmitValidation DataFieldDefs "alm/dmate-matg-p1.i" QueryContainer QueryContext AsynchronousSDO DataLogicProcedure DataLogicObject DataReadHandler DataReadColumns DataReadBuffer DataDelimiter | DataReadFormat TrimNumeric IsRowObjectExternal IsRowObjUpdExternal ManualAddQueryWhere DynamicData LastCommitErrorType LastCommitErrorKeys RunDataLogicProxy SchemaLocation CacheDuration INTEGER ShareData ghContainer adm2/smart.p cObjectName iStart / \ . hReposBuffer hPropTable hBuffer hTable deleteProperties ADM-CLONE-PROPS pcProcName hProc START-SUPER-PROC cAppService cASDivision cServerOperatingMode adm2/appserver.p getAppService Server Client NONE setASDivision setAppService adm2/dataquery.p dataAvailable,confirmContinue,isUpdatePending,buildDataRequest adm2/query.p adm2/queryext.p cTable iTable cColumns cDataCols cUpdCols cCalcData cCalcUpd iNumData iNumUpd cBuffers cKeyFields cAssignList iAssigns iPos iEntry iCount cTables cTableAssign cDbEntry cField cKeyTable cQueryString FOR EACH Almmmate NO-LOCK,       FIRST Almmmatg OF Almmmate NO-LOCK INDEXED-REPOSITION ,   Almmmate Almmmatg  rowObject.CodCia-2 = Almmmatg.CodCia  rowObject.codmat-2 = Almmmatg.codmat ; AlmDes CodAlm CodCia codmat CodUbi StkAct CodCia-2 codfam CodMar codmat-2 CodPr1 DesMar DesMat TpoArt UndStk subfam FchIng Pesmat AlmDes CodAlm CodCia codmat CodUbi StkAct CodCia-2 codfam CodMar codmat-2 CodPr1 DesMar DesMat TpoArt UndStk subfam FchIng Pesmat INITPROPS piTableIndex lRet DELETERECORDSTATIC adm2/data.p adm2/dataext.p adm2/dataextcols.p adm2/dataextapi.p AlmDes CodAlm CodCia codmat CodUbi StkAct CodCia-2 codfam CodMar codmat-2 CodPr1 DesMar DesMat TpoArt UndStk subfam FchIng Pesmat RowNum RowIdent RowMod RowIdentIdx RowUserProp ChangedFields cContainerType hRowObject hDataQuery cDataFieldDefs FOR EACH  setRowObjectTable setRowObjUpdTable getUpdatableColumns REMOVE Update-Target PUSHROWOBJUPDTABLE pcValType PUSHTABLEANDVALIDATE pcContext pcMessages pcUndoIds obtainContextForClient REMOTECOMMIT SERVERCOMMIT GETROWOBJUPDSTATIC DISABLE_UI qDataQuery x  0  �  �=      / �<   ��      0         pcServices      ��      T         pcServices  �   ��      x         piStartRow  �   ��      �         piStartRow  �   ��      �         piStartRow  �   ��      �         piStartRow      ��              piStartRow  <  ��      ,        pcViewColList       ��      T       
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
 hTarget �  ��      �        pcMessage       ��      �        pcMessage       ��      �        plEnabled             �     cType       0     V   �                             getObjectType   Y	  q	  s	  `        P  
   hReposBuffer    �        t  
   hPropTable  �        �  
   hBuffer           �  
   hTable  �  �     W   <          �                  adm-clone-props v  w  x  y  {  |  }  ~    �  �  �  �  �  �  �  �            P  
   hProc             p        pcProcName  �  �  	   X   <  X      �                  start-super-proc    �  �  �  �  �  �  �  �  �           �     cTable               iTable  <        0     cColumns    \        P     cDataCols   |        p     cUpdCols    �        �     cCalcData   �        �     cCalcUpd    �     	   �     iNumData    �     
   �     iNumUpd              cBuffers    8        ,     cKeyFields  X        L     cAssignList x        l     iAssigns    �        �     iPos    �        �     iEntry  �        �     iCount  �        �     cTables        �     cTableAssign    ,              cDbEntry    H        @     cField  h        \     cKeyTable             |     cQueryString    |  �  "   Y   �          �                  initProps   v  w  �  �  �  �                  !  ,  s  �    #  �  �  �  �  �  �  �  �  �  �  �  �  �  �            d     lRet              �        piTableIndex    �  �  (   Z   P  l      �                  deleteRecordStatic  
    '  (  D  E  a  b  ~    �  �  �  �  �  �  �  �      ,  -  I  J  f  g  �  �  �  �  �  �  �  �  �  �                         !       �  �     [       x      �                  pushRowObjUpdTable  w  �        �        pcValType                  $       �  \     \       �      D                  pushTableAndValidate    �  �  �  �        �        pcContext   �             $       �        �        pcMessages            �        pcUndoIds     ,     ]       h                        remoteCommit    �  �  �  �  �  X             $       |        p        pcMessages            �        pcUndoIds   �  �     ^       @      �                  serverCommit    �  �  �  ,     _                                 getRowObjUpdStatic    
  �  p     `               d                  disable_UI  n  o  4  <!       �      !                      �  �  �     RowObject   �         �         �         �         �                                              $         0         8         @         H         P         X         `         h         p         x         �         �         �         AlmDes  CodAlm  CodCia  codmat  CodUbi  StkAct  CodCia-2    codfam  CodMar  codmat-2    CodPr1  DesMar  DesMat  TpoArt  UndStk  subfam  FchIng  Pesmat  RowNum  RowIdent    RowMod  RowIdentIdx RowUserProp     �  �     RowObjUpd   �         �         �         �                                              $         ,         8         @         H         P         X         `         h         p         x         �         �         �         �         �         AlmDes  CodAlm  CodCia  codmat  CodUbi  StkAct  CodCia-2    codfam  CodMar  codmat-2    CodPr1  DesMar  DesMat  TpoArt  UndStk  subfam  FchIng  Pesmat  RowNum  RowIdent    RowMod  RowIdentIdx RowUserProp ChangedFields   �          �  
   appSrvUtils �        �     s-codcia                 s-codalm    D       0     xiRocketIndexLimit  l        X  
   gshAstraAppserver   �        �  
   gshSessionManager   �        �  
   gshRIManager    �        �  
   gshSecurityManager    	 	     �  
   gshProfileManager   4  
 
       
   gshRepositoryManager    `        H  
   gshTranslationManager   �        t  
   gshWebManager   �        �     gscSessionId    �        �     gsdSessionObj   �        �  
   gshFinManager             
   gshGenManager   8        (  
   gshAgnManager   \        L     gsdTempUniqueID |        p     gsdUserObj  �        �     gsdRenderTypeObj    �        �     gsdSessionScopeObj  �       �  
   ghProp         �  
   ghADMProps  ,         
   ghADMPropsBuf   T       @     glADMLoadFromRepos  p       h     glADMOk �       �  
   ghContainer �    	   �     cObjectName �    
   �     iStart  �       �     cAppService                cASDivision 8               cServerOperatingMode    \        L      cContainerType  �        p      cQueryString    �        �   
   hRowObject  �        �   
   hDataQuery  �        �      cColumns             �      cDataFieldDefs   !    X  !  RowObject         X  0!  RowObjUpd          "   >   �   �   �   �   '  (  )  *  A  M  N  O  Q  S  T  U  Y  Z  ]  ^  _  `  b  d  f  h  i  j  m  o  p  r  s  t  u  v  |  ~  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  
  J
  K
  M
  N
  O
  P
  Q
  R
  T
  U
  V
  W
  X
  Y
  Z
  [
  \
  ]
  ^
  _
  `
  a
  b
  c
  d
  e
  f
  g
  h
  i
  j
  k
  l
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
  ~
  �
  �
  �
  �
  �
  �
  �
                     	  
                          �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  6  �  �  �  �  �  �  �  �      '  9  X  Z  o  �      *  :  ;  <  ?  @  A  E  H  I  f  z  �  ,  -  9  ]  �  �  �  �  �  J  -  .  /  0  5  ;  B  �  �  �  �  �      %  /  I  J  T  n  �  �  �  �  �  �      ��  D:\newsie\on_in_co\aplic\alm\dmate-matg-p1.w h%  ��  C:\Progress\OpenEdge\src\adm2\data.i �%  �) . %C:\Progress\OpenEdge\src\adm2\custom\datacustom.i    �%  �� - C:\Progress\OpenEdge\src\adm2\robjflds.i &  @. , D:\newsie\on_in_co\aplic\alm\dmate-matg-p1.i D&  �   C:\Progress\OpenEdge\src\adm2\query.i    |&  z + C:\Progress\OpenEdge\src\adm2\delrecst.i �&  `W * C:\Progress\OpenEdge\src\adm2\tblprep.i  �&   ) %C:\Progress\OpenEdge\src\adm2\custom\querycustom.i   '  � ! C:\Progress\OpenEdge\src\adm2\dataquery.i    X'  �Z ( %C:\Progress\OpenEdge\src\adm2\custom\dataquerycustom.i   �'  �< " C:\Progress\OpenEdge\src\adm2\appserver.i    �'  �� ' %C:\Progress\OpenEdge\src\adm2\custom\appservercustom.i   (  I� # C:\Progress\OpenEdge\src\adm2\smart.i    P(  Ds & C:\Progress\OpenEdge\gui\fn  �(  tw % %C:\Progress\OpenEdge\src\adm2\custom\smartcustom.i   �(  Q. $ C:\Progress\OpenEdge\gui\set �(  �>  C:\Progress\OpenEdge\src\adm2\dataprop.i )  ��  %C:\Progress\OpenEdge\src\adm2\custom\datapropcustom.i    H)  ��  %C:\Progress\OpenEdge\src\adm2\custom\dataprtocustom.i    �)  �K  C:\Progress\OpenEdge\src\adm2\qryprop.i  �)  -�  %C:\Progress\OpenEdge\src\adm2\custom\qrypropcustom.i *  ��  %C:\Progress\OpenEdge\src\adm2\custom\qryprtocustom.i D*   
 C:\Progress\OpenEdge\src\adm2\dataqueryprop.i    �*  �d  %C:\Progress\OpenEdge\src\adm2\custom\dataquerypropcustom.i   �*  ��  %C:\Progress\OpenEdge\src\adm2\custom\dataqueryprtocustom.i   +  �l  C:\Progress\OpenEdge\src\adm2\appsprop.i P+  ɏ  %C:\Progress\OpenEdge\src\adm2\custom\appspropcustom.i    �+  V  %C:\Progress\OpenEdge\src\adm2\custom\appsprtocustom.i    �+  i$  C:\Progress\OpenEdge\src\adm2\smrtprop.i ,  �j  C:\Progress\OpenEdge\gui\get @,  �  %C:\Progress\OpenEdge\src\adm2\custom\smrtpropcustom.i    h,  ��  %C:\Progress\OpenEdge\src\adm2\custom\smrtprtocustom.i    �,  ��  C:\Progress\OpenEdge\src\adm2\smrtprto.i �,  Su  C:\Progress\OpenEdge\src\adm2\globals.i  $-  M�  %C:\Progress\OpenEdge\src\adm2\custom\globalscustom.i X-  )a  %C:\Progress\OpenEdge\src\adm2\custom\smartdefscustom.i   �-  �  C:\Progress\OpenEdge\src\adm2\appsprto.i �-  ��  %C:\Progress\OpenEdge\src\adm2\custom\appserverdefscustom.i   .  ��  C:\Progress\OpenEdge\src\adm2\dataqueryprto.i    X.  ª  %C:\Progress\OpenEdge\src\adm2\custom\dataquerydefscustom.i   �.  �� 	 C:\Progress\OpenEdge\src\adm2\qryprto.i  �.  �  %C:\Progress\OpenEdge\src\adm2\custom\querydefscustom.i   /  �`  C:\Progress\OpenEdge\src\adm2\dataprto.i T/  �  %C:\Progress\OpenEdge\src\adm2\custom\datadefscustom.i    �/  e�  !C:\Progress\OpenEdge\gui\adecomm\appserv.i   �/  �    D:\newsie\on_in_co\aplic\alm\dmate-matg-p1_cl.w         �      @0  �        P0  [  �     `0     �  &   p0  �   8     �0     �  .   �0  �   �     �0     �     �0  �   �     �0     �  $   �0  �   �     �0     p  $   �0  �   n      1     L  $   1  �   I      1     '  $   01  �   %     @1       $   P1  �         `1     �  $   p1  �   �     �1     �  $   �1  �   �     �1     �  $   �1  �   �     �1     q  -   �1  �   m     �1     Z  ,   �1  k          2  �        2     �  +    2  �  �      02     �  +   @2  �  �      P2     �  +   `2  �  �      p2     �  +   �2  �  �      �2     �  +   �2  �  �      �2     i  +   �2  �  f      �2     L  +   �2  �  I      �2     /  +    3  �  ,      3       +    3  �        03     �  +   @3  �  �      P3     �  +   `3  �  �      p3     �  +   �3  �  �      �3     �  +   �3  �  �      �3     �  +   �3  �  ~      �3     d  +   �3  �  a      �3     G  +    4  �  D      4     *  +    4  �  '      04       +   @4  �  �      P4     �  $   `4  �  �      p4     �  $   �4  j  �      �4     a  $   �4  i  `      �4     >  $   �4  h  =      �4       $   �4  ^        �4     �  *    5  ]  �      5     �  *    5  \  �      05     �  *   @5  [  �      P5     v  *   `5  Z  u      p5     O  *   �5  Y  N      �5     (  *   �5  X  '      �5       *   �5  W         �5     �  *   �5  V  �      �5     �  *    6  U  �      6     �  *    6  T  �      06     e  *   @6  S  d      P6     >  *   `6  R  =      p6       *   �6  Q        �6     �  *   �6  P  �      �6     �  *   �6  O  �      �6     �  *   �6  N  �      �6     {  *    7  M  z      7     T  *    7  ?  F      07     $  $   @7    �      P7     �  $   `7  �   F      p7     �  )   �7  g   �      �7  a   �  !   �7     y  (   �7  _   w  !   �7     U  $   �7  ]   S  !   �7     1  $   �7  I     !    8  �     "   8     �  '    8  �   �  "   08     �  $   @8  �   �  "   P8     r  $   `8  �   p  "   p8     N  $   �8  g   4  "   �8          �8  O   �  "   �8  �   �  #   �8     �  &   �8  �   U  #   �8     �  %   �8  �   �  #    9     �  $   9  �   �  #    9     �  $   09  �   �  #   @9     �  $   P9  �   �  #   `9     g  $   p9  �   S  #   �9     1  $   �9  }   %  #   �9       $   �9     �  #   �9     9  "   �9     �  !   �9     �      �9     ?      :  �   6     :  O   (      :          0:     �     @:  �   �     P:  �   �     `:  O   y     p:     h     �:          �:  y   �
     �:  �   �
  
   �:  G   �
     �:     �
     �:     �
     �:  c   "
  
   �:  x   
      ;  M   
     ;     �	      ;     �	     0;  a   �	     @;  �  p	     P;     Q	     `;  �  	     p;  O   	     �;     �     �;     �     �;  �   �     �;     �     �;          �;  x   �     �;     �     �;     l      <     h     <     T      <     ;     0<  Q   +     @<     �     P<     �     `<     �     p<     k     �<  ]   e  
   �<     [     �<       
   �<          �<     �  
   �<  Z   �     �<     �  	   �<     �      =     �     =     �      =  c   o     0=     M     @=          P=     �      `=     �      p=     �      �=     &      �=           �=           
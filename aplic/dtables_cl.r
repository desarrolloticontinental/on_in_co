	��V�I�6   �                                              � 36E000EFutf-8 MAIN E:\OpenEdge\on_in_co\APLIC\dtables_cl.w,, PROCEDURE disable_UI,, PROCEDURE serverCommit,,INPUT-OUTPUT RowObjUpd TABLE,OUTPUT pcMessages CHARACTER,OUTPUT pcUndoIds CHARACTER PROCEDURE remoteCommit,,INPUT-OUTPUT pcContext CHARACTER,INPUT-OUTPUT RowObjUpd TABLE,OUTPUT pcMessages CHARACTER,OUTPUT pcUndoIds CHARACTER PROCEDURE pushTableAndValidate,,INPUT pcValType CHARACTER,INPUT-OUTPUT RowObjUpd TABLE PROCEDURE pushRowObjUpdTable,,INPUT RowObjUpd TABLE PROCEDURE initProps,, PROCEDURE start-super-proc,,INPUT pcProcName CHARACTER PROCEDURE adm-clone-props,, PROCEDURE viewObject,, PROCEDURE toggleData,,INPUT plEnabled LOGICAL PROCEDURE showMessageProcedure,,INPUT pcMessage CHARACTER,OUTPUT plAnswer LOGICAL PROCEDURE returnFocus,,INPUT hTarget HANDLE PROCEDURE repositionObject,,INPUT pdRow DECIMAL,INPUT pdCol DECIMAL PROCEDURE removeLink,,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE PROCEDURE removeAllLinks,, PROCEDURE modifyUserLinks,,INPUT pcMod CHARACTER,INPUT pcLinkName CHARACTER,INPUT phObject HANDLE PROCEDURE modifyListProperty,,INPUT phCaller HANDLE,INPUT pcMode CHARACTER,INPUT pcListName CHARACTER,INPUT pcListValue CHARACTER PROCEDURE hideObject,, PROCEDURE exitObject,, PROCEDURE editInstanceProperties,, PROCEDURE displayLinks,, PROCEDURE createControls,, PROCEDURE changeCursor,,INPUT pcCursor CHARACTER PROCEDURE applyEntry,,INPUT pcField CHARACTER PROCEDURE adjustTabOrder,,INPUT phObject HANDLE,INPUT phAnchor HANDLE,INPUT pcPosition CHARACTER PROCEDURE addMessage,,INPUT pcText CHARACTER,INPUT pcField CHARACTER,INPUT pcTable CHARACTER PROCEDURE addLink,,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE PROCEDURE unbindServer,,INPUT pcMode CHARACTER PROCEDURE runServerObject,,INPUT phAppService HANDLE PROCEDURE disconnectObject,, PROCEDURE destroyObject,, PROCEDURE bindServer,, PROCEDURE transferDBRow,,INPUT pcRowIdent CHARACTER,INPUT piRowNum INTEGER PROCEDURE startFilter,, PROCEDURE releaseDBRow,, PROCEDURE refetchDBRow,,INPUT phRowObjUpd HANDLE PROCEDURE filterContainerHandler,,INPUT phFilterContainer HANDLE PROCEDURE fetchDBRowForUpdate,, PROCEDURE confirmContinue,,INPUT-OUTPUT pioCancel LOGICAL PROCEDURE compareDBRow,, PROCEDURE bufferCopyDBToRO,,INPUT phRowObj HANDLE,INPUT phBuffer HANDLE,INPUT pcExcludes CHARACTER,INPUT pcAssigns CHARACTER PROCEDURE assignDBRow,,INPUT phRowObjUpd HANDLE PROCEDURE updateState,,INPUT pcState CHARACTER PROCEDURE updateQueryPosition,, PROCEDURE updateAddQueryWhere,,INPUT pcWhere CHARACTER,INPUT pcField CHARACTER PROCEDURE undoTransaction,, PROCEDURE transferToExcel,,INPUT pcFieldList CHARACTER,INPUT plIncludeObj LOGICAL,INPUT plUseExisting LOGICAL,INPUT piMaxRecords INTEGER PROCEDURE synchronizeProperties,,INPUT pcPropertiesForServer CHARACTER,OUTPUT pcPropertiesForClient CHARACTER PROCEDURE submitValidation,,INPUT pcValueList CHARACTER,INPUT pcUpdColumns CHARACTER PROCEDURE submitForeignKey,,INPUT pcRowIdent CHARACTER,INPUT-OUTPUT pcValueList CHARACTER,INPUT-OUTPUT pcUpdColumns CHARACTER PROCEDURE submitCommit,,INPUT pcRowIdent CHARACTER,INPUT plReopen LOGICAL PROCEDURE startServerObject,, PROCEDURE setPropertyList,,INPUT pcProperties CHARACTER PROCEDURE serverFetchRowObjUpdTable,,OUTPUT phRowObjUpd TABLE-HANDLE PROCEDURE serverSendRows,,INPUT piStartRow INTEGER,INPUT pcRowIdent CHARACTER,INPUT plNext LOGICAL,INPUT piRowsToReturn INTEGER,OUTPUT piRowsReturned INTEGER,OUTPUT phRowObject TABLE-HANDLE PROCEDURE saveContextAndDestroy,,OUTPUT pcContext CHARACTER PROCEDURE rowObjectState,,INPUT pcState CHARACTER PROCEDURE retrieveFilter,, PROCEDURE restartServerObject,, PROCEDURE remoteSendRows,,INPUT-OUTPUT piocContext CHARACTER,INPUT piStartRow INTEGER,INPUT pcRowIdent CHARACTER,INPUT plNext LOGICAL,INPUT piRowsToReturn INTEGER,OUTPUT pioRowsReturned INTEGER,OUTPUT phRowObject TABLE-HANDLE,OUTPUT pocMessages CHARACTER PROCEDURE refreshRow,, PROCEDURE printToCrystal,,INPUT pcFieldList CHARACTER,INPUT plIncludeObj LOGICAL,INPUT piMaxRecords INTEGER PROCEDURE isUpdatePending,,INPUT-OUTPUT plUpdate LOGICAL PROCEDURE initializeServerObject,, PROCEDURE initializeObject,, PROCEDURE home,, PROCEDURE genContextList,,OUTPUT pcContext CHARACTER PROCEDURE fetchPrev,, PROCEDURE fetchNext,, PROCEDURE fetchLast,, PROCEDURE fetchFirst,, PROCEDURE fetchBatch,,INPUT plForwards LOGICAL PROCEDURE endClientDataRequest,, PROCEDURE destroyServerObject,, PROCEDURE describeSchema,,INPUT pcSdoName CHARACTER,OUTPUT hTtSchema TABLE-HANDLE PROCEDURE dataAvailable,,INPUT pcRelative CHARACTER PROCEDURE copyColumns,,INPUT pcViewColList CHARACTER,INPUT phDataQuery HANDLE PROCEDURE commitTransaction,, PROCEDURE clientSendRows,,INPUT piStartRow INTEGER,INPUT pcRowIdent CHARACTER,INPUT plNext LOGICAL,INPUT piRowsToReturn INTEGER,OUTPUT piRowsReturned INTEGER PROCEDURE batchServices,,INPUT pcServices CHARACTER,OUTPUT pcValues CHARACTER FUNCTION deleteRecordStatic,logical,INPUT piTableIndex INTEGER FUNCTION getRowObjUpdStatic,widget-handle, FUNCTION Signature,CHARACTER,INPUT pcName CHARACTER FUNCTION showmessage,LOGICAL,INPUT pcMessage CHARACTER FUNCTION setUserProperty,LOGICAL,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER FUNCTION setUIBMode,LOGICAL,INPUT pcMode CHARACTER FUNCTION setTranslatableProperties,LOGICAL,INPUT pcPropList CHARACTER FUNCTION setSupportedLinks,LOGICAL,INPUT pcLinkList CHARACTER FUNCTION setRunAttribute,LOGICAL,INPUT cRunAttribute CHARACTER FUNCTION setPhysicalVersion,LOGICAL,INPUT cVersion CHARACTER FUNCTION setPhysicalObjectName,LOGICAL,INPUT cTemp CHARACTER FUNCTION setPassThroughLinks,LOGICAL,INPUT pcLinks CHARACTER FUNCTION setParentDataKey,LOGICAL,INPUT cParentDataKey CHARACTER FUNCTION setObjectVersion,LOGICAL,INPUT cObjectVersion CHARACTER FUNCTION setObjectParent,LOGICAL,INPUT phParent HANDLE FUNCTION setObjectName,LOGICAL,INPUT pcName CHARACTER FUNCTION setLogicalVersion,LOGICAL,INPUT cVersion CHARACTER FUNCTION setLogicalObjectName,LOGICAL,INPUT c CHARACTER FUNCTION setInstanceProperties,LOGICAL,INPUT pcPropList CHARACTER FUNCTION setDynamicObject,LOGICAL,INPUT lTemp LOGICAL FUNCTION setDesignDataObject,LOGICAL,INPUT pcDataObject CHARACTER FUNCTION setDBAware,LOGICAL,INPUT lAware LOGICAL FUNCTION setDataTargetEvents,LOGICAL,INPUT pcEvents CHARACTER FUNCTION setDataTarget,LOGICAL,INPUT pcTarget CHARACTER FUNCTION setDataSourceNames,LOGICAL,INPUT pcSourceNames CHARACTER FUNCTION setDataSourceEvents,LOGICAL,INPUT pcEventsList CHARACTER FUNCTION setDataSource,LOGICAL,INPUT phObject HANDLE FUNCTION setDataLinksEnabled,LOGICAL,INPUT lDataLinksEnabled LOGICAL FUNCTION setContainerSourceEvents,LOGICAL,INPUT pcEvents CHARACTER FUNCTION setContainerSource,LOGICAL,INPUT phObject HANDLE FUNCTION setContainerHidden,LOGICAL,INPUT plHidden LOGICAL FUNCTION setChildDataKey,LOGICAL,INPUT cChildDataKey CHARACTER FUNCTION reviewMessages,CHARACTER, FUNCTION propertyType,CHARACTER,INPUT pcPropName CHARACTER FUNCTION messageNumber,CHARACTER,INPUT piMessage INTEGER FUNCTION mappedEntry,CHARACTER,INPUT pcEntry CHARACTER,INPUT pcList CHARACTER,INPUT plFirst LOGICAL,INPUT pcDelimiter CHARACTER FUNCTION linkProperty,CHARACTER,INPUT pcLink CHARACTER,INPUT pcPropName CHARACTER FUNCTION linkHandles,CHARACTER,INPUT pcLink CHARACTER FUNCTION instancePropertyList,CHARACTER,INPUT pcPropList CHARACTER FUNCTION getUserProperty,CHARACTER,INPUT pcPropName CHARACTER FUNCTION getUIBMode,CHARACTER, FUNCTION getTranslatableProperties,CHARACTER, FUNCTION getSupportedLinks,CHARACTER, FUNCTION getRunAttribute,CHARACTER, FUNCTION getQueryObject,LOGICAL, FUNCTION getPropertyDialog,CHARACTER, FUNCTION getPhysicalVersion,CHARACTER, FUNCTION getPhysicalObjectName,CHARACTER, FUNCTION getPassThroughLinks,CHARACTER, FUNCTION getParentDataKey,CHARACTER, FUNCTION getObjectVersionNumber,CHARACTER, FUNCTION getObjectVersion,CHARACTER, FUNCTION getObjectParent,HANDLE, FUNCTION getObjectPage,INTEGER, FUNCTION getObjectName,CHARACTER, FUNCTION getObjectInitialized,LOGICAL, FUNCTION getObjectHidden,LOGICAL, FUNCTION getLogicalVersion,CHARACTER, FUNCTION getLogicalObjectName,CHARACTER, FUNCTION getInstanceProperties,CHARACTER, FUNCTION getDynamicObject,LOGICAL, FUNCTION getDesignDataObject,CHARACTER, FUNCTION getDBAware,LOGICAL, FUNCTION getDataTargetEvents,CHARACTER, FUNCTION getDataTarget,CHARACTER, FUNCTION getDataSourceNames,CHARACTER, FUNCTION getDataSourceEvents,CHARACTER, FUNCTION getDataSource,HANDLE, FUNCTION getDataLinksEnabled,LOGICAL, FUNCTION getContainerType,CHARACTER, FUNCTION getContainerSourceEvents,CHARACTER, FUNCTION getContainerSource,HANDLE, FUNCTION getContainerHidden,LOGICAL, FUNCTION getContainerHandle,HANDLE, FUNCTION getChildDataKey,CHARACTER, FUNCTION fetchMessages,CHARACTER, FUNCTION assignLinkProperty,LOGICAL,INPUT pcLink CHARACTER,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER FUNCTION anyMessage,LOGICAL, FUNCTION setServerOperatingMode,LOGICAL,INPUT pcServerOperatingMode CHARACTER FUNCTION setServerFileName,LOGICAL,INPUT pcFileName CHARACTER FUNCTION setASUsePrompt,LOGICAL,INPUT plFlag LOGICAL FUNCTION setASInitializeOnRun,LOGICAL,INPUT plInitialize LOGICAL FUNCTION setASInfo,LOGICAL,INPUT pcInfo CHARACTER FUNCTION setASHandle,LOGICAL,INPUT phASHandle HANDLE FUNCTION setASDivision,LOGICAL,INPUT pcDivision CHARACTER FUNCTION setAppService,LOGICAL,INPUT pcAppService CHARACTER FUNCTION runServerProcedure,HANDLE,INPUT pcServerFileName CHARACTER,INPUT phAppService HANDLE FUNCTION getServerOperatingMode,CHARACTER, FUNCTION getServerFileName,CHARACTER, FUNCTION getASUsePrompt,LOGICAL, FUNCTION getASInitializeOnRun,LOGICAL, FUNCTION getASInfo,CHARACTER, FUNCTION getASHasStarted,LOGICAL, FUNCTION getASHandle,HANDLE, FUNCTION getAsDivision,CHARACTER, FUNCTION getASBound,LOGICAL, FUNCTION getAppService,CHARACTER, FUNCTION whereClauseBuffer,CHARACTER,INPUT pcWhere CHARACTER FUNCTION setQueryWhere,LOGICAL,INPUT pcWhere CHARACTER FUNCTION setQueryString,LOGICAL,INPUT pcQueryString CHARACTER FUNCTION setQuerySort,LOGICAL,INPUT pcSort CHARACTER FUNCTION setQueryPosition,LOGICAL,INPUT pcPosition CHARACTER FUNCTION rowidWhereCols,CHARACTER,INPUT pcColumns CHARACTER,INPUT pcValues CHARACTER,INPUT pcOperators CHARACTER FUNCTION rowidWhere,CHARACTER,INPUT pcWhere CHARACTER FUNCTION resolveBuffer,CHARACTER,INPUT pcBuffer CHARACTER FUNCTION removeQuerySelection,LOGICAL,INPUT pcColumns CHARACTER,INPUT pcOperators CHARACTER FUNCTION removeForeignKey,LOGICAL, FUNCTION refreshRowident,CHARACTER,INPUT pcRowident CHARACTER FUNCTION newWhereClause,CHARACTER,INPUT pcBuffer CHARACTER,INPUT pcExpression CHARACTER,INPUT pcWhere CHARACTER,INPUT pcAndOr CHARACTER FUNCTION newQueryWhere,CHARACTER,INPUT pcWhere CHARACTER FUNCTION newQueryValidate,CHARACTER,INPUT pcQueryString CHARACTER,INPUT pcExpression CHARACTER,INPUT pcBuffer CHARACTER,INPUT pcAndOr CHARACTER FUNCTION newQueryString,CHARACTER,INPUT pcColumns CHARACTER,INPUT pcValues CHARACTER,INPUT pcOperators CHARACTER,INPUT pcQueryString CHARACTER,INPUT pcAndOr CHARACTER FUNCTION insertExpression,CHARACTER,INPUT pcWhere CHARACTER,INPUT pcExpression CHARACTER,INPUT pcAndOr CHARACTER FUNCTION indexInformation,CHARACTER,INPUT pcQuery CHARACTER,INPUT plUseTableSep LOGICAL,INPUT pcIndexInfo CHARACTER FUNCTION getTargetProcedure,HANDLE, FUNCTION getQueryWhere,CHARACTER, FUNCTION getQueryString,CHARACTER, FUNCTION getQuerySort,CHARACTER, FUNCTION getQueryPosition,CHARACTER, FUNCTION getForeignValues,CHARACTER, FUNCTION getDataColumns,CHARACTER, FUNCTION excludeColumns,CHARACTER,INPUT iTable INTEGER FUNCTION dbColumnHandle,HANDLE,INPUT pcColumn CHARACTER FUNCTION dbColumnDataName,CHARACTER,INPUT pcDbColumn CHARACTER FUNCTION columnValMsg,CHARACTER,INPUT pcColumn CHARACTER FUNCTION columnTable,CHARACTER,INPUT pcColumn CHARACTER FUNCTION columnQuerySelection,CHARACTER,INPUT pcColumn CHARACTER FUNCTION columnDbColumn,CHARACTER,INPUT pcColumn CHARACTER FUNCTION columnDataType,CHARACTER,INPUT pcColumn CHARACTER FUNCTION bufferWhereClause,CHARACTER,INPUT pcBuffer CHARACTER,INPUT pcWhere CHARACTER FUNCTION bufferCompareDBToRO,LOGICAL,INPUT phRowObjUpd HANDLE,INPUT phBuffer HANDLE,INPUT pcExcludes CHARACTER,INPUT pcAssigns CHARACTER FUNCTION assignQuerySelection,LOGICAL,INPUT pcColumns CHARACTER,INPUT pcValues CHARACTER,INPUT pcOperators CHARACTER FUNCTION addQueryWhere,LOGICAL,INPUT pcWhere CHARACTER,INPUT pcBuffer CHARACTER,INPUT pcAndOr CHARACTER FUNCTION getObjectType,character, FUNCTION updateRow,LOGICAL,INPUT pcKeyValues CHARACTER,INPUT pcValueList CHARACTER FUNCTION submitRow,LOGICAL,INPUT pcRowIdent CHARACTER,INPUT pcValueList CHARACTER FUNCTION rowValues,CHARACTER,INPUT pcColumns CHARACTER,INPUT pcFormat CHARACTER,INPUT pcDelimiter CHARACTER FUNCTION rowAvailable,LOGICAL,INPUT pcDirection CHARACTER FUNCTION prepareQuery,LOGICAL,INPUT pcQuery CHARACTER FUNCTION openQuery,LOGICAL, FUNCTION openDataQuery,LOGICAL,INPUT pcPosition CHARACTER FUNCTION hasForeignKeyChanged,LOGICAL, FUNCTION getLastCommitErrorType,CHARACTER, FUNCTION firstRowIds,CHARACTER,INPUT pcQueryString CHARACTER FUNCTION findRowWhere,LOGICAL,INPUT pcColumns CHARACTER,INPUT pcValues CHARACTER,INPUT pcOperators CHARACTER FUNCTION findRow,LOGICAL,INPUT pcKeyValues CHARACTER FUNCTION fetchRowIdent,CHARACTER,INPUT pcRowIdent CHARACTER,INPUT pcViewColList CHARACTER FUNCTION fetchRow,CHARACTER,INPUT piRow INTEGER,INPUT pcViewColList CHARACTER FUNCTION deleteRow,LOGICAL,INPUT pcRowIdent CHARACTER FUNCTION createRow,LOGICAL,INPUT pcValueList CHARACTER FUNCTION copyRow,CHARACTER,INPUT pcViewColList CHARACTER FUNCTION colValues,CHARACTER,INPUT pcViewColList CHARACTER FUNCTION columnProps,CHARACTER,INPUT pcColList CHARACTER,INPUT pcPropList CHARACTER FUNCTION closeQuery,LOGICAL, FUNCTION canNavigate,LOGICAL, FUNCTION cancelRow,CHARACTER, FUNCTION addRow,CHARACTER,INPUT pcViewColList CHARACTER TEMP-TABLE RowObjUpd 0,RowNum:RowNum 0 NO,CodCia integer 0 0,CodAlm character 1 0,Descripcion character 2 0,RowNum integer 3 0,RowIdent character 4 0,RowMod character 5 0,RowIdentIdx character 6 0,RowUserProp character 7 0,ChangedFields character 8 0        \              p�              }� \  �              ��              ;     +   0� t  W   �� D  X   � |  Y   d�   [   h�   \   t� 0  ]   ��   ^   ��    `   ? ب �  iSO8859-1                                                                           �    �                                      �                   ��                   �       ~�   8�              ��  �   $      0                                                         PROGRESS                         �           
    
                    �              �                                                                                                     
  H  �      �  
    
                  �  x             4                                                                                          �          
  �  �      p  
    
                  \  $             �                                                                                          �          
  �          
    
                    �             �                                                                                                    
  L        �  
    
                  �  |             8                                                                                                    
  �  /      t  
    
                  `  (             �                                                                                          /          
  �  A         
    
                    �             �                                                                                          A          
  P  V      �  
    
                  �  �  	           <                                                                                          V          
  �  l      x  
    
                  d  ,  
           �                                                                                          l          
  �  z      $                           �             �                                                                                          z            T  �      �                        �  �             @                                                                                          �             	  �      |  
    
                  h  0	             �                                                                                          �          
  �	  �      (	  
    
                  	  �	             �	                                                                                          �          
  X
  �      �	  
    
                  �	  �
             D
                                                                                          �          
    �      �
                        l
  4             �
                                                                                          �            �  �      ,                          �             �                                                                                          �            \  �      �                        �  �             H                                                                                          �                �      �                        p               �                                                                                          �            p         �       |  X  �     �  |  �W               |             �          �      �              �       �  X  $     @  �  �o      �  	       �         �    $          �      �                 T�                                               X�          `  �  L l@                �         
             
             
                                         
                                                                                                               
             
                                          L   \   l   |   �   �   �   �   �   �   �   �       ,  <  L  \      L   \   l   |   �   �   �   �   �   �   �   �      ,  <  L  \                                                                                                                                     	                                 8  @  D  L  H          P             d  l  t  �  |          �             �  �  �  �  �          �             �  �  �                                      $                              (  0  8  @                             D  P  X  d                             h  t  |  �                                                                          CodCia  999 Cia Cia 0   C�digo de Compa�ia  CodAlm  x(3)    Almac�n Almac�n     C�digo de almac�n   Descripcion X(40)   Descripci�n Descripci�n     Descripci�n de almac�n  RowNum  ->,>>>,>>9  RowNum  0   RowIdent    x(8)    RowIdent        RowMod  x(8)    RowMod      RowIdentIdx x(8)    RowIdentIdx     RowUserProp x(8)    RowUserProp     �  ���	������               2        B        I                �     i     i     i     	 	 	        &  2  9  B  I  U                                                                                                                                     	                  
                                 �  �  �  �  �          �             �  �  �  �  �          �                   4  (          8             P  X  d  l                             p  |  �  �                              �  �  �  �                             �  �  �  �                             �  �  �  �                              �                                                                                 CodCia  999 Cia Cia 0   C�digo de Compa�ia  CodAlm  x(3)    Almac�n Almac�n     C�digo de almac�n   Descripcion X(40)   Descripci�n Descripci�n     Descripci�n de almac�n  RowNum  ->,>>>,>>9  RowNum  0   RowIdent    x(8)    RowIdent        RowMod  x(8)    RowMod      RowIdentIdx x(8)    RowIdentIdx     RowUserProp x(8)    RowUserProp     ChangedFields   x(8)    ChangedFields       �  ���
������               2        B        I                �     i     i     i     	 	 	        &  2  9  B  I  U  a    ��                            ����                            �    t�                    o�    undefined                                                               �       x�  x   `   ��  ��                    �����               �`e        O   ����    e�          O   ����    R�          O   ����    ��      d        �   �           4   ����     /      �                                3   ����       $       8  ���                       8      
                       � ߱        x  �   "   D       �     >          |�     �   �            4   ����d                 $                      ��                  �   �                   <@f           �   �  h  	  �   X                                        3   ����|       O   �   ��  ��  �   batchServices                                 �      ��                  U  X                 �        O   ����    e�          O   ����    R�          O   ����    ��            ��   l             8               ��                  `           ��                            ����                            clientSendRows                              P  8      ��                  Z  `  h              �f        O   ����    e�          O   ����    R�          O   ����    ��            ��   �             �               ��   �             �               ��                �               ��   ,             �               ��                              ��                            ����                            commitTransaction                                 �      ��                  b  c  ,              ��f        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            copyColumns                               �      ��                  e  h                 4�f        O   ����    e�          O   ����    R�          O   ����    ��            ��   l             8               �� 
                 `  
         ��                            ����                            dataAvailable                               P  8      ��                  j  l  h              |�o        O   ����    e�          O   ����    R�          O   ����    ��            ��                  �           ��                            ����                            describeSchema                              p	  X	      ��                  n  q  �	              ,�o        O   ����    e�          O   ����    R�          O   ����    ��            ��   �	             �	               �� 
          �       �	  
         ��                            ����                            destroyServerObject                             �
  �
      ��                  s  t  �
              ��o        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            endClientDataRequest                                �  �      ��                  v  w  �              ��o        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            fetchBatch                              �  �      ��                  y  {  �              d�o        O   ����    e�          O   ����    R�          O   ����    ��            ��                  �           ��                            ����                            fetchFirst                              �  �      ��                  }  ~  �              �        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            fetchLast                               �  �      ��                  �  �  �              (        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            fetchNext                               �  �      ��                  �  �  �              �        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            fetchPrev                               �  �      ��                  �  �  �              �         O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            genContextList                              �  �      ��                  �  �  �              P!        O   ����    e�          O   ����    R�          O   ����    ��            ��                  �           ��                            ����                            home                                �  �      ��                  �  �  �              �%        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeObject                                �  �      ��                  �  �  �              �(        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeServerObject                              �  �      ��                  �  �  �              H)        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            isUpdatePending                             �  �      ��                  �  �  �              �)        O   ����    e�          O   ����    R�          O   ����    ��            ��                  �           ��                            ����                            printToCrystal                              �  �      ��                  �  �  �              �F        O   ����    e�          O   ����    R�          O   ����    ��            ��   0             �               ��   X             $               ��                  L           ��                            ����                            refreshRow                              8         ��                  �  �  P              �R        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            remoteSendRows                              0        ��                  �  �  H              S        O   ����    e�          O   ����    R�          O   ����    ��            ��   �             `               ��   �             �               ��   �             �               ��                �               ��   4                             ��   \             (               �� 
  �      �       P  
             ��                  x           ��                            ����                            restartServerObject                             l  T      ��                  �  �  �              �        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            retrieveFilter                              d  L      ��                  �  �  |              Ԩ        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            rowObjectState                              \  D      ��                  �  �  t              ��        O   ����    e�          O   ����    R�          O   ����    ��            ��                  �           ��                            ����                            saveContextAndDestroy                               �  l      ��                  �  �  �              �        O   ����    e�          O   ����    R�          O   ����    ��            ��                  �           ��                            ����                            serverSendRows                              �  �      ��                  �  �  �              0o        O   ����    e�          O   ����    R�          O   ����    ��            ��                 �               ��   0              �               ��   X              $                ��   �              L                ��   �              t                �� 
          �       �   
         ��                            ����                            serverFetchRowObjUpdTable                               �!  �!      ��                  �  �  �!              <~        O   ����    e�          O   ����    R�          O   ����    ��            �� 
          �       �!  
         ��                            ����                            setPropertyList                             �"  �"      ��                  �  �  �"              `�        O   ����    e�          O   ����    R�          O   ����    ��            ��                  �"           ��                            ����                            serverSendRows                              �#  �#      ��                  �  �  �#              ��        O   ����    e�          O   ����    R�          O   ����    ��            ��   <$             $               ��   d$             0$               ��   �$             X$               ��   �$             �$               ��   �$             �$               �� 
          �       �$  
         ��                            ����                            startServerObject                               �%  �%      ��                  �  �  �%              ��g        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            submitCommit                                �&  �&      ��                  �  �  �&              H�g        O   ����    e�          O   ����    R�          O   ����    ��            ��    '             �&               ��                  '           ��                            ����                            submitForeignKey                                (  �'      ��                  �  �   (              (�g        O   ����    e�          O   ����    R�          O   ����    ��            ��   l(             8(               ��   �(             `(               ��                  �(           ��                            ����                            submitValidation                                |)  d)      ��                  �  �  �)              8�g        O   ����    e�          O   ����    R�          O   ����    ��            ��   �)             �)               ��                  �)           ��                            ����                            synchronizeProperties                               �*  �*      ��                  �  �  �*              ,�        O   ����    e�          O   ����    R�          O   ����    ��            ��   0+             �*               ��                  $+           ��                            ����                            transferToExcel                             ,  �+      ��                      ,,              �        O   ����    e�          O   ����    R�          O   ����    ��            ��   x,             D,               ��   �,             l,               ��   �,             �,               ��                  �,           ��                            ����                            undoTransaction                             �-  �-      ��                      �-              ��        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            updateAddQueryWhere                             �.  �.      ��                      �.              <�        O   ����    e�          O   ����    R�          O   ����    ��            ��   /             �.               ��                   /           ��                            ����                            updateQueryPosition                             �/  �/      ��                      0              @�        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            updateState                             �0  �0      ��                       1              t�        O   ����    e�          O   ����    R�          O   ����    ��            ��                  1           ��                            ����                            addRow          �1      �1     T       CHARACTER,INPUT pcViewColList CHARACTER cancelRow   �1      �1      �1   	 [       CHARACTER,  canNavigate �1      2      42    e       LOGICAL,    closeQuery  2      @2      l2   
 q       LOGICAL,    columnProps L2      x2      �2    |       CHARACTER,INPUT pcColList CHARACTER,INPUT pcPropList CHARACTER  colValues   �2      �2      3   	 �       CHARACTER,INPUT pcViewColList CHARACTER copyRow �2      83      `3    �       CHARACTER,INPUT pcViewColList CHARACTER createRow   @3      �3      �3   	 �       LOGICAL,INPUT pcValueList CHARACTER deleteRow   �3      �3      4   	 �       LOGICAL,INPUT pcRowIdent CHARACTER  fetchRow    �3      (4      T4  	  �       CHARACTER,INPUT piRow INTEGER,INPUT pcViewColList CHARACTER fetchRowIdent   44      �4      �4  
  �       CHARACTER,INPUT pcRowIdent CHARACTER,INPUT pcViewColList CHARACTER  findRow �4      5      ,5    �       LOGICAL,INPUT pcKeyValues CHARACTER findRowWhere    5      P5      �5    �       LOGICAL,INPUT pcColumns CHARACTER,INPUT pcValues CHARACTER,INPUT pcOperators CHARACTER  firstRowIds `5      �5      6    �       CHARACTER,INPUT pcQueryString CHARACTER getLastCommitErrorType  �5      ,6      d6    �       CHARACTER,  hasForeignKeyChanged    D6      p6      �6    �       LOGICAL,    openDataQuery   �6      �6      �6          LOGICAL,INPUT pcPosition CHARACTER  openQuery   �6      7      47   	        LOGICAL,    prepareQuery    7      @7      p7    *      LOGICAL,INPUT pcQuery CHARACTER rowAvailable    P7      �7      �7    7      LOGICAL,INPUT pcDirection CHARACTER rowValues   �7      �7      8   	 D      CHARACTER,INPUT pcColumns CHARACTER,INPUT pcFormat CHARACTER,INPUT pcDelimiter CHARACTER    submitRow   �7      l8      �8   	 N      LOGICAL,INPUT pcRowIdent CHARACTER,INPUT pcValueList CHARACTER  updateRow   x8      �8      9   	 X      LOGICAL,INPUT pcKeyValues CHARACTER,INPUT pcValueList CHARACTER getObjectType   �8      D9      t9    b      CHARACTER,  assignDBRow                             :  �9      ��                      :              �^f        O   ����    e�          O   ����    R�          O   ����    ��            �� 
                 4:  
         ��                            ����                            bufferCopyDBToRO                                (;  ;      ��                    
  @;              @kf        O   ����    e�          O   ����    R�          O   ����    ��            �� 
  �;             X;  
             �� 
  �;             �;  
             ��   �;             �;               ��                  �;           ��                            ����                            compareDBRow                                �<  �<      ��                      �<              �tf        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            confirmContinue                             �=  �=      ��                      �=              0wf        O   ����    e�          O   ����    R�          O   ����    ��            ��                  �=           ��                            ����                            dataAvailable                               �>  �>      ��                      �>              �{f        O   ����    e�          O   ����    R�          O   ����    ��            ��                  ?           ��                            ����                            fetchDBRowForUpdate                             �?  �?      ��                      @              �f        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            fetchFirst                              �@  �@      ��                      A              ��f        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            fetchLast                               �A  �A      ��                      �A              ̆f        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            fetchNext                               �B  �B      ��                     !  �B              8�f        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            fetchPrev                               �C  �C      ��                  #  $  �C              �f        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            filterContainerHandler                              �D  �D      ��                  &  (  �D              �/j        O   ����    e�          O   ����    R�          O   ����    ��            �� 
                 �D  
         ��                            ����                            initializeObject                                �E  �E      ��                  *  +  F              d4j        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            refetchDBRow                                �F  �F      ��                  -  /   G              �7j        O   ����    e�          O   ����    R�          O   ����    ��            �� 
                 G  
         ��                            ����                            releaseDBRow                                H  �G      ��                  1  2   H              <j        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            startFilter                             �H  �H      ��                  4  5  I              �<j        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            transferDBRow                               �I  �I      ��                  7  :  J              �?j        O   ����    e�          O   ����    R�          O   ����    ��            ��   XJ             $J               ��                  LJ           ��                            ����                            addQueryWhere   T9      �J      �J    �      LOGICAL,INPUT pcWhere CHARACTER,INPUT pcBuffer CHARACTER,INPUT pcAndOr CHARACTER    assignQuerySelection    �J      8K      pK    �      LOGICAL,INPUT pcColumns CHARACTER,INPUT pcValues CHARACTER,INPUT pcOperators CHARACTER  bufferCompareDBToRO PK      �K      �K    �      LOGICAL,INPUT phRowObjUpd HANDLE,INPUT phBuffer HANDLE,INPUT pcExcludes CHARACTER,INPUT pcAssigns CHARACTER bufferWhereClause   �K      hL      �L    �      CHARACTER,INPUT pcBuffer CHARACTER,INPUT pcWhere CHARACTER  columnDataType  |L      �L      M    �      CHARACTER,INPUT pcColumn CHARACTER  columnDbColumn  �L      ,M      \M    �      CHARACTER,INPUT pcColumn CHARACTER  columnQuerySelection    <M      �M      �M    �      CHARACTER,INPUT pcColumn CHARACTER  columnTable �M      �M      N    �      CHARACTER,INPUT pcColumn CHARACTER  columnValMsg    �M      ,N      \N           CHARACTER,INPUT pcColumn CHARACTER  dbColumnDataName    <N      �N      �N  !        CHARACTER,INPUT pcDbColumn CHARACTER    dbColumnHandle  �N      �N      O  "  )      HANDLE,INPUT pcColumn CHARACTER excludeColumns  �N      ,O      \O  #  8      CHARACTER,INPUT iTable INTEGER  getDataColumns  <O      |O      �O  $  G      CHARACTER,  getForeignValues    �O      �O      �O  %  V      CHARACTER,  getQueryPosition    �O      �O      ,P  &  g      CHARACTER,  getQuerySort    P      8P      hP  '  x      CHARACTER,  getQueryString  HP      tP      �P  (  �      CHARACTER,  getQueryWhere   �P      �P      �P  )  �      CHARACTER,  getTargetProcedure  �P      �P       Q  *  �      HANDLE, indexInformation     Q      (Q      \Q  +  �      CHARACTER,INPUT pcQuery CHARACTER,INPUT plUseTableSep LOGICAL,INPUT pcIndexInfo CHARACTER   insertExpression    <Q      �Q      �Q  ,  �      CHARACTER,INPUT pcWhere CHARACTER,INPUT pcExpression CHARACTER,INPUT pcAndOr CHARACTER  newQueryString  �Q      DR      tR  -  �      CHARACTER,INPUT pcColumns CHARACTER,INPUT pcValues CHARACTER,INPUT pcOperators CHARACTER,INPUT pcQueryString CHARACTER,INPUT pcAndOr CHARACTER  newQueryValidate    TR      S      8S  .  �      CHARACTER,INPUT pcQueryString CHARACTER,INPUT pcExpression CHARACTER,INPUT pcBuffer CHARACTER,INPUT pcAndOr CHARACTER   newQueryWhere   S      �S      �S  /  �      CHARACTER,INPUT pcWhere CHARACTER   newWhereClause  �S      T      4T  0        CHARACTER,INPUT pcBuffer CHARACTER,INPUT pcExpression CHARACTER,INPUT pcWhere CHARACTER,INPUT pcAndOr CHARACTER refreshRowident T      �T      �T  1        CHARACTER,INPUT pcRowident CHARACTER    removeForeignKey    �T      �T      0U  2  $      LOGICAL,    removeQuerySelection    U      <U      tU  3  5      LOGICAL,INPUT pcColumns CHARACTER,INPUT pcOperators CHARACTER   resolveBuffer   TU      �U      �U  4  J      CHARACTER,INPUT pcBuffer CHARACTER  rowidWhere  �U      V      4V  5 
 X      CHARACTER,INPUT pcWhere CHARACTER   rowidWhereCols  V      XV      �V  6  c      CHARACTER,INPUT pcColumns CHARACTER,INPUT pcValues CHARACTER,INPUT pcOperators CHARACTER    setQueryPosition    hV      �V      W  7  r      LOGICAL,INPUT pcPosition CHARACTER  setQuerySort    �V      <W      lW  8  �      LOGICAL,INPUT pcSort CHARACTER  setQueryString  LW      �W      �W  9  �      LOGICAL,INPUT pcQueryString CHARACTER   setQueryWhere   �W      �W      X  :  �      LOGICAL,INPUT pcWhere CHARACTER whereClauseBuffer   �W      4X      hX  ;  �      CHARACTER,INPUT pcWhere CHARACTER   bindServer                              Y  �X      ��                  �  �  (Y              �        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            destroyObject                               Z  �Y      ��                  �  �   Z              ��        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            destroyServerObject                             [  �Z      ��                  �  �  [              �        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            disconnectObject                                 \  �[      ��                  �  �  \              �        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeServerObject                               ]  �\      ��                  �  �  ]              �
        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            restartServerObject                             �]  �]      ��                  �  �  ^              �        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            runServerObject                             �^  �^      ��                  �  �  _              �        O   ����    e�          O   ����    R�          O   ����    ��            �� 
                 $_  
         ��                            ����                            startServerObject                               `   `      ��                  �  �  0`              d        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            unbindServer                                a  �`      ��                  �  �  (a              �        O   ����    e�          O   ����    R�          O   ����    ��            ��                  @a           ��                            ����                            getAppService   HX      �a      �a  <  �      CHARACTER,  getASBound  �a      �a      b  = 
 �      LOGICAL,    getAsDivision   �a      b      Lb  >  �      CHARACTER,  getASHandle ,b      Xb      �b  ?  �      HANDLE, getASHasStarted db      �b      �b  @  �      LOGICAL,    getASInfo   �b      �b      �b  A 	       CHARACTER,  getASInitializeOnRun    �b       c      8c  B        LOGICAL,    getASUsePrompt  c      Dc      tc  C  !      LOGICAL,    getServerFileName   Tc      �c      �c  D  0      CHARACTER,  getServerOperatingMode  �c      �c      �c  E  B      CHARACTER,  runServerProcedure  �c      d      8d  F  Y      HANDLE,INPUT pcServerFileName CHARACTER,INPUT phAppService HANDLE   setAppService   d      |d      �d  G  l      LOGICAL,INPUT pcAppService CHARACTER    setASDivision   �d      �d      e  H  z      LOGICAL,INPUT pcDivision CHARACTER  setASHandle �d      (e      Te  I  �      LOGICAL,INPUT phASHandle HANDLE setASInfo   4e      te      �e  J 	 �      LOGICAL,INPUT pcInfo CHARACTER  setASInitializeOnRun    �e      �e      �e  K  �      LOGICAL,INPUT plInitialize LOGICAL  setASUsePrompt  �e      f      Lf  L  �      LOGICAL,INPUT plFlag LOGICAL    setServerFileName   ,f      lf      �f  M  �      LOGICAL,INPUT pcFileName CHARACTER  setServerOperatingMode  �f      �f      �f  N  �      LOGICAL,INPUT pcServerOperatingMode CHARACTER   addLink                             �g  �g      ��                  �  �  �g              ��j        O   ����    e�          O   ����    R�          O   ����    ��            �� 
  h             �g  
             ��   8h             h               �� 
                 ,h  
         ��                            ����                            addMessage                              i   i      ��                  �  �  0i              ��j        O   ����    e�          O   ����    R�          O   ����    ��            ��   |i             Hi               ��   �i             pi               ��                  �i           ��                            ����                            adjustTabOrder                              �j  pj      ��                  �  �  �j              ��j        O   ����    e�          O   ����    R�          O   ����    ��            �� 
  �j             �j  
             �� 
  k             �j  
             ��                  k           ��                            ����                            applyEntry                              �k  �k      ��                  �  �  l              ��j        O   ����    e�          O   ����    R�          O   ����    ��            ��                  $l           ��                            ����                            changeCursor                                m  �l      ��                  �  �  ,m              ,�j        O   ����    e�          O   ����    R�          O   ����    ��            ��                  Dm           ��                            ����                            createControls                              4n  n      ��                  �  �  Ln              H�j        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            destroyObject                               ,o  o      ��                  �  �  Do              $�j        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            displayLinks                                $p  p      ��                  �  �  <p              4�j        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            editInstanceProperties                              $q  q      ��                  �  �  <q              P�        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            exitObject                              r   r      ��                  �  �  0r              ��        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            hideObject                              s  �r      ��                  �  �  $s              ���        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeObject                                t  �s      ��                  �  �   t              |��        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            modifyListProperty                              u  �t      ��                  �  �  u              ���        O   ����    e�          O   ����    R�          O   ����    ��            �� 
  hu             4u  
             ��   �u             \u               ��   �u             �u               ��                  �u           ��                            ����                            modifyUserLinks                             �v  �v      ��                  �  �  �v              (�j        O   ����    e�          O   ����    R�          O   ����    ��            ��    w             �v               ��   (w             �v               �� 
                 w  
         ��                            ����                            removeAllLinks                              x  �w      ��                  �  �  $x              |�j        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            removeLink                               y  �x      ��                  �  �  y              �j        O   ����    e�          O   ����    R�          O   ����    ��            �� 
  dy             0y  
             ��   �y             Xy               �� 
                 �y  
         ��                            ����                            repositionObject                                tz  \z      ��                  �  �  �z              ,k        O   ����    e�          O   ����    R�          O   ����    ��            ��   �z             �z               ��                  �z           ��                            ����                            returnFocus                             �{  �{      ��                  �  �  �{              �k        O   ����    e�          O   ����    R�          O   ����    ��            �� 
                 �{  
         ��                            ����                            showMessageProcedure                                �|  �|      ��                       �|              k        O   ����    e�          O   ����    R�          O   ����    ��            ��   D}             }               ��                  8}           ��                            ����                            toggleData                              $~  ~      ��                      <~              ,k        O   ����    e�          O   ����    R�          O   ����    ��            ��                  T~           ��                            ����                            viewObject                              @  (      ��                  	  
  X              \k        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            anyMessage  �f      �      �  O 
 9      LOGICAL,    assignLinkProperty  �      �      �  P  D      LOGICAL,INPUT pcLink CHARACTER,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER   fetchMessages   �      t�      ��  Q  W      CHARACTER,  getChildDataKey ��      ��      ��  R  e      CHARACTER,  getContainerHandle  ��      �       �  S  u      HANDLE, getContainerHidden   �      (�      \�  T  �      LOGICAL,    getContainerSource  <�      h�      ��  U  �      HANDLE, getContainerSourceEvents    |�      ��      ��  V  �      CHARACTER,  getContainerType    ��      �       �  W  �      CHARACTER,  getDataLinksEnabled  �      ,�      `�  X  �      LOGICAL,    getDataSource   @�      l�      ��  Y  �      HANDLE, getDataSourceEvents |�      ��      ؂  Z  �      CHARACTER,  getDataSourceNames  ��      �      �  [        CHARACTER,  getDataTarget   ��      $�      T�  \  !      CHARACTER,  getDataTargetEvents 4�      `�      ��  ]  /      CHARACTER,  getDBAware  t�      ��      ̃  ^ 
 C      LOGICAL,    getDesignDataObject ��      ؃      �  _  N      CHARACTER,  getDynamicObject    �      �      L�  `  b      LOGICAL,    getInstanceProperties   ,�      X�      ��  a  s      CHARACTER,  getLogicalObjectName    p�      ��      Ԅ  b  �      CHARACTER,  getLogicalVersion   ��      ��      �  c  �      CHARACTER,  getObjectHidden �       �      P�  d  �      LOGICAL,    getObjectInitialized    0�      \�      ��  e  �      LOGICAL,    getObjectName   t�      ��      Ѕ  f  �      CHARACTER,  getObjectPage   ��      ܅      �  g  �      INTEGER,    getObjectParent �      �      H�  h  �      HANDLE, getObjectVersion    (�      P�      ��  i        CHARACTER,  getObjectVersionNumber  d�      ��      Ȇ  j        CHARACTER,  getParentDataKey    ��      Ԇ      �  k  )      CHARACTER,  getPassThroughLinks �      �      H�  l  :      CHARACTER,  getPhysicalObjectName   (�      T�      ��  m  N      CHARACTER,  getPhysicalVersion  l�      ��      ̇  n  d      CHARACTER,  getPropertyDialog   ��      ؇      �  o  w      CHARACTER,  getQueryObject  �      �      H�  p  �      LOGICAL,    getRunAttribute (�      T�      ��  q  �      CHARACTER,  getSupportedLinks   d�      ��      Ĉ  r  �      CHARACTER,  getTranslatableProperties   ��      Ј      �  s  �      CHARACTER,  getUIBMode  �      �      D�  t 
 �      CHARACTER,  getUserProperty $�      P�      ��  u  �      CHARACTER,INPUT pcPropName CHARACTER    instancePropertyList    `�      ��      ��  v  �      CHARACTER,INPUT pcPropList CHARACTER    linkHandles ��      �      4�  w  	      CHARACTER,INPUT pcLink CHARACTER    linkProperty    �      X�      ��  x  	      CHARACTER,INPUT pcLink CHARACTER,INPUT pcPropName CHARACTER mappedEntry h�      Ċ      ��  y  	      CHARACTER,INPUT pcEntry CHARACTER,INPUT pcList CHARACTER,INPUT plFirst LOGICAL,INPUT pcDelimiter CHARACTER  messageNumber   Њ      \�      ��  z  )	      CHARACTER,INPUT piMessage INTEGER   propertyType    l�      ��      ��  {  7	      CHARACTER,INPUT pcPropName CHARACTER    reviewMessages  ��      �      8�  |  D	      CHARACTER,  setChildDataKey �      D�      t�  }  S	      LOGICAL,INPUT cChildDataKey CHARACTER   setContainerHidden  T�      ��      Ќ  ~  c	      LOGICAL,INPUT plHidden LOGICAL  setContainerSource  ��      ��      $�    v	      LOGICAL,INPUT phObject HANDLE   setContainerSourceEvents    �      D�      ��  �  �	      LOGICAL,INPUT pcEvents CHARACTER    setDataLinksEnabled `�      ��      ؍  �  �	      LOGICAL,INPUT lDataLinksEnabled LOGICAL setDataSource   ��       �      0�  �  �	      LOGICAL,INPUT phObject HANDLE   setDataSourceEvents �      P�      ��  �  �	      LOGICAL,INPUT pcEventsList CHARACTER    setDataSourceNames  d�      ��      ��  �  �	      LOGICAL,INPUT pcSourceNames CHARACTER   setDataTarget   ��      �      8�  �  �	      LOGICAL,INPUT pcTarget CHARACTER    setDataTargetEvents �      \�      ��  �  �	      LOGICAL,INPUT pcEvents CHARACTER    setDBAware  p�      ��      ��  � 
 
      LOGICAL,INPUT lAware LOGICAL    setDesignDataObject ��       �      4�  �  
      LOGICAL,INPUT pcDataObject CHARACTER    setDynamicObject    �      \�      ��  �  ,
      LOGICAL,INPUT lTemp LOGICAL setInstanceProperties   p�      ��      �  �  =
      LOGICAL,INPUT pcPropList CHARACTER  setLogicalObjectName    Đ      �      @�  �  S
      LOGICAL,INPUT c CHARACTER   setLogicalVersion    �      \�      ��  �  h
      LOGICAL,INPUT cVersion CHARACTER    setObjectName   p�      ��      �  �  z
      LOGICAL,INPUT pcName CHARACTER  setObjectParent đ      �      4�  �  �
      LOGICAL,INPUT phParent HANDLE   setObjectVersion    �      T�      ��  �  �
      LOGICAL,INPUT cObjectVersion CHARACTER  setParentDataKey    h�      ��      �  �  �
      LOGICAL,INPUT cParentDataKey CHARACTER  setPassThroughLinks Ē      �      @�  �  �
      LOGICAL,INPUT pcLinks CHARACTER setPhysicalObjectName    �      `�      ��  �  �
      LOGICAL,INPUT cTemp CHARACTER   setPhysicalVersion  x�      ��      �  �  �
      LOGICAL,INPUT cVersion CHARACTER    setRunAttribute ̓      �      @�  �  �
      LOGICAL,INPUT cRunAttribute CHARACTER   setSupportedLinks    �      h�      ��  �        LOGICAL,INPUT pcLinkList CHARACTER  setTranslatableProperties   |�      ��      ��  �        LOGICAL,INPUT pcPropList CHARACTER  setUIBMode  ܔ       �      L�  � 
 3      LOGICAL,INPUT pcMode CHARACTER  setUserProperty ,�      l�      ��  �  >      LOGICAL,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER  showmessage |�      ܕ      �  �  N      LOGICAL,INPUT pcMessage CHARACTER   Signature   �      ,�      X�  � 	 Z      CHARACTER,INPUT pcName CHARACTER     �        ��  �          4   �����                 �                      ��                  !  N                  D$k           !  ��         "  ,�  ��          4   �����                 ��                      ��                  #  M                  �$k           #  <�  ��     :  ė  4�          4   �����                 D�                      ��                  F  H                  D,k           F  ԗ         G                                  ,     
                    � ߱        Ș  $   J  p�  ���                           $   L  ��  ���                       x                         � ߱        �     R  8�  ��          4   �����                ��                      ��                  S  	                  �,k           S  H�  �  o   V      ,                                 D�  $   W  �  ���                       �  @         �              � ߱        X�  �   X        l�  �   Y  �      ��  �   [        ��  �   ]  x      ��  �   _  �      ��  �   a  `      К  �   b  �      �  �   c        ��  �   f  �      �  �   h          �  �   i  |      4�  �   k  �      H�  �   l  t      \�  �   m  �      p�  �   n  ,      ��  �   o  �      ��  �   u  �      ��  �   w  P	      ��  �   }  �	      ԛ  �      
      �  �   �  t
      ��  �   �  �
      �  �   �  l      $�  �   �  �      8�  �   �  \      L�  �   �  �      `�  �   �  D      t�  �   �  �      ��  �   �  �      ��  �   �  0      ��  �   �  �      Ĝ  �   �  �      ؜  �   �        �  �   �  X       �  �   �  �      �  �   �        (�  �   �  L      <�  �   �  �      P�  �   �  �      d�  �   �         x�  �   �  <      ��  �   �  x      ��  �   �  �      ��  �   �  �          �   �  ,                      Ԟ          @�  (�      ��                  >	  l	  X�              �Xk        O   ����    e�          O   ����    R�          O   ����    ��      �     
                                     (                         � ߱         �  $  R	  p�  ���                           O   j	  ��  ��  h               l�          \�  d�    L�                                             ��                            ����                                D9      ȝ      �     V     t�                       p�  b                     ��     �	  (�  ��          4   ����t                ��                      ��                  �	  
                  �Uk           �	  8�  ��  �   �	  �      Р  �   �	  H      �  �   �	  �      ��  �   �	  @      �  �   �	  �       �  �   �	  8      4�  �   �	  �      H�  �   �	  (      \�  �   �	  �      p�  �   �	         ��  �   �	  �      ��  �   �	        ��  �   �	  �          �   �	        �     C
  ء  H�          4   ����x                X�                      ��                  D
  �
                  L9           D
  �  l�  �   F
  �      ��  �   G
  T      ��  �   H
  �      ��  �   I
  D      ��  �   J
  �      Т  �   K
  �      �  �   M
  p      ��  �   N
  �      �  �   O
  X       �  �   P
  �      4�  �   Q
  �      H�  �   R
  D       \�  �   S
  �       p�  �   T
  �       ��  �   U
  x!      ��  �   V
  �!      ��  �   W
  h"      ��  �   X
  �"      ԣ  �   Y
  `#      �  �   Z
  �#      ��  �   [
  X$      �  �   \
  �$      $�  �   ]
  �$      8�  �   ^
  L%      L�  �   _
  �%      `�  �   `
  <&      t�  �   a
  �&      ��  �   b
  4'      ��  �   c
  �'      ��  �   d
  ,(      Ĥ  �   e
  h(      ؤ  �   g
  �(      �  �   h
  X)       �  �   i
  �)      �  �   j
  *      (�  �   k
  �*      <�  �   l
  �*      P�  �   m
  l+      d�  �   n
  �+      x�  �   o
  \,      ��  �   p
  �,      ��  �   q
  L-      ��  �   r
  �-      ȥ  �   s
  <.      ܥ  �   t
  �.      �  �   u
  4/      �  �   v
  �/          �   w
  $0      ̨     �
  0�  ��          4   ����T0                ��                      ��                  �
  �                  ���           �
  @�  Ħ  �   �
  �0      ئ  �   �
  (1      �  �   �
  �1       �  �   �
  2      �  �   �
  �2      (�  �   �
  3      <�  �   �
  |3      P�  �   �
  �3      d�  �   �
  t4      x�  �   �
  �4      ��  �   �
  l5      ��  �      �5      ��  �     d6      ȧ  �     �6      ܧ  �     L7      �  �     �7      �  �     <8      �  �     �8      ,�  �     ,9      @�  �     �9      T�  �   	  :      h�  �   
  X:      |�  �     �:      ��  �     H;      ��  �     �;      ��  �     8<          �     �<      Ы     �  �  T�          4   ����=  	              d�                      ��             	     �  /                  T`f           �  ��  x�  �   �  |=      ��  �   �  �=      ��  �   �  t>      ��  �   �  �>      ȩ  �   �  l?      ܩ  �   �  �?      �  �   �  \@      �  �   �  �@      �  �   �  TA      ,�  �   �  �A      @�  �   �  DB      T�  �   �  �B      h�  �   �  <C      |�  �   �  �C      ��  �   �  ,D      ��  �   �  �D      ��  �   �  $E      ̪  �   �  �E      �  �   �  F      ��  �   �  �F      �  �   �  G      �  �   �  �G      0�  �   �  �G      D�  �   �  8H      X�  �   �  �H      l�  �   �  0I      ��  �   �  �I      ��  �   �   J          �   �  �J      getRowObjUpdStatic  deleteRecordStatic  d�     �  �  ��          4   ����K      /   �  $�     4�                          3   ����K            T�                      3   ����<K  �     �  |�  �  <�      4   ����XK  
              ��                      ��             
     �  Q                  �7�           �  ��  �  �   �  �K      h�  $   �  <�  ���                       �K     
                    � ߱        |�  �   �  L      ԭ  $   �  ��  ���                       ,L  @         L              � ߱        ��  $   �   �  ���                       �L       	       	           � ߱        �M     
                N                     \O  @        
 O              � ߱         �  V     ,�  ���                        hO       	       	       �O       
       
       �O       	       	           � ߱        ��  $      ��  ���                       �P     
                Q                     dR  @        
 $R              � ߱            V   2  L�  ���                                      �                      ��                  S  �                  H9�           S  ܯ  pR     
                �R                     <T  @        
 �S          �T  @        
 dT          U  @        
 �T          dU  @        
 $U              � ߱            V   h  L�  ���                        adm-clone-props ��  0�              �     W     4                          0  U                     start-super-proc    @�  ��  �           �     X                                  v                     ��       $�  4�          4   �����X      /   	  `�     p�                          3   ���� Y            ��                      3   ���� Y  ��  $   #  ̲  ���                       @Y                         � ߱        ̴     3  �  ��   �      4   ����\Y                ��                      ��                  4  8                  l5�           4   �  pY                     �Y                     �Y                         � ߱            $   5  ��  ���                              9  8�  t�          4   �����Y  �Y                         � ߱            $   :  H�  ���                       �Y                         � ߱        ��  $   >  ��  ���                       �     A  �   �  x�      4   �����Y      $   B  L�  ���                       Z                         � ߱            �   _  ,Z      lZ     
                �Z                     8\  @        
 �[              � ߱        �  V   s  ��  ���                        0�  �   �  D\      (�     %  H�  X�          4   �����\      /   &  ��     ��                          3   �����\            ��                      3   �����\  �\     
                P]                     �^  @        
 `^              � ߱        ��  V   2  Ķ  ���                        �^     
                h_                     �`  @        
 x`              � ߱        �  V   V  T�  ���                        T�     �  ��  l�          4   �����`                |�                      ��                  �  �                  <-�           �  �  �  /   �  ��     ��                          3   �����`            ظ                      3   �����`      /   �  �     $�                          3   ����a            D�                      3   ����8a  �  /  C  ��         la                      3   ����Ta  initProps   ��  ��              D     Y     <                          8  �  	                                   Ⱥ          p�  X�      ��                 �    ��              ��m        O   ����    e�          O   ����    R�          O   ����    ��      �                      ��          ��  p     �r  �        d�  T�     �r                                        ��                                       ���             ��  �  Ի     �r                                        ��                  !  =                  (��           !  t�  d�  T�     s                                        ��                  >  Z                  ���           >  ��  �  Լ     s                                        ��                  [  w                  ���           [  t�  d�  T�     0s                                        ��                  x  �                  ���           x  ��  �  Խ     Ds                                        ��                  �  �                  ���           �  t�  d�  T�     Xs                                        ��                  �  �                  l��           �  ��  �  Ծ     ls                                        ��                  �  �                  <��           �  t�  d�  T�     �s  	                                      ��             	     �                    ��           �  ��  �  Կ     �s  
                                      ��             
     	  %                  ��           	  t�  d�  T�     �s                                        ��                  &  B                  ���           &  ��  ��  ��     �s                                        ��                  C  _                  ���           C  t�  d�  T�     �s                                        ��                  `  |                  ���           `  ��  ��  ��     �s                                        ��                  }  �                  �~e           }  t�  d�  T�     �s                                        ��                  �  �                  De           �  ��  ��  ��     t                                        ��                  �  �                  �e           �  t�  d�  T�      t                                        ��                  �  �                  �e           �  ��      ��     4t                                        ��                  �                    ��e           �  t�      O     ��  ��  Ht               h�          P�  \�   , 0�                                                       �     ��                            ����                            ��  ��  ��  ��      ��     Z     p�                      � l�  �                     ��     &  $�  ��          4   ����Tt                ��                      ��                  '  ;                  4�e           '  4�  �  /   (  ��     ��                          3   ����dt             �                      3   �����t  |�  /   )  <�     L�                          3   �����t            l�                      3   �����t  ��  /   .  ��     ��                          3   �����t            ��                      3   �����t      /   4  �     $�                          3   ����u            D�                      3   ����8u  Xu     
                �u                     $w  @        
 �v              � ߱        ��  V   �  T�  ���                        ��  $   �  �  ���                       8w                         � ߱        Pw     
                �w                     y  @        
 �x              � ߱        ��  V   �  <�  ���                        ��  $   �  ��  ���                       (y     
                    � ߱        <y     
                �y                     {  @        
 �z              � ߱        ��  V   �  $�  ���                        p�  $   �  ��  ���                       {     
                    � ߱        ({     
                �{                     �|  @        
 �|              � ߱        ��  V   �  �  ���                        X�  $     ��  ���                       }                         � ߱        4}     
                �}                        @        
 �~              � ߱        ��  V     ��  ���                        ��  �   3        T�  $   4  ��  ���                       8     
                    � ߱        L     
                �                     �  @        
 ؀              � ߱        ��  V   >  ��  ���                        ��  $   X  ��  ���                       $�     
                    � ߱        ��  �   r  8�      D�  $   |  �  ���                       x�     
                    � ߱        X�  �   �  ��      ��  $   �  ��  ���                       ́                         � ߱               �  ��  ��          4   �����      /   �  �     �                          3   �����  D�     
   4�                      3   ����(�  t�        d�                      3   ����0�  ��        ��                      3   ����D�            ��                      3   ����`�  pushRowObjUpdTable  ��  ��  �                   [      �                               �                     pushTableAndValidate    ��  D�  �           p     \     �                          �                       remoteCommit    \�  ��  �           d     ]     �                          �  b                     serverCommit    ��  $�  �           `     ^     �                          �  o                                     8�          �  ��      ��                  �  �   �              ��e        O   ����    e�          O   ����    R�          O   ����    ��          O   �  ��  ��  ��    ��                            ����                            4�  ��      ��              _      P�                      
�     |                     disable_UI  ��  ��                      `      �                               �  
                    �  �    ����  �       ��       ��  8   ����   ��  8   ����       8   ����       8   ����       ��  ��      viewObject  ,   ��  ��  ��      toggleData  ,INPUT plEnabled LOGICAL    ��   �  8�      showMessageProcedure    ,INPUT pcMessage CHARACTER,OUTPUT plAnswer LOGICAL  �  |�  ��      returnFocus ,INPUT hTarget HANDLE   l�  ��  ��      repositionObject    ,INPUT pdRow DECIMAL,INPUT pdCol DECIMAL    ��   �  �      removeLink  ,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE ��  `�  p�      removeAllLinks  ,   P�  ��  ��      modifyUserLinks ,INPUT pcMod CHARACTER,INPUT pcLinkName CHARACTER,INPUT phObject HANDLE t�  ��   �      modifyListProperty  ,INPUT phCaller HANDLE,INPUT pcMode CHARACTER,INPUT pcListName CHARACTER,INPUT pcListValue CHARACTER    ��  x�  ��      hideObject  ,   h�  ��  ��      exitObject  ,   ��  ��  ��      editInstanceProperties  ,   ��  ��  ��      displayLinks    ,   ��  �  �      createControls  ,   ��  ,�  <�      changeCursor    ,INPUT pcCursor CHARACTER   �  h�  t�      applyEntry  ,INPUT pcField CHARACTER    X�  ��  ��      adjustTabOrder  ,INPUT phObject HANDLE,INPUT phAnchor HANDLE,INPUT pcPosition CHARACTER ��  �  �      addMessage  ,INPUT pcText CHARACTER,INPUT pcField CHARACTER,INPUT pcTable CHARACTER ��  l�  t�      addLink ,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE \�  ��  ��      unbindServer    ,INPUT pcMode CHARACTER ��   �  �      runServerObject ,INPUT phAppService HANDLE  ��  <�  P�      disconnectObject    ,   ,�  d�  t�      destroyObject   ,   T�  ��  ��      bindServer  ,   x�  ��  ��      transferDBRow   ,INPUT pcRowIdent CHARACTER,INPUT piRowNum INTEGER  ��  ��  �      startFilter ,   ��  �  ,�      releaseDBRow    ,   �  @�  P�      refetchDBRow    ,INPUT phRowObjUpd HANDLE   0�  |�  ��      filterContainerHandler  ,INPUT phFilterContainer HANDLE l�  ��  ��      fetchDBRowForUpdate ,   ��  ��  ��      confirmContinue ,INPUT-OUTPUT pioCancel LOGICAL ��  ,�  <�      compareDBRow    ,   �  P�  d�      bufferCopyDBToRO    ,INPUT phRowObj HANDLE,INPUT phBuffer HANDLE,INPUT pcExcludes CHARACTER,INPUT pcAssigns CHARACTER   @�  ��  ��      assignDBRow ,INPUT phRowObjUpd HANDLE   ��  �  �      updateState ,INPUT pcState CHARACTER     �  H�  \�      updateQueryPosition ,   8�  p�  ��      updateAddQueryWhere ,INPUT pcWhere CHARACTER,INPUT pcField CHARACTER    `�  ��  ��      undoTransaction ,   ��  ��  ��      transferToExcel ,INPUT pcFieldList CHARACTER,INPUT plIncludeObj LOGICAL,INPUT plUseExisting LOGICAL,INPUT piMaxRecords INTEGER  ��  |�  ��      synchronizeProperties   ,INPUT pcPropertiesForServer CHARACTER,OUTPUT pcPropertiesForClient CHARACTER   l�  ��  �      submitValidation    ,INPUT pcValueList CHARACTER,INPUT pcUpdColumns CHARACTER   ��  T�  h�      submitForeignKey    ,INPUT pcRowIdent CHARACTER,INPUT-OUTPUT pcValueList CHARACTER,INPUT-OUTPUT pcUpdColumns CHARACTER  D�  ��  ��      submitCommit    ,INPUT pcRowIdent CHARACTER,INPUT plReopen LOGICAL  ��  0�  D�      startServerObject   ,    �  X�  h�      setPropertyList ,INPUT pcProperties CHARACTER   H�  ��  ��      serverFetchRowObjUpdTable   ,OUTPUT TABLE-HANDLE phRowObjUpd    ��  ��  ��      serverSendRows  ,INPUT piStartRow INTEGER,INPUT pcRowIdent CHARACTER,INPUT plNext LOGICAL,INPUT piRowsToReturn INTEGER,OUTPUT piRowsReturned INTEGER,OUTPUT TABLE-HANDLE phRowObject    ��  ��  ��      saveContextAndDestroy   ,OUTPUT pcContext CHARACTER ��  ��  �      rowObjectState  ,INPUT pcState CHARACTER    ��  0�  @�      retrieveFilter  ,    �  T�  h�      restartServerObject ,   D�  |�  ��      remoteSendRows  ,INPUT-OUTPUT piocContext CHARACTER,INPUT piStartRow INTEGER,INPUT pcRowIdent CHARACTER,INPUT plNext LOGICAL,INPUT piRowsToReturn INTEGER,OUTPUT pioRowsReturned INTEGER,OUTPUT TABLE-HANDLE phRowObject,OUTPUT pocMessages CHARACTER   l�  ��  ��      refreshRow  ,   t�  ��  ��      printToCrystal  ,INPUT pcFieldList CHARACTER,INPUT plIncludeObj LOGICAL,INPUT piMaxRecords INTEGER  ��  �  (�      isUpdatePending ,INPUT-OUTPUT plUpdate LOGICAL  �  X�  p�      initializeServerObject  ,   H�  ��  ��      initializeObject    ,   t�  ��  ��      home    ,   ��  ��  ��      genContextList  ,OUTPUT pcContext CHARACTER ��  �  �      fetchPrev   ,   ��  $�  0�      fetchNext   ,   �  D�  P�      fetchLast   ,   4�  d�  p�      fetchFirst  ,   T�  ��  ��      fetchBatch  ,INPUT plForwards LOGICAL   t�  ��  ��      endClientDataRequest    ,   ��  ��  ��      destroyServerObject ,   ��  �   �      describeSchema  ,INPUT pcSdoName CHARACTER,OUTPUT TABLE-HANDLE hTtSchema     �  l�  |�      dataAvailable   ,INPUT pcRelative CHARACTER \�  ��  ��      copyColumns ,INPUT pcViewColList CHARACTER,INPUT phDataQuery HANDLE ��  ��  �      commitTransaction   ,   ��  $�  4�      clientSendRows  ,INPUT piStartRow INTEGER,INPUT pcRowIdent CHARACTER,INPUT plNext LOGICAL,INPUT piRowsToReturn INTEGER,OUTPUT piRowsReturned INTEGER    �  ��  ��      batchServices   ,INPUT pcServices CHARACTER,OUTPUT pcValues CHARACTER        � 
"     
 h%     adecomm/as-utils.w  
"   
   �    }        �
"     
    �     }        ��    C   %               � 
"    
 � %              h �P  \         (          
�                          
�            � d   i
"    
 k
�H T   %              �     }        �GG %              � 
"   
   P �L 
�H T   %              �     }        �GG %              
"   
   �        �    7%               
"   
 �               1� t  
 �    � %               o%   o           � �    
"   
 �           �    1� �   �    � %               o%   o           � �   
"   
 �           �    1� �  
 �    � %               o%   o           � �   
"   
 �           l    1� �   �    � %               o%   o           � �    
"   
 �           �    1� �   �    � %               o%   o           � �   
"   
 �           T    1� �   � �   � %               o%   o           %               
"   
 � �          �    1� �   � �      
"   
 �               1�    �    � %               o%   o           � %  
"   
 �           �    1� '   �    � %               o%   o           � 6  S 
"   
 �           �    1� �   � �   � %               o%   o           %               
"   
 �           p    1� �   � �   � %               o%   o           %               
"   
 �           �    1� �   � �   � %               o%   o           %              
"   
 � �          h    1� �   � � �     
"   
 �           �    1� �  
 � �   � %               o%   o           %               
"   
 �                1� �   �    � %               o%   o           � �    
"   
 � �          �    1� �   � �      
"   
 �           �    1� �   �    � %               o%   o           �   t 
"   
 � �          D	    1� v  
 � �      
"   
 �           �	    1� �   �    � %               o%   o           � �  � 
"   
 �           �	    1�    �    � %               o%   o           � �    
"   
 �           h
    1� 6  
 � A   � %               o%   o           %               
"   
 k�           �
    1� E   k� �   � %               o%   o           %              
"   
 k�           `    1� M   k�    � %               o%   o           � �    k
"   
 k�           �    1� ^   k�    � %               o%   o           o%   o           
"   
 k�           P    1� n  
 k�    � %               o%   o           � �    k
"   
 k�           �    1� y   k� �  	 � %               o%   o           � �  / k
"   
 � �          8    1� �   � � �  	   
"   
 k�           t    1� �   k� �  	 � o%   o           o%   o           � �    k
"   
 � �          �    1� �   � � �  	   
"   
 k�           $    1� �   k� �  	 � o%   o           o%   o           � �    k
"   
 � �          �    1�    � � �     
"   
 � �          �    1�    � � �  	   
"   
 � �              1� #   � � �  	   
"   
 � �          L    1� 0   � � �  	   
"   
 k�           �    1� >   k� �   � o%   o           o%   o           %              
"   
 � �              1� O   � � �  	   
"   
 � �          @    1� ]  
 � � h     
"   
 � �          |    1� p   � � �  	   
"   
 � �          �    1�    � � �  	   
"   
 � �          �    1� �   � � �  	   
"   
 � �          0    1� �   � � �  	   
"   
 � �          l    1� �  	 � � �  	   
"   
 � �          �    1� �   � � �  	   
"   
 � �          �    1� �   � � �  	   
"   
 k�                1� �   k�    � %               o%   o           o%   o           
�H T   %              �     }        �GG %              
"   
   
"   
 �
"   
   
"   
 i(�  L ( l       �        �    �� �   � P   �        �    �@    
� @  , 
�            �� �     p�               �L
�    %              � 8          � $         �           
�    �       
"   
 �� @  , 
�           �� �  
 �p�               �L"      P �L 
�H T   %              �     }        �GG %              
"   
 ��           �    1� #  
 ��    � %               o%   o           � �    �
"   
 ��           <    1� .  
 ��    � %               o%   o           o%   o           
"   
 k�           �    1� 9   k�    � %               o%   o           o%   o           
"   
 k�           4    1� B   k� �   � %               o%   o           %               
"   
 k�           �    1� Q   k� �   � %               o%   o           %               
"   
 h�           ,    1� ^   h�    � %               o%   o           � �    k
"   
 k�           �    1� e   k� �   � %               o%   o           %              
"   
 k�               1� w   k� �   � %               o%   o           o%   o           
"   
 k�           �    1� �   k�    � %               o%   o           o%   o           
"   
 k�               1� �  	 k�    � %               o%   o           � �    k
"   
 k�           �    1� �   k�    � %               o%   o           o%   o           
"   
 k�               1� �   k�    � %               o%   o           o%   o           
"   
 k�           �    1� �   k� �   � %               o%   o           %               
"   
 k�           �    1� �   k� �   � %               o%   o           o%   o           P �L 
�H T   %              �     }        �GG %              
"   
 ��           �    1� �  
 �� �   � %               o%   o           %              
"   
 ��           H    1� �   ��    � %               o%   o           o%   o           
"   
 ��           �    1� �   ��    � %               o%   o           � �    k
"   
 ��           8    1� �   ��    � %               o%   o           o%   o           
"   
 � �          �    1�    � �      
"   
 k�           �    1�    k�    � %               o%   o           � +  ! k
"   
 k�           d    1� M   k�    � %               o%   o           � �    k
"   
 k�           �    1� Z   k�    � %               o%   o           � m   k
"   
 � �          L    1� |   � � �     
"   
 � �          �    1� �   � �      
"   
 k�           �    1� �   k�    � %               o%   o           � �    k
"   
 � �          8     1� �  
 � �      
"   
 h�           t     1� �   h� �   � %               o%   o           o%   o           
"   
 k�           �     1� �   k� �   � %               o%   o           %               
"   
 k�           l!    1� �   k� �   � %               o%   o           %               
"   
 k�           �!    1� �   k�    � %               o%   o           � �    k
"   
 k�           \"    1� �   k�    � %               o%   o           o%   o           
"   
 ��           �"    1�    �� �   � %               o%   o           %              
"   
 k�           T#    1�    k� �   � %               o%   o           %               
"   
 h�           �#    1�     h� �   � %               o%   o           %               
"   
 � �          L$    1� 0   � �      
"   
 � �          �$    1� =   � �      
"   
 k�           �$    1� J   k� A   � %               o%   o           o%   o           
"   
 k�           @%    1� V   k�    � %               o%   o           � �    k
"   
 k�           �%    1� d   k�    � %               o%   o           o%   o           
"   
 ��           0&    1� r   �� �   � o%   o           o%   o           o%   o           
"   
 ��           �&    1� �   �� �  	 � %               o%   o           o%   o           
"   
 ��           ('    1� �   ��    � %               o%   o           o%   o           
"   
 k�           �'    1� �  
 k� A   � %               o%   o           o%   o           
"   
 � �           (    1� �   � �      
"   
 k�           \(    1� �   k�    � %               o%   o           � �  4 k
"   
 k�           �(    1�   
 k� �   � %               o%   o           %              
"   
 � �          L)    1�    � �      
"   
 k�           �)    1� )   k�    � %               o%   o           � �    h
"   
 k�           �)    1� 7   k� �   � %               o%   o           %              
"   
 k�           x*    1� F   k�    � %               o%   o           � �    k
"   
 k�           �*    1� S   k�    � %               o%   o           � �    k
"   
 k�           `+    1� a   k�    � %               o%   o           � �    k
"   
 ��           �+    1� m   �� �   � %               o%   o           %               
"   
 ��           P,    1� |  	 ��    � %               o%   o           o%   o           
"   
 h�           �,    1� �   h�    � %               o%   o           � �  	 k
"   
 k�           @-    1� �   k� A   � %               o%   o           %       �       
"   
 k�           �-    1� �   k�    � %               o%   o           � �    k
"   
 k�           0.    1� �   k� �   � o%   o           o%   o           %              
"   
 k�           �.    1� �   k� �   � %               o%   o           %               
"   
 k�           (/    1� �   k�    � %               o%   o           o%   o           
"   
 ��           �/    1� �   �� �  	 � %               o%   o           � �    �
"   
 � �          0    1� �   � � �  	   P �L 
�H T   %              �     }        �GG %              
"   
 h�           �0    1� 
  
 h�    � %               o%   o           � �    h
"   
 k�           1    1�    k� �   � %               o%   o           %               
"   
 k�           �1    1� "  	 k�    � %               o%   o           � �    k
"   
 k�           2    1� ,   k�    � %               o%   o           � �    k
"   
 k�           �2    1� :   k� �   � %               o%   o           %               
"   
 k�           �2    1� J   k�    � %               o%   o           � �    k
"   
 k�           p3    1� ]   k�    � %               o%   o           o%   o           
"   
 ��           �3    1� e   ��    � %               o%   o           o%   o           
"   
 k�           h4    1� r   k� �   � %               o%   o           o%   o           
"   
 h�           �4    1� �   h� �   � %               o%   o           o%   o           
"   
 k�           `5    1� �   k� �   � %               o%   o           o%   o           
"   
 k�           �5    1� �   k�    � %               o%   o           o%   o           
"   
 k�           X6    1� �  	 k� �  	 � %               o%   o           � �    k
"   
 k�           �6    1� �  
 k� �  	 � %               o%   o           � �    k
"   
 ��           @7    1� �   ��    � %               o%   o           � �    k
"   
 ��           �7    1� �   ��    � %               o%   o           o%   o           
"   
 k�           08    1� �   k�    � %               o%   o           o%   o           
"   
 ��           �8    1� �   ��    � %               o%   o           � �    k
"   
 k�            9    1�    k�    � %               o%   o           � �    �
"   
 k�           �9    1�    k� �  	 � %               o%   o           o%   o           
"   
 � �          :    1� %   � �      
"   
 k�           L:    1� 1   k�    � %               o%   o           � �    k
"   
 k�           �:    1� ?   k�    � %               o%   o           o%   o           
"   
 h�           <;    1� R   h� �   � %               o%   o           o%   o           
"   
 k�           �;    1� d  
 k�    � %               o%   o           � �    k
"   
 k�           ,<    1� o   k�    � %               o%   o           � �    k
"   
 k�           �<    1� �   k� �   � %               o%   o           %               P �L 
�H T   %              �     }        �GG %              
"   
 k�           p=    1� �  	 k�    � %               o%   o           o%   o           
"   
 k�           �=    1� �   k�    � %               o%   o           o%   o           
"   
 ��           h>    1� �   ��    � %               o%   o           o%   o           
"   
 h�           �>    1� �   h� �   � %               o%   o           %              
"   
 k�           `?    1� �   k�    � %               o%   o           � �  M h
"   
 k�           �?    1� ;   k� �   � %               o%   o           %              
"   
 k�           P@    1� L   k� �   � %               o%   o           %               
"   
 k�           �@    1� `   k� �   � %               o%   o           %               
"   
 k�           HA    1� w   k� �  	 � %               o%   o           � �   k
"   
 ��           �A    1� �   �� �   � %               o%   o           %               
"   
 ��           8B    1� �   �� �  	 � %               o%   o           o%   o           
"   
 ��           �B    1� �   �� �   � o%   o           o%   o           %              
"   
 h�           0C    1� �   h� �  	 � o%   o           o%   o           � �    h
"   
 k�           �C    1� �   k�    � o%   o           o%   o           o%   o           
"   
 k�            D    1� �   k�    � o%   o           o%   o           o%   o           
"   
 k�           �D    1� �   k� �  	 � o%   o           o%   o           o%   o           
"   
 k�           E    1�     k�    � o%   o           o%   o           o%   o           
"   
 k�           �E    1�    k� �  	 � o%   o           o%   o           �    k
"   
 k�           F    1�    k� �  	 � o%   o           o%   o           � .   k
"   
 k�           |F    1� :   k� �   � %               o%   o           %               
"   
 k�           �F    1� N   k� �   � %               o%   o           %               
"   
 � �          tG    1� b   � � �  	   
"   
 k�           �G    1� v   k� �   � %               o%   o           %               
"   
 k�           ,H    1� �   k�    � %               o%   o           o%   o           
"   
 k�           �H    1� �   k�    � %               o%   o           o%   o           
"   
 ��           $I    1� �   �� �   � %               o%   o           o%   o           
"   
 k�           �I    1� �   k�    � %               o%   o           � �    k
"   
 ��           J    1� �   �� �   � %               o%   o           %               
"   
 k�           �J    1� �  	 k� �   � %               o%   o           %                "    � %     start-super-proc �� %     adm2/smart.p iP �L 
�H T   %              �     }        �GG %              
"   
   �       �K    6� �     
"   
   
�        �K    8
"   
   �        �K    ��     }        �G 4              
"   
 ߱G %              G %              %�   AppService,ASInfo,ASUsePrompt,CacheDuration,CheckCurrentChanged,DestroyStateless,DisconnectAppServer,ServerOperatingMode,ShareData,UpdateFromSource,ForeignFields,ObjectName,OpenOnInit,PromptColumns,PromptOnDelete,RowsToBatch,RebuildOnRepos,ToggleDataTargets  
�H T   %              �     }        �GG %              
"   
 i
"   
 � 
"   
 i
"   
   (�  L ( l       �        �M    �� �   � P   �        �M    �@    
� @  , 
�       �M    �� �   ip�               �L
�    %              � 8       N    � $         �           
�    �     i
"   
 �p� @  , 
�       O    ��    �p�               �L"  	  , �   �    k�    � �     }        �A      |    "  	    �    �%              (<   \ (    |    �     }        �A�    �A"  
  k    "  	  i"  
  k  < "  	  i"  
  k(    |    �     }        �A�    �A"  
  k
�H T   %              �     }        �GG %              
"   
 i
"   
 � 
"   
 i
"   
   (�  L ( l       �        �P    �� �   � P   �        �P    �@    
� @  , 
�       �P    �� �   ip�               �L
�    %              � 8      Q    � $         �           
�    �     i
"   
 �p� @  , 
�       R    �� t  
 �p�               �L"  	  , 
�H T   %              �     }        �GG %              
"   
   
"   
 m
"   
   
"   
   (�  L ( l       �        �R    �� �   � P   �        �R    �@    
� @  , 
�       �R    �� �     p�               �L
�    %              � 8      �R    � $         �           
�    �       
"   
 �p� @  , 
�       �S    �� �  
 �p�               �L%     SmartDataObject 
"   
   p� @  , 
�       XT    �� �     p�               �L%               
"   
  p� @  , 
�       �T    �� �    p�               �L%               
"   
  p� @  , 
�       U    �� �    p�               �L(        � �      � �      � �      �     }        �A
�H T   %              �     }        �GG %              
"   
 k (   � 
"   
 i    �        �U    �� �   �
"   
   � 8      DV    � $         �           
�    �     i
"   
   �        �V    �
"   
   �       �V    /
"   
   
"   
   �       �V    6� �     
"   
   
�        W    8
"   
   �        4W    �
"   
   �       TW    �
"   
   p�    � D   �
�    �     }        �G 4              
"   
 ߱G %              G %              
�     }        �
"   
    (   � 
"   
 i    �        X    �A"    �A
"   
   
�        dX    �@ � 
"   
 k"      �       }        �
"   
 � %              %                "    � %     start-super-proc �� %     adm2/appserver.p ���    � �     
�    �     }        �%               %      Server  - �     }        �    "    k� �    � %               %      Client      "    k� �    � %      NONE    p�,  8         $     "    k        � �   i
�    
�H T   %              �     }        �GG %              
"   
 i
"   
 � 
"   
 i
"   
   (�  L ( l       �        �Z    �� �   � P   �        �Z    �@    
� @  , 
�       �Z    �� �   ip�               �L
�    %              � 8      �Z    � $         �           
�    �     i
"   
 �p� @  , 
�       �[    �� �   �p�               �L"    , p�,  8         $     "    k        � �   i
�     "    � %     start-super-proc �� %     adm2/dataquery.p =k
�H T   %              �     }        �GG %              
"   
 i
"   
 � 
"   
 i
"   
 i(�  L ( l       �         ]    �� �   � P   �        ,]    �@    
� @  , 
�       8]    �� �   ip�               �L
�    %              � 8      D]    � $         �    i     
�    �     i
"   
 �p� @  , 
�       T^    �� �   �p�               �L%H > 8   dataAvailable,confirmContinue,isUpdatePending,buildDataRequest  
�H T   %              �     }        �GG %              
"   
 i
"   
 � 
"   
 i
"   
 i(�  L ( l       �        8_    �� �   � P   �        D_    �@    
� @  , 
�       P_    �� �   ip�               �L
�    %              � 8      \_    � $         �    i     
�    �     i
"   
 �p� @  , 
�       l`    �� �   �p�               �L%               "    � %     start-super-proc �� %     adm2/query.p i%     start-super-proc �� %     adm2/queryext.p % 	    initProps i
�    %T J D   FOR EACH Almacen       WHERE Almacen.CodCia = 1 NO-LOCK INDEXED-REPOSITION  �   � �     � �     � �         "    k� �    � 
�H T   %              �     }        �GG %              
"   
 i
"   
 � 
"   
 i
"   
   (�  L ( l       �        db    �� �   � P   �        pb    �@    
� @  , 
�       |b    �� �   ip�               �L
�    %              � 8      �b    � $         �           
�    �     i
"   
 �p� @  , 
�       �c    �� "  	 �p�               �L"    , %               �    "      � �         %              %                   "      %                  "      "      T(        "    �%              "    �� �   � "      �       "    i�    "    ��    � � �      �    i�    "     �     S    "      "    �     "    k%                � @    �     t T     P   4       � "      (0       4       k"      � �      � �    i� �   kT ,  %              T   "    k"    � � �     �    i� �   kT    �    "    k�    � "      �    i"      %                   %              %                   "      %                  "      �     "       \      H   "      ((       "    i%              � �    � � �     4  m     "      
�H T   %              �     }        �GG %              
"   
 i
"   
 � 
"   
 i
"   
   (�  L ( l       �        @h    �� �   � P   �        Lh    �@    
� @  , 
�       Xh    �� �   ip�               �L
�    %              � 8      dh    � $         �           
�    �     i
"   
 �p� @  , 
�       ti    �� 
  
 �p�               �L"    ,       "  
  ��    � �   k� �   �       "  	    �    � �   � � �   k�   � �     � �     � �   i�   � �     � �   i� �   k�   � �     � �     � �     
�H T   %              �     }        �GG %              
"   
 � 
"   
 i
"   
 � 
"   
 � (�  L ( l       �        k    �� �   � P   �        k    �@    
� @  , 
�       k    �� �   � p�               �L
�    %              � 8      (k    � $         �           
�    �       
"   
 �p� @  , 
�       8l    �� �   �p�               �L"    , 
"   
   p� @  , 
�       �l    �� o     p�               �L"    , 
"   
  p� @  , 
�       �l    �� J    p�               �L"    ,     %              %                   "      %                  "      �     "      4 (        "  
    �    � �     � �         "  	  m�     "    kT    "      "      @ A,    �   � �   � � �     "    i"       T      @   "    � (        "      � �    i� �      � �   i"    k     "  	   %              D H   @ A,    �   � �   i� �     "    i"    �,    S   "    i� �   �� �   � %                T      @   "    � (        "      � �    i� �      � �   i"    k     "  
   %                         "    � � �     "    i           "      � �   i"      
�H T   %              �     }        �GG %              
"   
 k
"   
   
"   
 k
"   
 i(�  L ( l       �        �p    �� �   � P   �        �p    �@    
� @  , 
�       q    �� �   kp�               �L
�    %              � 8      q    � $         �    i     
�    �     � 
"   
 �p� @  , 
�       $r    �� o   �p�               �L"    , 
"   
   p� @  , 
�       |r    �� J     p�               �L"    , "      %              %              %              %              %              %              %              %              %       	       %       
       %              %              %              %              %              %              %              %              "       "    � %     start-super-proc �� %     adm2/data.p %     start-super-proc �� %     adm2/dataext.p %     start-super-proc �� %     adm2/dataextcols.p %     start-super-proc �� %     adm2/dataextapi.p k
�H T   %              �     }        �GG %              
"   
 i
"   
 � 
"   
 i
"   
 i(�  L ( l       �        �u    �� �   � P   �        �u    �@    
� @  , 
�       �u    �� �   ip�               �L
�    %              � 8      �u    � $         �    i     
�    �     i
"   
 �p� @  , 
�       �v    �� v   �p�               �L%               %     "dtables.i" 
�H T   %              �     }        �GG %              
"   
 i
"   
 � 
"   
 i
"   
   (�  L ( l       �        �w    �� �   � P   �        �w    �@    
� @  , 
�       �w    �� �   ip�               �L
�    %              � 8      �w    � $         �           
�    �     i
"   
 �p� @  , 
�       �x    �� w   �p�               �L"    , 
�     	         �G
�H T   %              �     }        �GG %              
"   
 i
"   
 � 
"   
 i
"   
   (�  L ( l       �        �y    �� �   � P   �        �y    �@    
� @  , 
�       �y    �� �   ip�               �L
�    %              � 8      �y    � $         �           
�    �     i
"   
 �p� @  , 
�       �z    �� �  
 �p�               �L
"   
 , 
�     
        �G
�H T   %              �     }        �GG %              
"   
 i
"   
 � 
"   
 i
"   
   (�  L ( l       �        t{    �� �   � P   �        �{    �@    
� @  , 
�       �{    �� �   ip�               �L
�    %              � 8      �{    � $         �           
�    �     i
"   
 �p� @  , 
�       �|    �� |  	 �p�               �L
"   
 , 
"   
 �      � �  	   �         }    �
�H T   %              �     }        �GG %              
"   
 i
"   
 � 
"   
 i
"   
   (�  L ( l       �        �}    �� �   � P   �        �}    �@    
� @  , 
�       �}    �� �   ip�               �L
�    %              � 8      �}    � $         �           
�    �     i
"   
 �p� @  , 
�       �~    �� �   �p�               �L"    , 
"   
   �           �"      
�     
        �G
�H T   %              �     }        �GG %              
"   
 i
"   
 � 
"   
 i
"   
   (�  L ( l       �        �    �� �   � P   �        �    �@    
� @  , 
�       �    �� �   ip�               �L
�    %              � 8      �    � $         �           
�    �     i
"   
 �p� @  , 
�       ̀    �� �  	 �p�               �L
"   
 , 
�             �Gp�,  8         $     
"   
 h        � �   i
�    
�             �Gp�,  8         $     
"   
 h        � �   i
�    �    � �     
�        "    k� �    � %     modifyListProperty  
�    %      REMOVE  %     SupportedLinks %     Update-Target  %     bufferValidate  
�    "      �  %      setContextAndInitialize 
�    "      %     bufferCommit    
�    "      "      �    � K     
�    %               %     bufferCommit    
�    "      "      
�     
        �G�     }        �
�                    �           x   `       ��                 `  �  �               Tbm        O   ����    e�          O   ����    R�          O   ����    ��         $   o  �   ���                       �U     
                    � ߱               p    �          4   ����V                �                      ��                  q  �                  �jm           q  (  �  �  r  PV             t  �  4          4   �����V                D                      ��                  u  �                   km           u  �  x  o   v      ,                                 �  �   w  �V      �  �   x  �V      �  $   y  �  ���                        W     
                    � ߱          �   z  @W         �   {  `W      4  �   ~  �W          $   �  `  ���                       �W  @         �W              � ߱                     (                T �            
             
             
             
                 $   4   D          $   4   D   ����        ��                            ����                                            �           x   `       ��                 �  �  �               Hlm        O   ����    e�          O   ����    R�          O   ����    ��      e                      �          �  $   �  �   ���                       X     
                    � ߱                  �  �                      ��                   �  �                  �pm          �  (      4   ����$X      $   �  �  ���                       pX     
                    � ߱        d     �    (          4   �����X      /  �  T                               3   �����X  x  �   �  �X          O   �  ��  ��  �X               �          �  �   , �                          
                               �      ��                            ����                                            �           x   `       ��                 O  �  �               P.�        O   ����    e�          O   ����    R�          O   ����    ��         $   o  �   ���                       ta                         � ߱        X  $   p  ,  ���                       �a                         � ߱             �  p  �          4   �����a  b     
                �b                     �c  @        
 �c              � ߱            V   �  �  ���                        h  $   �  <  ���                       �c                         � ߱           $   �  �  ���                       d                         � ߱          0      �  �                      ��        0          �                    �|m    8     �  �      $   �  \  ���                       $d                         � ߱        �  $   �  �  ���                       Td                         � ߱            4   ����|d  �d                     �d                     �d                     @e                     `e                         � ߱        �  $   �  �  ���                                �  �          4   �����e      $   	    ���                       �e          �f             � ߱        �  $     d  ���                       �f                         � ߱                 X  �                      ��        0                               �m    |       �      $     ,  ���                       �f                         � ߱        �  $     �  ���                       $g                         � ߱            4   ����Lg      $     �  ���                       tg                         � ߱        �g     
                ph                     �i  @        
 �i              � ߱        (  V   %    ���                        �i       
       
        j       	       	       4j                     `j                         � ߱        T  $   l  �  ���                       H	  $     �  ���                       �j                         � ߱        �j     
                4k                     �l  @        
 Dl          �l  @        
 �l          4m  @        
 �l              � ߱        �	  V     �  ���                          �	      <
  �
                      ��        0          �  �                  D�m    l     �  t	      $   �  
  ���                       @m                         � ߱        �
  $   �  h
  ���                       pm                         � ߱        �
  4   �����m      4   �����m    $   �  �
  ���                       $n                         � ߱             �  $  �          4   ����Dn                �                      ��                  �  �                  Њm           �  4  �n                     �n       	       	           � ߱            $   �  �  ���                              �  0  �          4   ����o                �                      ��                  �  �                  d�m           �  @  �o                     p       
       
           � ߱            $   �  �  ���                       <p                     pp                         � ߱          $   �  $  ���                       �p     
                 q                     pr  @        
 0r          �r  @        
 �r              � ߱            V   �  �  ���                                    7            �  |  � x                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             �   �   �   �   �   �   �   �       $  4  D  T  d  t  �  \  �  �  �  �  8HXhx��������(8HX      �   �   �   �   �   �   �   �      $  4  D  T  d  t  �   \ �  �  �  �  8HXhx��������(8HX  �                    � �                     �          ��                            ����                                                        x   `       ��                  V  a  �               $�e        O   ����    e�          O   ����    R�          O   ����    ��              !                    ��                            ����                                            �           x   `       ��                  k  z  �               ��e        O   ����    e�          O   ����    R�          O   ����    ��             �              �                  $                  X  /  w       (  ��                      3   ����|�            H                      3   ������      O   x  ��  ��  ��               �          �  �    �                                             ��                            ����                                            <          x   `       ��                  �  �  �               ��e        O   ����    e�          O   ����    R�          O   ����    ��      ,       �              �          �       $                  6                     �          A                               �  /  �  h     x  Ԃ                      3   ������            �                      3   ����܂  �  /  �  �     �  �                      3   �����  l                            3   �����      $   �  @  ���                                                   � ߱                  �  �                  3   �����      $   �  �  ���                                                   � ߱        L  $   �     ���                       $�                         � ߱            O   �  ��  ��  @�               �          �  �   @ �                                                              0              0           ��                            ����                                                      x   `       ��                  �  �  �               Īm        O   ����    e�          O   ����    R�          O   ����    ��      �       $                  6       �              �          A                      �              /  �  @     P  p�                      3   ����T�  �        p  �                  3   ����x�      $   �  �  ���                                                   � ߱                  �                    3   ������      $   �  4  ���                                                   � ߱                     �          �  �   , �                                                                 ��                            ����                                            �           x   `       ��                  0  ;  �               Hf        O   ����    e�          O   ����    R�          O   ����    ��             :  �   �           4   ������      �   :  ��    ��                            ����                            TXS appSrvUtils E:\OpenEdge\on_in_co\APLIC\dtables.w should only be RUN PERSISTENT. ADDROW CANCELROW CANNAVIGATE CLOSEQUERY COLUMNPROPS COLVALUES COPYROW CREATEROW DELETEROW FETCHROW FETCHROWIDENT FINDROW FINDROWWHERE FIRSTROWIDS GETLASTCOMMITERRORTYPE HASFOREIGNKEYCHANGED OPENDATAQUERY OPENQUERY PREPAREQUERY ROWAVAILABLE ROWVALUES SUBMITROW UPDATEROW GETOBJECTTYPE xiRocketIndexLimit ADDQUERYWHERE ASSIGNQUERYSELECTION BUFFERCOMPAREDBTORO BUFFERWHERECLAUSE COLUMNDATATYPE COLUMNDBCOLUMN COLUMNQUERYSELECTION COLUMNTABLE COLUMNVALMSG DBCOLUMNDATANAME DBCOLUMNHANDLE EXCLUDECOLUMNS GETDATACOLUMNS GETFOREIGNVALUES GETQUERYPOSITION GETQUERYSORT GETQUERYSTRING GETQUERYWHERE GETTARGETPROCEDURE INDEXINFORMATION INSERTEXPRESSION NEWQUERYSTRING NEWQUERYVALIDATE NEWQUERYWHERE NEWWHERECLAUSE REFRESHROWIDENT REMOVEFOREIGNKEY REMOVEQUERYSELECTION RESOLVEBUFFER ROWIDWHERE ROWIDWHERECOLS SETQUERYPOSITION SETQUERYSORT SETQUERYSTRING SETQUERYWHERE WHERECLAUSEBUFFER GETAPPSERVICE GETASBOUND GETASDIVISION GETASHANDLE GETASHASSTARTED GETASINFO GETASINITIALIZEONRUN GETASUSEPROMPT GETSERVERFILENAME GETSERVEROPERATINGMODE RUNSERVERPROCEDURE SETAPPSERVICE SETASDIVISION SETASHANDLE SETASINFO SETASINITIALIZEONRUN SETASUSEPROMPT SETSERVERFILENAME SETSERVEROPERATINGMODE gshAstraAppserver gshSessionManager gshRIManager gshSecurityManager gshProfileManager gshRepositoryManager gshTranslationManager gshWebManager gscSessionId gsdSessionObj gshFinManager gshGenManager gshAgnManager gsdTempUniqueID gsdUserObj gsdRenderTypeObj gsdSessionScopeObj ghProp ghADMProps ghADMPropsBuf glADMLoadFromRepos glADMOk ANYMESSAGE ASSIGNLINKPROPERTY FETCHMESSAGES GETCHILDDATAKEY GETCONTAINERHANDLE GETCONTAINERHIDDEN GETCONTAINERSOURCE GETCONTAINERSOURCEEVENTS GETCONTAINERTYPE GETDATALINKSENABLED GETDATASOURCE GETDATASOURCEEVENTS GETDATASOURCENAMES GETDATATARGET GETDATATARGETEVENTS GETDBAWARE GETDESIGNDATAOBJECT GETDYNAMICOBJECT GETINSTANCEPROPERTIES GETLOGICALOBJECTNAME GETLOGICALVERSION GETOBJECTHIDDEN GETOBJECTINITIALIZED GETOBJECTNAME GETOBJECTPAGE GETOBJECTPARENT GETOBJECTVERSION GETOBJECTVERSIONNUMBER GETPARENTDATAKEY GETPASSTHROUGHLINKS GETPHYSICALOBJECTNAME GETPHYSICALVERSION GETPROPERTYDIALOG GETQUERYOBJECT GETRUNATTRIBUTE GETSUPPORTEDLINKS GETTRANSLATABLEPROPERTIES GETUIBMODE GETUSERPROPERTY INSTANCEPROPERTYLIST LINKHANDLES LINKPROPERTY MAPPEDENTRY MESSAGENUMBER PROPERTYTYPE REVIEWMESSAGES SETCHILDDATAKEY SETCONTAINERHIDDEN SETCONTAINERSOURCE SETCONTAINERSOURCEEVENTS SETDATALINKSENABLED SETDATASOURCE SETDATASOURCEEVENTS SETDATASOURCENAMES SETDATATARGET SETDATATARGETEVENTS SETDBAWARE SETDESIGNDATAOBJECT SETDYNAMICOBJECT SETINSTANCEPROPERTIES SETLOGICALOBJECTNAME SETLOGICALVERSION SETOBJECTNAME SETOBJECTPARENT SETOBJECTVERSION SETPARENTDATAKEY SETPASSTHROUGHLINKS SETPHYSICALOBJECTNAME SETPHYSICALVERSION SETRUNATTRIBUTE SETSUPPORTEDLINKS SETTRANSLATABLEPROPERTIES SETUIBMODE SETUSERPROPERTY SHOWMESSAGE SIGNATURE prepareInstance ObjectName CHAR  ObjectVersion ADM2.2 ObjectType SmartDataObject ContainerType PropertyDialog adm2/support/datad.w QueryObject LOGICAL ContainerHandle HANDLE InstanceProperties AppService,ASInfo,ASUsePrompt,CacheDuration,CheckCurrentChanged,DestroyStateless,DisconnectAppServer,ServerOperatingMode,ShareData,UpdateFromSource,ForeignFields,ObjectName,OpenOnInit,PromptColumns,PromptOnDelete,RowsToBatch,RebuildOnRepos,ToggleDataTargets SupportedLinks Data-Source,Data-Target,Navigation-Target,Update-Target,Commit-Target,Filter-Target ContainerHidden ObjectInitialized ObjectHidden ObjectsCreated HideOnInit UIBMode ContainerSource ContainerSourceEvents initializeObject,hideObject,viewObject,destroyObject,enableObject,confirmExit,confirmCancel,confirmOk,isUpdateActive DataSource DataSourceEvents dataAvailable,queryPosition,updateState,deleteComplete,fetchDataSet,confirmContinue,confirmCommit,confirmUndo,assignMaxGuess,isUpdatePending TranslatableProperties ObjectPage INT DBAware DesignDataObject DataSourceNames DataTarget DataTargetEvents CHARACTER updateState,rowObjectState,fetchBatch,LinkState LogicalObjectName PhysicalObjectName LogicalVersion PhysicalVersion DynamicObject RunAttribute ChildDataKey ParentDataKey DataLinksEnabled InactiveLinks InstanceId DECIMAL SuperProcedure SuperProcedureMode SuperProcedureHandle LayoutPosition ClassName RenderingProcedure ThinRenderingProcedure Label cType ADMProps Target WHERE Target = WIDGET-H(" ") AppService ASDivision ASHandle ASHasConnected ASHasStarted ASInfo ASInitializeOnRun ASUsePrompt BindSignature BindScope ServerOperatingMode ServerFileName ServerFirstCall NeedContext AutoCommit BLOBColumns BufferHandles CLOBColumns CommitSource CommitSourceEvents commitTransaction,undoTransaction CommitTarget CommitTargetEvents rowObjectState CurrentRowid ROWID CurrentUpdateSource DataColumns DataHandle DataIsFetched DataModified DataQueryBrowsed DataQueryString FetchOnOpen FillBatchOnRepos FilterActive FilterAvailable FilterSource FilterWindow FirstRowNum ForeignFields ForeignValues IgnoreTreeViewFilter IndexInformation LargeColumns LastRowNum NavigationSource NavigationSourceEvents fetchFirst,fetchNext,fetchPrev,fetchLast,startFilter OpenOnInit PrimarySDOSource PromptColumns PromptOnDelete QueryColumns QueryPosition QueryString RebuildOnRepos RowObject RowObjectState NoUpdates RowsToBatch Tables ToggleDataTargets TransferChildrenForAll UpdatableColumns UpdatableWhenNew UpdateSource AssignList AuditEnabled BaseQuery CalcFieldList CheckLastOnOpen DataColumnsByTable DBNames EntityFields FetchHasAudit FetchHasComment FetchAutoComment FirstResultRow KeyFields KeyTableId LastDBRowIdent LastResultRow NewBatchInfo NoLockReadOnlyTables PhysicalTables PositionForClient QueryHandle QueryRowIdent RequiredProperties SkipTransferDBRow TempTables UpdatableColumnsByTable UpdateFromSource RowObjUpd RowObjectTable RowObjUpdTable CheckCurrentChanged StatelessSavedProperties CheckCurrentChanged,RowObjectState,LastResultRow,FirstResultRow,QueryRowIdent DestroyStateless DisconnectAppServer ServerSubmitValidation DataFieldDefs "dtables.i" QueryContainer QueryContext AsynchronousSDO DataLogicProcedure DataLogicObject DataReadHandler DataReadColumns DataReadBuffer DataDelimiter | DataReadFormat TrimNumeric IsRowObjectExternal IsRowObjUpdExternal ManualAddQueryWhere DynamicData LastCommitErrorType LastCommitErrorKeys RunDataLogicProxy SchemaLocation CacheDuration INTEGER ShareData ghContainer adm2/smart.p cObjectName iStart / \ . hReposBuffer hPropTable hBuffer hTable deleteProperties ADM-CLONE-PROPS pcProcName hProc START-SUPER-PROC cAppService cASDivision cServerOperatingMode adm2/appserver.p getAppService Server Client NONE setASDivision setAppService adm2/dataquery.p dataAvailable,confirmContinue,isUpdatePending,buildDataRequest adm2/query.p adm2/queryext.p cTable iTable cColumns cDataCols cUpdCols cCalcData cCalcUpd iNumData iNumUpd cBuffers cKeyFields cAssignList iAssigns iPos iEntry iCount cTables cTableAssign cDbEntry cField cKeyTable cQueryString FOR EACH Almacen       WHERE Almacen.CodCia = 1 NO-LOCK INDEXED-REPOSITION ,   Almacen  ; CodCia CodAlm Descripcion INITPROPS piTableIndex lRet DELETERECORDSTATIC adm2/data.p adm2/dataext.p adm2/dataextcols.p adm2/dataextapi.p CodCia CodAlm Descripcion RowNum RowIdent RowMod RowIdentIdx RowUserProp ChangedFields cContainerType hRowObject hDataQuery cDataFieldDefs FOR EACH  setRowObjectTable setRowObjUpdTable getUpdatableColumns REMOVE Update-Target PUSHROWOBJUPDTABLE pcValType PUSHTABLEANDVALIDATE pcContext pcMessages pcUndoIds obtainContextForClient REMOTECOMMIT SERVERCOMMIT GETROWOBJUPDSTATIC DISABLE_UI qDataQuery t  P-  �  �:      / �<   ��      0         pcServices      ��      T         pcServices  �   ��      x         piStartRow  �   ��      �         piStartRow  �   ��      �         piStartRow  �   ��      �         piStartRow      ��              piStartRow  <  ��      ,        pcViewColList       ��      T       
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
 hTarget �  ��      �        pcMessage       ��      �        pcMessage       ��      �        plEnabled             �     cType       0     V   �                             getObjectType   R	  j	  l	  `        P  
   hReposBuffer    �        t  
   hPropTable  �        �  
   hBuffer           �  
   hTable  �  �     W   <          �                  adm-clone-props o  p  q  r  t  u  v  w  x  y  z  {  ~  �  �  �  �            P  
   hProc             p        pcProcName  �  �  	   X   <  X      �                  start-super-proc    �  �  �  �  �  �  �  �  �           �     cTable               iTable  <        0     cColumns    \        P     cDataCols   |        p     cUpdCols    �        �     cCalcData   �        �     cCalcUpd    �     	   �     iNumData    �     
   �     iNumUpd              cBuffers    8        ,     cKeyFields  X        L     cAssignList x        l     iAssigns    �        �     iPos    �        �     iEntry  �        �     iCount  �        �     cTables        �     cTableAssign    ,              cDbEntry    H        @     cField  h        \     cKeyTable             |     cQueryString    |  �  !   Y   �          �                  initProps   o  p  �  �  �  �  �  �    	            %  l      �  �  �  �  �  �  �  �  �  �  �  �  �  �            `     lRet              �        piTableIndex    �  �  (   Z   L  h      �                  deleteRecordStatic         !  =  >  Z  [  w  x  �  �  �  �  �  �  �  �    	  %  &  B  C  _  `  |  }  �  �  �  �  �  �  �  �                         !       �  �     [       t      �                  pushRowObjUpdTable  a  �        �        pcValType                  $       �  X     \       �      @                  pushTableAndValidate    w  x  z  �        |        pcContext   �             $       �        �        pcMessages            �        pcUndoIds     (     ]       d                        remoteCommit    �  �  �  �  �  T             $       x        l        pcMessages            �        pcUndoIds   �  �     ^       <      �                  serverCommit    �  �  �  (     _                                 getRowObjUpdStatic  �  �  �  l     `               `                  disable_UI  :  ;  0  �       X      `                      p  �  �     RowObject             (         0         <         D         P         X         d         CodCia  CodAlm  Descripcion RowNum  RowIdent    RowMod  RowIdentIdx RowUserProp     �  �  	   RowObjUpd   �                                              (         0         <         H         CodCia  CodAlm  Descripcion RowNum  RowIdent    RowMod  RowIdentIdx RowUserProp ChangedFields   x          l  
   appSrvUtils �       �     xiRocketIndexLimit  �        �  
   gshAstraAppserver   �        �  
   gshSessionManager             
   gshRIManager    <        (  
   gshSecurityManager  d        P  
   gshProfileManager   �        x  
   gshRepositoryManager    �  	 	     �  
   gshTranslationManager   �  
 
     �  
   gshWebManager           �     gscSessionId    (             gsdSessionObj   L        <  
   gshFinManager   p        `  
   gshGenManager   �        �  
   gshAgnManager   �        �     gsdTempUniqueID �        �     gsdUserObj           �     gsdRenderTypeObj    (             gsdSessionScopeObj  D       <  
   ghProp  d       X  
   ghADMProps  �       x  
   ghADMPropsBuf   �       �     glADMLoadFromRepos  �       �     glADMOk �       �  
   ghContainer     	         cObjectName (    
         iStart  H       <     cAppService h       \     cASDivision �       |     cServerOperatingMode    �       �     cContainerType  �       �     cQueryString    �       �  
   hRowObject           
   hDataQuery  <       0     cColumns             P     cDataFieldDefs  |    X  p  RowObject         X  �  RowObjUpd          "   >   �   �   �   �      !  "  #  :  F  G  H  J  L  M  N  R  S  V  W  X  Y  [  ]  _  a  b  c  f  h  i  k  l  m  n  o  u  w  }    �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  
  C
  D
  F
  G
  H
  I
  J
  K
  M
  N
  O
  P
  Q
  R
  S
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
                     	  
            �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  /  �  �  �  �  �  �  �  �  �       2  Q  S  h  �    	  #  3  4  5  8  9  :  >  A  B  _  s  �  %  &  2  V  �  �  �  �  �  C  &  '  (  )  .  4  ;  �  �  �  �  �  �  �      3  4  >  X  r  |  �  �  �  �      e�  E:\OpenEdge\on_in_co\APLIC\dtables.w �"  ��  C:\Progress\OpenEdge\src\adm2\data.i �"  �) . %C:\Progress\OpenEdge\src\adm2\custom\datacustom.i    $#  �� - C:\Progress\OpenEdge\src\adm2\robjflds.i d#  �h , E:\OpenEdge\on_in_co\APLIC\dtables.i �#  �   C:\Progress\OpenEdge\src\adm2\query.i    �#  z + C:\Progress\OpenEdge\src\adm2\delrecst.i �#  `W * C:\Progress\OpenEdge\src\adm2\tblprep.i  0$   ) %C:\Progress\OpenEdge\src\adm2\custom\querycustom.i   d$  � ! C:\Progress\OpenEdge\src\adm2\dataquery.i    �$  �Z ( %C:\Progress\OpenEdge\src\adm2\custom\dataquerycustom.i   �$  �< " C:\Progress\OpenEdge\src\adm2\appserver.i     %  �� ' %C:\Progress\OpenEdge\src\adm2\custom\appservercustom.i   X%  I� # C:\Progress\OpenEdge\src\adm2\smart.i    �%  Ds & C:\Progress\OpenEdge\gui\fn  �%  tw % %C:\Progress\OpenEdge\src\adm2\custom\smartcustom.i   �%  Q. $ C:\Progress\OpenEdge\gui\set 8&  �>  C:\Progress\OpenEdge\src\adm2\dataprop.i `&  ��  %C:\Progress\OpenEdge\src\adm2\custom\datapropcustom.i    �&  ��  %C:\Progress\OpenEdge\src\adm2\custom\dataprtocustom.i    �&  �K  C:\Progress\OpenEdge\src\adm2\qryprop.i  '  -�  %C:\Progress\OpenEdge\src\adm2\custom\qrypropcustom.i P'  ��  %C:\Progress\OpenEdge\src\adm2\custom\qryprtocustom.i �'   
 C:\Progress\OpenEdge\src\adm2\dataqueryprop.i    �'  �d  %C:\Progress\OpenEdge\src\adm2\custom\dataquerypropcustom.i   (  ��  %C:\Progress\OpenEdge\src\adm2\custom\dataqueryprtocustom.i   T(  �l  C:\Progress\OpenEdge\src\adm2\appsprop.i �(  ɏ  %C:\Progress\OpenEdge\src\adm2\custom\appspropcustom.i    �(  V  %C:\Progress\OpenEdge\src\adm2\custom\appsprtocustom.i    )  i$  C:\Progress\OpenEdge\src\adm2\smrtprop.i X)  �j  C:\Progress\OpenEdge\gui\get �)  �  %C:\Progress\OpenEdge\src\adm2\custom\smrtpropcustom.i    �)  ��  %C:\Progress\OpenEdge\src\adm2\custom\smrtprtocustom.i    �)  ��  C:\Progress\OpenEdge\src\adm2\smrtprto.i <*  Su  C:\Progress\OpenEdge\src\adm2\globals.i  p*  M�  %C:\Progress\OpenEdge\src\adm2\custom\globalscustom.i �*  )a  %C:\Progress\OpenEdge\src\adm2\custom\smartdefscustom.i   �*  �  C:\Progress\OpenEdge\src\adm2\appsprto.i (+  ��  %C:\Progress\OpenEdge\src\adm2\custom\appserverdefscustom.i   \+  ��  C:\Progress\OpenEdge\src\adm2\dataqueryprto.i    �+  ª  %C:\Progress\OpenEdge\src\adm2\custom\dataquerydefscustom.i   �+  �� 	 C:\Progress\OpenEdge\src\adm2\qryprto.i  (,  �  %C:\Progress\OpenEdge\src\adm2\custom\querydefscustom.i   \,  �`  C:\Progress\OpenEdge\src\adm2\dataprto.i �,  �  %C:\Progress\OpenEdge\src\adm2\custom\datadefscustom.i    �,  e�  !C:\Progress\OpenEdge\gui\adecomm\appserv.i   -  U   E:\OpenEdge\on_in_co\APLIC\dtables_cl.w         A      �-  �   �     �-  [  �     �-     �  &   �-  �   "     �-     �  .   �-  �   �     �-     �     �-  �   �     .     ~  $   .  �   |     $.     Z  $   4.  �   X     D.     6  $   T.  �   3     d.       $   t.  �        �.     �  $   �.  �   �     �.     �  $   �.  �   �     �.     �  $   �.  �   �     �.     �  $   �.  �   s     /     [  -   /  �   W     $/     S  ,   4/  k        D/  �        T/     �  +   d/  �  �      t/     �  +   �/  �  �      �/     �  +   �/  �  �      �/     �  +   �/  �  �      �/       +   �/  �  |      �/     b  +   0  �  _      0     E  +   $0  �  B      40     (  +   D0  �  %      T0       +   d0  �        t0     �  +   �0  �  �      �0     �  +   �0  �  �      �0     �  +   �0  �  �      �0     �  +   �0  �  �      �0     z  +   1  �  w      1     ]  +   $1  �  Z      41     @  +   D1  �  =      T1     #  +   d1  �         t1       +   �1  �  �      �1     �  $   �1  �  �      �1     �  $   �1  j  |      �1     Z  $   �1  i  Y      �1     7  $   2  h  6      2       $   $2  ^  
      42     �  *   D2  ]  �      T2     �  *   d2  \  �      t2     �  *   �2  [  �      �2     o  *   �2  Z  n      �2     H  *   �2  Y  G      �2     !  *   �2  X         �2     �  *   3  W  �      3     �  *   $3  V  �      43     �  *   D3  U  �      T3     �  *   d3  T  �      t3     ^  *   �3  S  ]      �3     7  *   �3  R  6      �3       *   �3  Q        �3     �  *   �3  P  �      �3     �  *   4  O  �      4     �  *   $4  N  �      44     t  *   D4  M  s      T4     M  *   d4  ?  ?      t4       $   �4    �      �4     �  $   �4  �   ?      �4     �  )   �4  g   �      �4  a   �  !   �4     r  (   �4  _   p  !   5     N  $   5  ]   L  !   $5     *  $   45  I     !   D5  �     "   T5     �  '   d5  �   �  "   t5     �  $   �5  �   �  "   �5     k  $   �5  �   i  "   �5     G  $   �5  g   -  "   �5          �5  O   �  "   �5  �   �  #   6     ~  &   6  �   N  #   $6     �  %   46  �   �  #   D6     �  $   T6  �   �  #   d6     �  $   t6  �   �  #   �6     �  $   �6  �   �  #   �6     `  $   �6  �   L  #   �6     *  $   �6  }     #   �6     �  $   �6     �  #   7     2  "   7     �  !   $7     �      47     8     D7  �   /     T7  O   !     d7          t7     �     �7  �   �     �7  �   �     �7  O   r     �7     a     �7          �7  y   �
     �7  �   �
  
   �7  G   �
     8     �
     8     {
     $8  c   
  
   48  x   
     D8  M   �	     T8     �	     d8     �	     t8  a   �	     �8  �  i	     �8     J	     �8  �  	     �8  O   		     �8     �     �8     �     �8  �   �     �8     �     9     �     9  x   �     $9     �     49     e     D9     a     T9     M     d9     4     t9  Q   $     �9     �     �9     �     �9     ~     �9     d     �9  ]   ^  
   �9     T     �9       
   �9     �     :     �  
   :  Z   �     $:     �  	   4:     �     D:     �     T:     �     d:  c   h     t:     F     �:     �      �:     �      �:     �      �:     �      �:     &      �:           �:           
	��V�;�K?    �              �                                 �� 3F140144utf-8 MAIN O:\on_in_co\Util\vgn-clie.w,, PROCEDURE disable_UI,, PROCEDURE adm-create-objects,, PROCEDURE displayObjects,, PROCEDURE start-super-proc,,INPUT pcProcName CHARACTER PROCEDURE adm-clone-props,, PROCEDURE toggleData,,INPUT plEnabled LOGICAL PROCEDURE showMessageProcedure,,INPUT pcMessage CHARACTER,OUTPUT plAnswer LOGICAL PROCEDURE returnFocus,,INPUT hTarget HANDLE PROCEDURE repositionObject,,INPUT pdRow DECIMAL,INPUT pdCol DECIMAL PROCEDURE removeLink,,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE PROCEDURE removeAllLinks,, PROCEDURE modifyUserLinks,,INPUT pcMod CHARACTER,INPUT pcLinkName CHARACTER,INPUT phObject HANDLE PROCEDURE modifyListProperty,,INPUT phCaller HANDLE,INPUT pcMode CHARACTER,INPUT pcListName CHARACTER,INPUT pcListValue CHARACTER PROCEDURE hideObject,, PROCEDURE exitObject,, PROCEDURE editInstanceProperties,, PROCEDURE displayLinks,, PROCEDURE createControls,, PROCEDURE changeCursor,,INPUT pcCursor CHARACTER PROCEDURE applyEntry,,INPUT pcField CHARACTER PROCEDURE adjustTabOrder,,INPUT phObject HANDLE,INPUT phAnchor HANDLE,INPUT pcPosition CHARACTER PROCEDURE addMessage,,INPUT pcText CHARACTER,INPUT pcField CHARACTER,INPUT pcTable CHARACTER PROCEDURE addLink,,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE PROCEDURE unbindServer,,INPUT pcMode CHARACTER PROCEDURE startServerObject,, PROCEDURE runServerObject,,INPUT phAppService HANDLE PROCEDURE restartServerObject,, PROCEDURE initializeServerObject,, PROCEDURE disconnectObject,, PROCEDURE destroyServerObject,, PROCEDURE bindServer,, PROCEDURE processAction,,INPUT pcAction CHARACTER PROCEDURE enableObject,, PROCEDURE disableObject,, PROCEDURE applyLayout,, PROCEDURE viewPage,,INPUT piPageNum INTEGER PROCEDURE viewObject,, PROCEDURE selectPage,,INPUT piPageNum INTEGER PROCEDURE removePageNTarget,,INPUT phTarget HANDLE,INPUT piPage INTEGER PROCEDURE passThrough,,INPUT pcLinkName CHARACTER,INPUT pcArgument CHARACTER PROCEDURE notifyPage,,INPUT pcProc CHARACTER PROCEDURE initPages,,INPUT pcPageList CHARACTER PROCEDURE initializeVisualContainer,, PROCEDURE hidePage,,INPUT piPageNum INTEGER PROCEDURE destroyObject,, PROCEDURE deletePage,,INPUT piPageNum INTEGER PROCEDURE createObjects,, PROCEDURE constructObject,,INPUT pcProcName CHARACTER,INPUT phParent HANDLE,INPUT pcPropList CHARACTER,OUTPUT phObject HANDLE PROCEDURE changePage,, PROCEDURE assignPageProperty,,INPUT pcProp CHARACTER,INPUT pcValue CHARACTER PROCEDURE validateFields,,INPUT-OUTPUT pcNotValidFields CHARACTER PROCEDURE updateTitle,, PROCEDURE updateRecord,, PROCEDURE updateMode,,INPUT pcMode CHARACTER PROCEDURE showDataMessagesProcedure,,OUTPUT pcReturn CHARACTER PROCEDURE resetRecord,, PROCEDURE queryPosition,,INPUT pcState CHARACTER PROCEDURE okToContinueProcedure,,INPUT pcAction CHARACTER,OUTPUT plAnswer LOGICAL PROCEDURE deleteRecord,, PROCEDURE dataAvailable,,INPUT pcRelative CHARACTER PROCEDURE confirmExit,,INPUT-OUTPUT plCancel LOGICAL PROCEDURE confirmDelete,,INPUT-OUTPUT plAnswer LOGICAL PROCEDURE confirmContinue,,INPUT-OUTPUT plCancel LOGICAL PROCEDURE collectChanges,,INPUT-OUTPUT pcChanges CHARACTER,INPUT-OUTPUT pcInfo CHARACTER PROCEDURE viewRecord,, PROCEDURE valueChanged,, PROCEDURE updateState,,INPUT pcState CHARACTER PROCEDURE toolbar,,INPUT pcValue CHARACTER PROCEDURE initializeObject,, PROCEDURE enableFields,, PROCEDURE displayFields,,INPUT pcColValues CHARACTER PROCEDURE disableFields,,INPUT pcFieldType CHARACTER PROCEDURE copyRecord,, PROCEDURE cancelRecord,, PROCEDURE addRecord,, FUNCTION Signature,CHARACTER,INPUT pcName CHARACTER FUNCTION showmessage,LOGICAL,INPUT pcMessage CHARACTER FUNCTION setUserProperty,LOGICAL,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER FUNCTION setUIBMode,LOGICAL,INPUT pcMode CHARACTER FUNCTION setTranslatableProperties,LOGICAL,INPUT pcPropList CHARACTER FUNCTION setSupportedLinks,LOGICAL,INPUT pcLinkList CHARACTER FUNCTION setRunAttribute,LOGICAL,INPUT cRunAttribute CHARACTER FUNCTION setPhysicalVersion,LOGICAL,INPUT cVersion CHARACTER FUNCTION setPhysicalObjectName,LOGICAL,INPUT cTemp CHARACTER FUNCTION setPassThroughLinks,LOGICAL,INPUT pcLinks CHARACTER FUNCTION setParentDataKey,LOGICAL,INPUT cParentDataKey CHARACTER FUNCTION setObjectVersion,LOGICAL,INPUT cObjectVersion CHARACTER FUNCTION setObjectName,LOGICAL,INPUT pcName CHARACTER FUNCTION setLogicalVersion,LOGICAL,INPUT cVersion CHARACTER FUNCTION setInstanceProperties,LOGICAL,INPUT pcPropList CHARACTER FUNCTION setDynamicObject,LOGICAL,INPUT lTemp LOGICAL FUNCTION setDesignDataObject,LOGICAL,INPUT pcDataObject CHARACTER FUNCTION setDBAware,LOGICAL,INPUT lAware LOGICAL FUNCTION setDataTargetEvents,LOGICAL,INPUT pcEvents CHARACTER FUNCTION setDataTarget,LOGICAL,INPUT pcTarget CHARACTER FUNCTION setDataSourceNames,LOGICAL,INPUT pcSourceNames CHARACTER FUNCTION setDataSourceEvents,LOGICAL,INPUT pcEventsList CHARACTER FUNCTION setDataSource,LOGICAL,INPUT phObject HANDLE FUNCTION setDataLinksEnabled,LOGICAL,INPUT lDataLinksEnabled LOGICAL FUNCTION setContainerSourceEvents,LOGICAL,INPUT pcEvents CHARACTER FUNCTION setContainerSource,LOGICAL,INPUT phObject HANDLE FUNCTION setContainerHidden,LOGICAL,INPUT plHidden LOGICAL FUNCTION setChildDataKey,LOGICAL,INPUT cChildDataKey CHARACTER FUNCTION reviewMessages,CHARACTER, FUNCTION propertyType,CHARACTER,INPUT pcPropName CHARACTER FUNCTION messageNumber,CHARACTER,INPUT piMessage INTEGER FUNCTION mappedEntry,CHARACTER,INPUT pcEntry CHARACTER,INPUT pcList CHARACTER,INPUT plFirst LOGICAL,INPUT pcDelimiter CHARACTER FUNCTION linkProperty,CHARACTER,INPUT pcLink CHARACTER,INPUT pcPropName CHARACTER FUNCTION linkHandles,CHARACTER,INPUT pcLink CHARACTER FUNCTION instancePropertyList,CHARACTER,INPUT pcPropList CHARACTER FUNCTION getUserProperty,CHARACTER,INPUT pcPropName CHARACTER FUNCTION getUIBMode,CHARACTER, FUNCTION getTranslatableProperties,CHARACTER, FUNCTION getSupportedLinks,CHARACTER, FUNCTION getRunAttribute,CHARACTER, FUNCTION getQueryObject,LOGICAL, FUNCTION getPropertyDialog,CHARACTER, FUNCTION getPhysicalVersion,CHARACTER, FUNCTION getPhysicalObjectName,CHARACTER, FUNCTION getPassThroughLinks,CHARACTER, FUNCTION getParentDataKey,CHARACTER, FUNCTION getObjectVersionNumber,CHARACTER, FUNCTION getObjectVersion,CHARACTER, FUNCTION getObjectPage,INTEGER, FUNCTION getObjectName,CHARACTER, FUNCTION getObjectInitialized,LOGICAL, FUNCTION getObjectHidden,LOGICAL, FUNCTION getLogicalVersion,CHARACTER, FUNCTION getLogicalObjectName,CHARACTER, FUNCTION getInstanceProperties,CHARACTER, FUNCTION getDynamicObject,LOGICAL, FUNCTION getDesignDataObject,CHARACTER, FUNCTION getDBAware,LOGICAL, FUNCTION getDataTargetEvents,CHARACTER, FUNCTION getDataTarget,CHARACTER, FUNCTION getDataSourceNames,CHARACTER, FUNCTION getDataSourceEvents,CHARACTER, FUNCTION getDataSource,HANDLE, FUNCTION getDataLinksEnabled,LOGICAL, FUNCTION getContainerType,CHARACTER, FUNCTION getContainerSourceEvents,CHARACTER, FUNCTION getContainerSource,HANDLE, FUNCTION getContainerHidden,LOGICAL, FUNCTION getContainerHandle,HANDLE, FUNCTION getChildDataKey,CHARACTER, FUNCTION fetchMessages,CHARACTER, FUNCTION assignLinkProperty,LOGICAL,INPUT pcLink CHARACTER,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER FUNCTION anyMessage,LOGICAL, FUNCTION setServerOperatingMode,LOGICAL,INPUT pcServerOperatingMode CHARACTER FUNCTION setServerFileName,LOGICAL,INPUT pcFileName CHARACTER FUNCTION setASUsePrompt,LOGICAL,INPUT plFlag LOGICAL FUNCTION setASInitializeOnRun,LOGICAL,INPUT plInitialize LOGICAL FUNCTION setASInfo,LOGICAL,INPUT pcInfo CHARACTER FUNCTION setASHandle,LOGICAL,INPUT phASHandle HANDLE FUNCTION setASDivision,LOGICAL,INPUT pcDivision CHARACTER FUNCTION setAppService,LOGICAL,INPUT pcAppService CHARACTER FUNCTION runServerProcedure,HANDLE,INPUT pcServerFileName CHARACTER,INPUT phAppService HANDLE FUNCTION getServerOperatingMode,CHARACTER, FUNCTION getServerFileName,CHARACTER, FUNCTION getASUsePrompt,LOGICAL, FUNCTION getASInitializeOnRun,LOGICAL, FUNCTION getASInfo,CHARACTER, FUNCTION getASHasStarted,LOGICAL, FUNCTION getASHandle,HANDLE, FUNCTION getAsDivision,CHARACTER, FUNCTION getASBound,LOGICAL, FUNCTION getAppService,CHARACTER, FUNCTION createUiEvents,LOGICAL, FUNCTION getObjectSecured,LOGICAL, FUNCTION getObjectTranslated,LOGICAL, FUNCTION setResizeVertical,LOGICAL,INPUT plResizeVertical LOGICAL FUNCTION setResizeHorizontal,LOGICAL,INPUT plResizeHorizontal LOGICAL FUNCTION setObjectLayout,LOGICAL,INPUT pcLayout CHARACTER FUNCTION setLayoutOptions,LOGICAL,INPUT pcOptions CHARACTER FUNCTION setHideOnInit,LOGICAL,INPUT plHide LOGICAL FUNCTION setDisableOnInit,LOGICAL,INPUT plDisable LOGICAL FUNCTION setDefaultLayout,LOGICAL,INPUT pcDefault CHARACTER FUNCTION setAllFieldNames,LOGICAL,INPUT pcValue CHARACTER FUNCTION setAllFieldHandles,LOGICAL,INPUT pcValue CHARACTER FUNCTION getResizeVertical,LOGICAL, FUNCTION getResizeHorizontal,LOGICAL, FUNCTION getWidth,DECIMAL, FUNCTION getRow,DECIMAL, FUNCTION getObjectLayout,CHARACTER, FUNCTION getObjectEnabled,LOGICAL, FUNCTION getLayoutVariable,CHARACTER, FUNCTION getLayoutOptions,CHARACTER, FUNCTION getHideOnInit,LOGICAL, FUNCTION getHeight,DECIMAL, FUNCTION getEnabledObjHdls,CHARACTER, FUNCTION getEnabledObjFlds,CHARACTER, FUNCTION getDisableOnInit,LOGICAL, FUNCTION getDefaultLayout,CHARACTER, FUNCTION getCol,DECIMAL, FUNCTION getAllFieldNames,CHARACTER, FUNCTION getAllFieldHandles,CHARACTER, FUNCTION setStatusArea,LOGICAL,INPUT plStatusArea LOGICAL FUNCTION setWindowTitleViewer,LOGICAL,INPUT phViewer HANDLE FUNCTION setWaitForObject,LOGICAL,INPUT phObject HANDLE FUNCTION setUpdateSource,LOGICAL,INPUT pcSource CHARACTER FUNCTION setTopOnly,LOGICAL,INPUT plTopOnly LOGICAL FUNCTION setSdoForeignFields,LOGICAL,INPUT cSdoForeignFields CHARACTER FUNCTION setSavedContainerMode,LOGICAL,INPUT cSavedContainerMode CHARACTER FUNCTION setRunMultiple,LOGICAL,INPUT plMultiple LOGICAL FUNCTION setRunDOOptions,LOGICAL,INPUT pcOptions CHARACTER FUNCTION setRouterTarget,LOGICAL,INPUT phObject HANDLE FUNCTION setReEnableDataLinks,LOGICAL,INPUT cReEnableDataLinks CHARACTER FUNCTION setPrimarySdoTarget,LOGICAL,INPUT hPrimarySdoTarget HANDLE FUNCTION setPageSource,LOGICAL,INPUT phObject HANDLE FUNCTION setPageNTarget,LOGICAL,INPUT pcObject CHARACTER FUNCTION setOutMessageTarget,LOGICAL,INPUT phObject HANDLE FUNCTION setNavigationTarget,LOGICAL,INPUT cTarget CHARACTER FUNCTION setNavigationSourceEvents,LOGICAL,INPUT pcEvents CHARACTER FUNCTION setNavigationSource,LOGICAL,INPUT pcSource CHARACTER FUNCTION setMultiInstanceSupported,LOGICAL,INPUT lMultiInstanceSupported LOGICAL FUNCTION setMultiInstanceActivated,LOGICAL,INPUT lMultiInstanceActivated LOGICAL FUNCTION setInMessageTarget,LOGICAL,INPUT phObject HANDLE FUNCTION setFilterSource,LOGICAL,INPUT phObject HANDLE FUNCTION setDynamicSDOProcedure,LOGICAL,INPUT pcProc CHARACTER FUNCTION setDisabledAddModeTabs,LOGICAL,INPUT cDisabledAddModeTabs CHARACTER FUNCTION setCurrentPage,LOGICAL,INPUT iPage INTEGER FUNCTION setContainerTarget,LOGICAL,INPUT pcObject CHARACTER FUNCTION setCallerWindow,LOGICAL,INPUT h HANDLE FUNCTION setCallerProcedure,LOGICAL,INPUT h HANDLE FUNCTION setCallerObject,LOGICAL,INPUT h HANDLE FUNCTION pageNTargets,CHARACTER,INPUT phTarget HANDLE,INPUT piPageNum INTEGER FUNCTION getStatusArea,LOGICAL, FUNCTION getWindowTitleViewer,HANDLE, FUNCTION getWaitForObject,HANDLE, FUNCTION getUpdateSource,CHARACTER, FUNCTION getTopOnly,LOGICAL, FUNCTION getSdoForeignFields,CHARACTER, FUNCTION getSavedContainerMode,CHARACTER, FUNCTION getRunMultiple,LOGICAL, FUNCTION getRunDOOptions,CHARACTER, FUNCTION getReEnableDataLinks,CHARACTER, FUNCTION getPrimarySdoTarget,HANDLE, FUNCTION getPageSource,HANDLE, FUNCTION getPageNTarget,CHARACTER, FUNCTION getOutMessageTarget,HANDLE, FUNCTION getNavigationTarget,HANDLE, FUNCTION getNavigationSourceEvents,CHARACTER, FUNCTION getNavigationSource,CHARACTER, FUNCTION getMultiInstanceSupported,LOGICAL, FUNCTION getMultiInstanceActivated,LOGICAL, FUNCTION getFilterSource,HANDLE, FUNCTION getDynamicSDOProcedure,CHARACTER, FUNCTION getDisabledAddModeTabs,CHARACTER, FUNCTION getCurrentPage,INTEGER, FUNCTION getContainerTargetEvents,CHARACTER, FUNCTION getContainerTarget,CHARACTER, FUNCTION getContainerMode,CHARACTER, FUNCTION getCallerWindow,HANDLE, FUNCTION getCallerProcedure,HANDLE, FUNCTION enablePagesInFolder,LOGICAL,INPUT pcPageInformation CHARACTER FUNCTION disablePagesInFolder,LOGICAL,INPUT pcPageInformation CHARACTER FUNCTION showDataMessages,CHARACTER, FUNCTION setWindowTitleField,LOGICAL,INPUT cWindowTitleField CHARACTER FUNCTION setUpdateTargetNames,LOGICAL,INPUT pcTargetNames CHARACTER FUNCTION setUpdateTarget,LOGICAL,INPUT pcObject CHARACTER FUNCTION setTableIOSourceEvents,LOGICAL,INPUT pcEvents CHARACTER FUNCTION setTableIOSource,LOGICAL,INPUT phObject HANDLE FUNCTION setSaveSource,LOGICAL,INPUT plSave LOGICAL FUNCTION setObjectParent,LOGICAL,INPUT phParent HANDLE FUNCTION setLogicalObjectName,LOGICAL,INPUT pcLogicalObjectName CHARACTER FUNCTION setGroupAssignTargetEvents,LOGICAL,INPUT pcEvents CHARACTER FUNCTION setGroupAssignTarget,LOGICAL,INPUT pcObject CHARACTER FUNCTION setGroupAssignSourceEvents,LOGICAL,INPUT pcEvents CHARACTER FUNCTION setGroupAssignSource,LOGICAL,INPUT phObject HANDLE FUNCTION setEnabledFields,LOGICAL,INPUT pcFieldList CHARACTER FUNCTION setDisplayedFields,LOGICAL,INPUT pcFieldList CHARACTER FUNCTION setDataModified,LOGICAL,INPUT plModified LOGICAL FUNCTION setContainerMode,LOGICAL,INPUT pcContainerMode CHARACTER FUNCTION okToContinue,LOGICAL,INPUT pcAction CHARACTER FUNCTION getWindowTitleField,CHARACTER, FUNCTION getUpdateTargetNames,CHARACTER, FUNCTION getUpdateTarget,CHARACTER, FUNCTION getTableIOSourceEvents,CHARACTER, FUNCTION getTableIOSource,HANDLE, FUNCTION getRowIdent,CHARACTER, FUNCTION getRecordState,CHARACTER, FUNCTION getObjectParent,HANDLE, FUNCTION getNewRecord,CHARACTER, FUNCTION getGroupAssignTargetEvents,CHARACTER, FUNCTION getGroupAssignTarget,CHARACTER, FUNCTION getGroupAssignSourceEvents,CHARACTER, FUNCTION getGroupAssignSource,HANDLE, FUNCTION getFieldsEnabled,LOGICAL, FUNCTION getFieldHandles,CHARACTER, FUNCTION getEnabledHandles,CHARACTER, FUNCTION getEnabledFields,CHARACTER, FUNCTION getDisplayedTables,CHARACTER, FUNCTION getDisplayedFields,CHARACTER, FUNCTION getDataModified,LOGICAL, FUNCTION getCreateHandles,CHARACTER, FUNCTION setShowPopup,LOGICAL,INPUT plShowPopup LOGICAL FUNCTION getShowPopup,LOGICAL, FUNCTION getObjectType,character, FUNCTION getTargetProcedure,HANDLE, FUNCTION widgetValueList,CHARACTER,INPUT pcNameList CHARACTER,INPUT pcDelimiter CHARACTER FUNCTION widgetValue,CHARACTER,INPUT pcName CHARACTER FUNCTION widgetIsTrue,LOGICAL,INPUT pcName CHARACTER FUNCTION widgetIsModified,LOGICAL,INPUT pcNameList CHARACTER FUNCTION widgetIsFocused,LOGICAL,INPUT pcName CHARACTER FUNCTION widgetIsBlank,LOGICAL,INPUT pcNameList CHARACTER FUNCTION widgetLongcharValue,LONGCHAR,INPUT pcName CHARACTER FUNCTION widgetHandle,HANDLE,INPUT pcName CHARACTER FUNCTION viewWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION toggleWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION resetWidgetValue,LOGICAL,INPUT pcNameList CHARACTER FUNCTION highlightWidget,LOGICAL,INPUT pcNameList CHARACTER,INPUT pcHighlightType CHARACTER FUNCTION hideWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION formattedWidgetValueList,CHARACTER,INPUT pcNameList CHARACTER,INPUT pcDelimiter CHARACTER FUNCTION formattedWidgetValue,CHARACTER,INPUT pcName CHARACTER FUNCTION enableWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION enableRadioButton,LOGICAL,INPUT pcNameList CHARACTER,INPUT piButtonNum INTEGER FUNCTION disableWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION disableRadioButton,LOGICAL,INPUT pcNameList CHARACTER,INPUT piButtonNum INTEGER FUNCTION clearWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION blankWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION assignWidgetValueList,LOGICAL,INPUT pcNameList CHARACTER,INPUT pcValueList CHARACTER,INPUT pcDelimiter CHARACTER FUNCTION assignWidgetValue,LOGICAL,INPUT pcName CHARACTER,INPUT pcValue CHARACTER FUNCTION assignFocusedWidget,LOGICAL,INPUT pcName CHARACTER      0              �
             � 0  ��              |s              �+    +   T~ �  U   � `  V   T� �   Z   H� �  ]   ̏ t  ^           @� X  ? �� �(  iSO8859-1                                                                           �    �                                      �                   ��                    p     �   ��    �             ��  �   �                                                               PROGRESS                                    
    
                    �              �                                                                                                     
  4         �          X  @     X     \      �                       �                 �   �  �      ,  
    
                    �             �                                                                                          �          
  \  
      �  
    
                  �  �             H                                                                                          
          
          �  
    
                  p  8             �                                                                                                    
  �  )      0  
    
                    �             �                                                                                          )          
  `  <      �  
    
                  �  �             L                                                                                          <          
    N      �  
    
                  t  <  	           �                                                                                          N          
  �  c      4  
    
                     �  
           �                                                                                          c          
  d  y      �  
    
                  �  �             P                                                                                          y          
    �      �                         x  @             �                                                                                          �            �  �      8                        $  �             �                                                                                          �            h	  �      �  
    
                  �  �	             T	                                                                                          �          
  
  �      �	  
    
                  |	  D
              
                                                                                          �          
  �
  �      <
  
    
                  (
  �
             �
                                                                                          �          
  l  �      �
                        �
  �             X                                                                                          �              �      �                        �  H                                                                                                       �            �  �      @                        ,  �             �                                                                                          �                �      �                        �                 \                                                                                          �                          ��                                               ��            X  P ��            
             
             
             
             
                                         
                                                                                                                                                                        P   `   p   �   �   �   �   �   �   �   �           0  @  P  `  p      P   `   p   �   �   �   �   �   �   �   �          0  @  P  `  p                                                                                                                (  0  8  H  @          L             `  h  p  �  |          �             �  �  �  �  �                         �  �  �  �  �          �                   <  ,                                                                     NomCli  x(50)   Nombre  Nombre      Nombre del Cliente  DirCli  x(60)   Direcci�n   Direcci�n       Direcci�n del Cliente   NroCard x(8)    NroCard Nrocard     Ruc x(11)   Ruc Ruc     Registro Unico de Contribuyente (Cliente)   CodDept X(3)    Departamento    Departamento        �  ���������     �     e(                �     i     	       !   (   0   4     ��                                               �          ����                            undefined                                                               �       �  �   l   �                        �����               ��	                    O   ����    e�          O   ����    R�          O   ����    ��      t       �   �              4   ����     /                                    3   ����       $     H  ���                       8      
                       � ߱        �  �      D       p
     F          assignFocusedWidget         �      �     <       LOGICAL,INPUT pcName CHARACTER  assignWidgetValue   �      �      $    P       LOGICAL,INPUT pcName CHARACTER,INPUT pcValue CHARACTER  assignWidgetValueList         \      �    b       LOGICAL,INPUT pcNameList CHARACTER,INPUT pcValueList CHARACTER,INPUT pcDelimiter CHARACTER  blankWidget t      �          x       LOGICAL,INPUT pcNameList CHARACTER  clearWidget �      @      l    �       LOGICAL,INPUT pcNameList CHARACTER  disableRadioButton  L      �      �    �       LOGICAL,INPUT pcNameList CHARACTER,INPUT piButtonNum INTEGER    disableWidget   �            4    �       LOGICAL,INPUT pcNameList CHARACTER  enableRadioButton         X      �    �       LOGICAL,INPUT pcNameList CHARACTER,INPUT piButtonNum INTEGER    enableWidget    l      �      �    �       LOGICAL,INPUT pcNameList CHARACTER  formattedWidgetValue    �             X  	  �       CHARACTER,INPUT pcName CHARACTER    formattedWidgetValueList    8      |      �  
  �       CHARACTER,INPUT pcNameList CHARACTER,INPUT pcDelimiter CHARACTER    hideWidget  �      �      (   
 �       LOGICAL,INPUT pcNameList CHARACTER  highlightWidget       L      |    	      LOGICAL,INPUT pcNameList CHARACTER,INPUT pcHighlightType CHARACTER  resetWidgetValue    \      �      �          LOGICAL,INPUT pcNameList CHARACTER  toggleWidget    �            H    *      LOGICAL,INPUT pcNameList CHARACTER  viewWidget  (      l      �   
 7      LOGICAL,INPUT pcNameList CHARACTER  widgetHandle    x      �      �    B      HANDLE,INPUT pcName CHARACTER   widgetLongcharValue �            @    O      LONGCHAR,INPUT pcName CHARACTER widgetIsBlank          `      �    c      LOGICAL,INPUT pcNameList CHARACTER  widgetIsFocused p      �      �    q      LOGICAL,INPUT pcName CHARACTER  widgetIsModified    �      	      8	    �      LOGICAL,INPUT pcNameList CHARACTER  widgetIsTrue    	      \	      �	    �      LOGICAL,INPUT pcName CHARACTER  widgetValue l	      �	      �	    �      CHARACTER,INPUT pcName CHARACTER    widgetValueList �	      �	      ,
    �      CHARACTER,INPUT pcNameList CHARACTER,INPUT pcDelimiter CHARACTER    d�    G  �
        d       4   ����d                                       ��                  G  K                  	                       G  �
  \  	  H  L                                        3   ����|       O   J  ��  ��  �   addRecord                                 �      ��                  �  �                |G�	                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            cancelRecord                                  �      ��                  �  �                 �b�	                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            copyRecord                                �      ��                  �  �                 e�	                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            disableFields                                 �      ��                       $              �e�	                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  <           ��                            ����                            displayFields                               8         ��                      P              T�	                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  h           ��                            ����                            enableFields                                d  L      ��                    	  |              ���	                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeObject                                l  T      ��                      �              l �	                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            toolbar                             h  P      ��                      �              �	                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �           ��                            ����                            updateState                             �  x      ��                      �              pˑ	                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �           ��                            ����                            valueChanged                                �  �      ��                      �              L0�	                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            viewRecord                              �  �      ��                      �              �0�	                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            getTargetProcedure  
      ,      `    {      HANDLE, getObjectType   @      h      �    �      CHARACTER,  getShowPopup    x      �      �    �      LOGICAL,    setShowPopup    �      �          �      LOGICAL,INPUT plShowPopup LOGICAL   addRecord                               �  �      ��                  �  �  �              ��	                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            cancelRecord                                �  �      ��                  �  �  �              h�	                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            collectChanges                              �  �      ��                  �  �  �              �p�	                    O   ����    e�          O   ����    R�          O   ����    ��            ��   0             �               ��                  $           ��                            ����                            confirmContinue                                      ��                  �  �  8              (�	                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  P           ��                            ����                            confirmDelete                               L  4      ��                  �  �  d              �J�	                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  |           ��                            ����                            confirmExit                             t  \      ��                  �  �  �              tK�	                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �           ��                            ����                            copyRecord                              �  �      ��                  �  �  �              �ԑ	                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            dataAvailable                               �   �       ��                  �  �  �               4�	                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �            ��                            ����                            deleteRecord                                �!  �!      ��                  �  �  �!              �W�	                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeObject                                �"  �"      ��                  �  �  �"              �h�	                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            okToContinueProcedure                               �#  �#      ��                  �  �  �#              �i�	                    O   ����    e�          O   ����    R�          O   ����    ��            ��   D$             $               ��                  8$           ��                            ����                            queryPosition                               4%  %      ��                  �  �  L%              0)�	                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  d%           ��                            ����                            resetRecord                             \&  D&      ��                  �  �  t&              �S�	                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            showDataMessagesProcedure                               l'  T'      ��                  �  �  �'              T��	                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �'           ��                            ����                            updateMode                              �(  |(      ��                  �  �  �(              �4�	                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �(           ��                            ����                            updateRecord                                �)  �)      ��                  �  �  �)              pG�	                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            updateState                             �*  �*      ��                  �  �  �*              �ۑ	                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �*           ��                            ����                            updateTitle                             �+  �+      ��                  �  �   ,              LБ	                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            validateFields                              �,  �,      ��                  �  �  -              �	                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  -           ��                            ����                            getCreateHandles    �      �-      �-    �      CHARACTER,  getDataModified �-      �-      �-    �      LOGICAL,    getDisplayedFields  �-       .      4.    �      CHARACTER,  getDisplayedTables  .      @.      t.    �      CHARACTER,  getEnabledFields    T.      �.      �.     �      CHARACTER,  getEnabledHandles   �.      �.      �.  !        CHARACTER,  getFieldHandles �.       /      0/  "         CHARACTER,  getFieldsEnabled    /      </      p/  #  0      LOGICAL,    getGroupAssignSource    P/      |/      �/  $  A      HANDLE, getGroupAssignSourceEvents  �/      �/      �/  %  V      CHARACTER,  getGroupAssignTarget    �/      0      <0  &  q      CHARACTER,  getGroupAssignTargetEvents  0      H0      �0  '  �      CHARACTER,  getNewRecord    d0      �0      �0  (  �      CHARACTER,  getObjectParent �0      �0      �0  )  �      HANDLE, getRecordState  �0      1      41  *  �      CHARACTER,  getRowIdent 1      @1      l1  +  �      CHARACTER,  getTableIOSource    L1      x1      �1  ,  �      HANDLE, getTableIOSourceEvents  �1      �1      �1  -  �      CHARACTER,  getUpdateTarget �1      �1      (2  .        CHARACTER,  getUpdateTargetNames    2      42      l2  /        CHARACTER,  getWindowTitleField L2      x2      �2  0  &      CHARACTER,  okToContinue    �2      �2      �2  1  :      LOGICAL,INPUT pcAction CHARACTER    setContainerMode    �2      3      @3  2  G      LOGICAL,INPUT pcContainerMode CHARACTER setDataModified  3      h3      �3  3  X      LOGICAL,INPUT plModified LOGICAL    setDisplayedFields  x3      �3      �3  4  h      LOGICAL,INPUT pcFieldList CHARACTER setEnabledFields    �3      4      H4  5  {      LOGICAL,INPUT pcFieldList CHARACTER setGroupAssignSource    (4      l4      �4  6  �      LOGICAL,INPUT phObject HANDLE   setGroupAssignSourceEvents  �4      �4       5  7  �      LOGICAL,INPUT pcEvents CHARACTER    setGroupAssignTarget    �4      $5      \5  8  �      LOGICAL,INPUT pcObject CHARACTER    setGroupAssignTargetEvents  <5      �5      �5  9  �      LOGICAL,INPUT pcEvents CHARACTER    setLogicalObjectName    �5      �5      6  :  �      LOGICAL,INPUT pcLogicalObjectName CHARACTER setObjectParent �5      D6      t6  ;        LOGICAL,INPUT phParent HANDLE   setSaveSource   T6      �6      �6  <        LOGICAL,INPUT plSave LOGICAL    setTableIOSource    �6      �6      7  =        LOGICAL,INPUT phObject HANDLE   setTableIOSourceEvents  �6      87      p7  >  0      LOGICAL,INPUT pcEvents CHARACTER    setUpdateTarget P7      �7      �7  ?  G      LOGICAL,INPUT pcObject CHARACTER    setUpdateTargetNames    �7      �7       8  @  W      LOGICAL,INPUT pcTargetNames CHARACTER   setWindowTitleField  8      H8      |8  A  l      LOGICAL,INPUT cWindowTitleField CHARACTER   showDataMessages    \8      �8      �8  B  �      CHARACTER,  assignPageProperty                              �9  h9      ��                  �  �  �9              4s�	                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �9             �9               ��                  �9           ��                            ����                            changePage                              �:  �:      ��                  �  �  �:              8��	                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            confirmExit                             �;  �;      ��                  �  �  �;              0.�	                    O   ����    e�          O   ����    R�          O   ����    ��            ��                   <           ��                            ����                            constructObject                             �<  �<      ��                  �  �  =              '�	                    O   ����    e�          O   ����    R�          O   ����    ��            ��   `=             ,=               �� 
  �=             T=  
             ��   �=             |=               �� 
                 �=  
         ��                            ����                            createObjects                               �>  �>      ��                       �>              P��	                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            deletePage                              �?  �?      ��                      �?              ̉�	                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �?           ��                            ����                            destroyObject                               �@  �@      ��                      �@              L��	                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            hidePage                                �A  �A      ��                  
    �A              D��	                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �A           ��                            ����                            initializeObject                                �B  �B      ��                      C              ���	                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeVisualContainer                               D  �C      ��                      $D              `B�	                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initPages                               E  �D      ��                      $E              �B�	                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  <E           ��                            ����                            notifyPage                              4F  F      ��                      LF              \C�	                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  dF           ��                            ����                            passThrough                             \G  DG      ��                      tG              ��	                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �G             �G               ��                  �G           ��                            ����                            removePageNTarget                               �H  �H      ��                  !  $  �H              0��	                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  I             �H  
             ��                  I           ��                            ����                            selectPage                              J  �I      ��                  &  (  J              L��	                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  4J           ��                            ����                            toolbar                             (K  K      ��                  *  ,  @K              ���	                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  XK           ��                            ����                            viewObject                              PL  8L      ��                  .  /  hL              ���	                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            viewPage                                PM  8M      ��                  1  3  hM              T��	                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �M           ��                            ����                            disablePagesInFolder    �8      �M       N  C  �      LOGICAL,INPUT pcPageInformation CHARACTER   enablePagesInFolder  N      LN      �N  D  �      LOGICAL,INPUT pcPageInformation CHARACTER   getCallerProcedure  `N      �N      �N  E  �      HANDLE, getCallerWindow �N      �N      O  F  �      HANDLE, getContainerMode    �N       O      TO  G  �      CHARACTER,  getContainerTarget  4O      `O      �O  H  �      CHARACTER,  getContainerTargetEvents    tO      �O      �O  I        CHARACTER,  getCurrentPage  �O      �O      P  J        INTEGER,    getDisabledAddModeTabs  �O      $P      \P  K  )      CHARACTER,  getDynamicSDOProcedure  <P      hP      �P  L  @      CHARACTER,  getFilterSource �P      �P      �P  M  W      HANDLE, getMultiInstanceActivated   �P      �P       Q  N  g      LOGICAL,    getMultiInstanceSupported    Q      ,Q      hQ  O  �      LOGICAL,    getNavigationSource HQ      tQ      �Q  P  �      CHARACTER,  getNavigationSourceEvents   �Q      �Q      �Q  Q  �      CHARACTER,  getNavigationTarget �Q      �Q      0R  R  �      HANDLE, getOutMessageTarget R      8R      lR  S  �      HANDLE, getPageNTarget  LR      tR      �R  T  �      CHARACTER,  getPageSource   �R      �R      �R  U         HANDLE, getPrimarySdoTarget �R      �R      S  V        HANDLE, getReEnableDataLinks    �R      $S      \S  W  "      CHARACTER,  getRunDOOptions <S      hS      �S  X  7      CHARACTER,  getRunMultiple  xS      �S      �S  Y  G      LOGICAL,    getSavedContainerMode   �S      �S      T  Z  V      CHARACTER,  getSdoForeignFields �S      $T      XT  [  l      CHARACTER,  getTopOnly  8T      dT      �T  \ 
 �      LOGICAL,    getUpdateSource pT      �T      �T  ]  �      CHARACTER,  getWaitForObject    �T      �T      U  ^  �      HANDLE, getWindowTitleViewer    �T      U      LU  _  �      HANDLE, getStatusArea   ,U      TU      �U  `  �      LOGICAL,    pageNTargets    dU      �U      �U  a  �      CHARACTER,INPUT phTarget HANDLE,INPUT piPageNum INTEGER setCallerObject �U      �U      (V  b  �      LOGICAL,INPUT h HANDLE  setCallerProcedure  V      @V      tV  c  �      LOGICAL,INPUT h HANDLE  setCallerWindow TV      �V      �V  d  �      LOGICAL,INPUT h HANDLE  setContainerTarget  �V      �V      W  e        LOGICAL,INPUT pcObject CHARACTER    setCurrentPage  �V      ,W      \W  f  "      LOGICAL,INPUT iPage INTEGER setDisabledAddModeTabs  <W      xW      �W  g  1      LOGICAL,INPUT cDisabledAddModeTabs CHARACTER    setDynamicSDOProcedure  �W      �W      X  h  H      LOGICAL,INPUT pcProc CHARACTER  setFilterSource �W      8X      hX  i  _      LOGICAL,INPUT phObject HANDLE   setInMessageTarget  HX      �X      �X  j  o      LOGICAL,INPUT phObject HANDLE   setMultiInstanceActivated   �X      �X      Y  k  �      LOGICAL,INPUT lMultiInstanceActivated LOGICAL   setMultiInstanceSupported   �X      HY      �Y  l  �      LOGICAL,INPUT lMultiInstanceSupported LOGICAL   setNavigationSource dY      �Y      �Y  m  �      LOGICAL,INPUT pcSource CHARACTER    setNavigationSourceEvents   �Y      Z      HZ  n  �      LOGICAL,INPUT pcEvents CHARACTER    setNavigationTarget (Z      lZ      �Z  o  �      LOGICAL,INPUT cTarget CHARACTER setOutMessageTarget �Z      �Z      �Z  p  �      LOGICAL,INPUT phObject HANDLE   setPageNTarget  �Z      [      D[  q  	      LOGICAL,INPUT pcObject CHARACTER    setPageSource   $[      h[      �[  r  	      LOGICAL,INPUT phObject HANDLE   setPrimarySdoTarget x[      �[      �[  s  )	      LOGICAL,INPUT hPrimarySdoTarget HANDLE  setReEnableDataLinks    �[      \      L\  t  =	      LOGICAL,INPUT cReEnableDataLinks CHARACTER  setRouterTarget ,\      x\      �\  u  R	      LOGICAL,INPUT phObject HANDLE   setRunDOOptions �\      �\      �\  v  b	      LOGICAL,INPUT pcOptions CHARACTER   setRunMultiple  �\      ]      L]  w  r	      LOGICAL,INPUT plMultiple LOGICAL    setSavedContainerMode   ,]      p]      �]  x  �	      LOGICAL,INPUT cSavedContainerMode CHARACTER setSdoForeignFields �]      �]      ^  y  �	      LOGICAL,INPUT cSdoForeignFields CHARACTER   setTopOnly  �]      4^      `^  z 
 �	      LOGICAL,INPUT plTopOnly LOGICAL setUpdateSource @^      �^      �^  {  �	      LOGICAL,INPUT pcSource CHARACTER    setWaitForObject    �^      �^      _  |  �	      LOGICAL,INPUT phObject HANDLE   setWindowTitleViewer    �^      (_      `_  }  �	      LOGICAL,INPUT phViewer HANDLE   setStatusArea   @_      �_      �_  ~  �	      LOGICAL,INPUT plStatusArea LOGICAL  applyLayout                             d`  L`      ��                  �  �  |`              p֓	                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            disableObject                               ha  Pa      ��                  �  �  �a              ד	                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            enableObject                                lb  Tb      ��                  �  �  �b              �ד	                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeObject                                tc  \c      ��                  �  �  �c              ���	                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            processAction                               xd  `d      ��                  �  �  �d              ���	                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �d           ��                            ����                            getAllFieldHandles  �_      e      De    �	      CHARACTER,  getAllFieldNames    $e      Pe      �e  �  
      CHARACTER,  getCol  de      �e      �e  �  
      DECIMAL,    getDefaultLayout    �e      �e      �e  �  %
      CHARACTER,  getDisableOnInit    �e      f      8f  �  6
      LOGICAL,    getEnabledObjFlds   f      Df      xf  �  G
      CHARACTER,  getEnabledObjHdls   Xf      �f      �f  �  Y
      CHARACTER,  getHeight   �f      �f      �f  � 	 k
      DECIMAL,    getHideOnInit   �f      �f      ,g  �  u
      LOGICAL,    getLayoutOptions    g      8g      lg  �  �
      CHARACTER,  getLayoutVariable   Lg      xg      �g  �  �
      CHARACTER,  getObjectEnabled    �g      �g      �g  �  �
      LOGICAL,    getObjectLayout �g      �g      (h  �  �
      CHARACTER,  getRow  h      4h      \h  �  �
      DECIMAL,    getWidth    <h      hh      �h  �  �
      DECIMAL,    getResizeHorizontal th      �h      �h  �  �
      LOGICAL,    getResizeVertical   �h      �h      i  �  �
      LOGICAL,    setAllFieldHandles  �h       i      Ti  �  �
      LOGICAL,INPUT pcValue CHARACTER setAllFieldNames    4i      ti      �i  �        LOGICAL,INPUT pcValue CHARACTER setDefaultLayout    �i      �i      �i  �  !      LOGICAL,INPUT pcDefault CHARACTER   setDisableOnInit    �i       j      Tj  �  2      LOGICAL,INPUT plDisable LOGICAL setHideOnInit   4j      tj      �j  �  C      LOGICAL,INPUT plHide LOGICAL    setLayoutOptions    �j      �j      �j  �  Q      LOGICAL,INPUT pcOptions CHARACTER   setObjectLayout �j      k      Lk  �  b      LOGICAL,INPUT pcLayout CHARACTER    setResizeHorizontal ,k      pk      �k  �  r      LOGICAL,INPUT plResizeHorizontal LOGICAL    setResizeVertical   �k      �k      l  �  �      LOGICAL,INPUT plResizeVertical LOGICAL  getObjectTranslated �k      ,l      `l  �  �      LOGICAL,    getObjectSecured    @l      ll      �l  �  �      LOGICAL,    createUiEvents  �l      �l      �l  �  �      LOGICAL,    bindServer                              xm  `m      ��                  �  �  �m              �m�	                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            destroyObject                               |n  dn      ��                  �  �  �n              �X�	                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            destroyServerObject                             �o  lo      ��                  �  �  �o              �Y�	                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            disconnectObject                                �p  tp      ��                  �  �  �p              ���	                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeServerObject                              �q  �q      ��                  �  �  �q              ���	                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            restartServerObject                             �r  �r      ��                  �  �  �r              ��	                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            runServerObject                             �s  �s      ��                  �  �  �s              ���	                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
                 �s  
         ��                            ����                            startServerObject                               �t  �t      ��                  �  �  �t              ���	                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            unbindServer                                �u  �u      ��                  �  �  �u               �	                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  v           ��                            ����                            getAppService   �l      pv      �v  �  �      CHARACTER,  getASBound  �v      �v      �v  � 
 �      LOGICAL,    getAsDivision   �v      �v      w  �  �      CHARACTER,  getASHandle �v       w      Lw  �  �      HANDLE, getASHasStarted ,w      Tw      �w  �  �      LOGICAL,    getASInfo   dw      �w      �w  � 	       CHARACTER,  getASInitializeOnRun    �w      �w       x  �        LOGICAL,    getASUsePrompt  �w      x      <x  �  .      LOGICAL,    getServerFileName   x      Hx      |x  �  =      CHARACTER,  getServerOperatingMode  \x      �x      �x  �  O      CHARACTER,  runServerProcedure  �x      �x       y  �  f      HANDLE,INPUT pcServerFileName CHARACTER,INPUT phAppService HANDLE   setAppService   �x      Dy      ty  �  y      LOGICAL,INPUT pcAppService CHARACTER    setASDivision   Ty      �y      �y  �  �      LOGICAL,INPUT pcDivision CHARACTER  setASHandle �y      �y      z  �  �      LOGICAL,INPUT phASHandle HANDLE setASInfo   �y      <z      hz  � 	 �      LOGICAL,INPUT pcInfo CHARACTER  setASInitializeOnRun    Hz      �z      �z  �  �      LOGICAL,INPUT plInitialize LOGICAL  setASUsePrompt  �z      �z      {  �  �      LOGICAL,INPUT plFlag LOGICAL    setServerFileName   �z      4{      h{  �  �      LOGICAL,INPUT pcFileName CHARACTER  setServerOperatingMode  H{      �{      �{  �  �      LOGICAL,INPUT pcServerOperatingMode CHARACTER   addLink                             �|  h|      ��                  }  �  �|              $�	                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  �|             �|  
             ��   }             �|               �� 
                  }  
         ��                            ����                            addMessage                              �}  �}      ��                  �  �  ~              ��	                    O   ����    e�          O   ����    R�          O   ����    ��            ��   \~             (~               ��   �~             P~               ��                  x~           ��                            ����                            adjustTabOrder                              t  \      ��                  �  �  �              ���	                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  �             �  
             �� 
   �             �  
             ��                  �           ��                            ����                            applyEntry                              �  Ԁ      ��                  �  �  �              d��	                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �           ��                            ����                            changeCursor                                �   �      ��                  �  �  0�              �C�	                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  H�           ��                            ����                            createControls                              D�  ,�      ��                  �  �  \�              LD�	                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            destroyObject                               H�  0�      ��                  �  �  `�              �D�	                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            displayLinks                                L�  4�      ��                  �  �  d�              �n�	                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            editInstanceProperties                              X�  @�      ��                  �  �  p�              Lo�	                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            exitObject                              X�  @�      ��                  �  �  p�               p�	                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            hideObject                              X�  @�      ��                  �  �  p�              �	                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeObject                                `�  H�      ��                  �  �  x�              ��	                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            modifyListProperty                              h�  P�      ��                  �  �  ��              T4�	                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  ̊             ��  
             ��   �             ��               ��   �             �               ��                  �           ��                            ����                            modifyUserLinks                             �  �      ��                  �  �  $�              (��	                    O   ����    e�          O   ����    R�          O   ����    ��            ��   p�             <�               ��   ��             d�               �� 
                 ��  
         ��                            ����                            removeAllLinks                              ��  p�      ��                  �  �  ��              D��	                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            removeLink                              ��  p�      ��                  �  �  ��              ��	                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  �             ��  
             ��   �             ��               �� 
                 �  
         ��                            ����                            repositionObject                                �  ��      ��                  �  �   �              �?�	                    O   ����    e�          O   ����    R�          O   ����    ��            ��   l�             8�               ��                  `�           ��                            ����                            returnFocus                             X�  @�      ��                  �  �  p�              �A�	                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
                 ��  
         ��                            ����                            showMessageProcedure                                ��  t�      ��                  �  �  ��              ��	                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �             ��               ��                  �           ��                            ����                            toggleData                              ܓ  ē      ��                  �  �  ��              h��	                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �           ��                            ����                            viewObject                              �  �      ��                  �  �  �              ��	                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            anyMessage  �{      t�      ��  � 
 F      LOGICAL,    assignLinkProperty  ��      ��      ��  �  Q      LOGICAL,INPUT pcLink CHARACTER,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER   fetchMessages   ��      8�      h�  �  d      CHARACTER,  getChildDataKey H�      t�      ��  �  r      CHARACTER,  getContainerHandle  ��      ��      �  �  �      HANDLE, getContainerHidden  Ė      �       �  �  �      LOGICAL,    getContainerSource   �      ,�      `�  �  �      HANDLE, getContainerSourceEvents    @�      h�      ��  �  �      CHARACTER,  getContainerType    ��      ��      �  �  �      CHARACTER,  getDataLinksEnabled ė      �      $�  �  �      LOGICAL,    getDataSource   �      0�      `�  �  �      HANDLE, getDataSourceEvents @�      h�      ��  �        CHARACTER,  getDataSourceNames  |�      ��      ܘ  �        CHARACTER,  getDataTarget   ��      �      �  �  .      CHARACTER,  getDataTargetEvents ��      $�      X�  �  <      CHARACTER,  getDBAware  8�      d�      ��  � 
 P      LOGICAL,    getDesignDataObject p�      ��      Й  �  [      CHARACTER,  getDynamicObject    ��      ܙ      �  �  o      LOGICAL,    getInstanceProperties   �      �      T�  �  �      CHARACTER,  getLogicalObjectName    4�      `�      ��  �  �      CHARACTER,  getLogicalVersion   x�      ��      ؚ  �  �      CHARACTER,  getObjectHidden ��      �      �  �  �      LOGICAL,    getObjectInitialized    ��       �      X�  �  �      LOGICAL,    getObjectName   8�      d�      ��  �  �      CHARACTER,  getObjectPage   t�      ��      Л  �  �      INTEGER,    getObjectVersion    ��      ܛ      �  �  �      CHARACTER,  getObjectVersionNumber  �      �      T�  �        CHARACTER,  getParentDataKey    4�      `�      ��  �  &      CHARACTER,  getPassThroughLinks t�      ��      Ԝ  �  7      CHARACTER,  getPhysicalObjectName   ��      ��      �  �  K      CHARACTER,  getPhysicalVersion  ��      $�      X�  �  a      CHARACTER,  getPropertyDialog   8�      d�      ��  �  t      CHARACTER,  getQueryObject  x�      ��      ԝ  �  �      LOGICAL,    getRunAttribute ��      ��      �  �  �      CHARACTER,  getSupportedLinks   �      �      P�  �  �      CHARACTER,  getTranslatableProperties   0�      \�      ��  �  �      CHARACTER,  getUIBMode  x�      ��      О  � 
 �      CHARACTER,  getUserProperty ��      ܞ      �  �  �      CHARACTER,INPUT pcPropName CHARACTER    instancePropertyList    �      4�      l�  �  �      CHARACTER,INPUT pcPropList CHARACTER    linkHandles L�      ��      ��  �        CHARACTER,INPUT pcLink CHARACTER    linkProperty    ��      �      �  �        CHARACTER,INPUT pcLink CHARACTER,INPUT pcPropName CHARACTER mappedEntry ��      P�      |�  �        CHARACTER,INPUT pcEntry CHARACTER,INPUT pcList CHARACTER,INPUT plFirst LOGICAL,INPUT pcDelimiter CHARACTER  messageNumber   \�      �      �  �  &      CHARACTER,INPUT piMessage INTEGER   propertyType    ��      <�      l�  �  4      CHARACTER,INPUT pcPropName CHARACTER    reviewMessages  L�      ��      ġ  �  A      CHARACTER,  setChildDataKey ��      С       �  �  P      LOGICAL,INPUT cChildDataKey CHARACTER   setContainerHidden  �      (�      \�  �  `      LOGICAL,INPUT plHidden LOGICAL  setContainerSource  <�      |�      ��  �  s      LOGICAL,INPUT phObject HANDLE   setContainerSourceEvents    ��      Т      �  �  �      LOGICAL,INPUT pcEvents CHARACTER    setDataLinksEnabled �      0�      d�  �  �      LOGICAL,INPUT lDataLinksEnabled LOGICAL setDataSource   D�      ��      ��  �  �      LOGICAL,INPUT phObject HANDLE   setDataSourceEvents ��      ܣ      �  �  �      LOGICAL,INPUT pcEventsList CHARACTER    setDataSourceNames  �      8�      l�  �  �      LOGICAL,INPUT pcSourceNames CHARACTER   setDataTarget   L�      ��      Ĥ  �  �      LOGICAL,INPUT pcTarget CHARACTER    setDataTargetEvents ��      �      �  �  �      LOGICAL,INPUT pcEvents CHARACTER    setDBAware  ��      @�      l�  � 
 
      LOGICAL,INPUT lAware LOGICAL    setDesignDataObject L�      ��      ��  �        LOGICAL,INPUT pcDataObject CHARACTER    setDynamicObject    ��      �      �  �  )      LOGICAL,INPUT lTemp LOGICAL setInstanceProperties   ��      8�      p�  �  :      LOGICAL,INPUT pcPropList CHARACTER  setLogicalVersion   P�      ��      Ȧ  �  P      LOGICAL,INPUT cVersion CHARACTER    setObjectName   ��      �      �  �  b      LOGICAL,INPUT pcName CHARACTER  setObjectVersion    ��      <�      p�  �  p      LOGICAL,INPUT cObjectVersion CHARACTER  setParentDataKey    P�      ��      ̧  �  �      LOGICAL,INPUT cParentDataKey CHARACTER  setPassThroughLinks ��      ��      (�  �  �      LOGICAL,INPUT pcLinks CHARACTER setPhysicalObjectName   �      H�      ��  �  �      LOGICAL,INPUT cTemp CHARACTER   setPhysicalVersion  `�      ��      Ԩ  �  �      LOGICAL,INPUT cVersion CHARACTER    setRunAttribute ��      ��      (�  �  �      LOGICAL,INPUT cRunAttribute CHARACTER   setSupportedLinks   �      P�      ��  �  �      LOGICAL,INPUT pcLinkList CHARACTER  setTranslatableProperties   d�      ��      �  �  �      LOGICAL,INPUT pcPropList CHARACTER  setUIBMode  ĩ      �      4�  � 
       LOGICAL,INPUT pcMode CHARACTER  setUserProperty �      T�      ��  �        LOGICAL,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER  showmessage d�      Ī      �  �  &      LOGICAL,INPUT pcMessage CHARACTER   Signature   Ъ      �      @�  � 	 2      CHARACTER,INPUT pcName CHARACTER    8�    �	  ��  ��      �       4   �����                 �                      ��                  �	  
                  �	                       �	  ��        �	  (�  ��      �       4   �����                 ��                      ��                  �	  
                  ��	                       �	  8�  ��    
  Ь  L�      �       4   �����                 \�                      ��                  
  
                  ��	                       
  �         
                                  d     
                    � ߱        �  $  
  ��  ���                           $  
  �  ���                       �                         � ߱        D�    
  T�  Ю      �      4   �����                �                      ��                  
  �
                  ���	                       
  d�  �  o   !
      ,                                 l�  $   "
  @�  ���                       4  @                        � ߱        ��  �   #
  T      ��  �   $
  �      ��  �   &
  <      ��  �   (
  �      Я  �   *
  $      �  �   ,
  �      ��  �   -
        �  �   .
  P       �  �   1
  �      4�  �   3
  8      H�  �   4
  �      \�  �   6
  0      p�  �   7
  �      ��  �   8
  �      ��  �   9
  d      ��  �   :
  �      ��  �   @
  	      ԰  �   B
  �	      �  �   H
  �	      ��  �   J
  8
      �  �   L
  �
      $�  �   M
  (      8�  �   S
  �      L�  �   T
        `�  �   U
  �      t�  �   V
        ��  �   Y
  |      ��  �   Z
  �      ��  �   \
  ,      ı  �   ]
  h      ر  �   _
  �      �  �   `
         �  �   a
  T      �  �   b
  �      (�  �   c
  �      <�  �   d
  H      P�  �   e
  �      d�  �   g
  �      x�  �   h
  �      ��  �   i
  8      ��  �   k
  t      ��  �   l
  �      Ȳ  �   m
  �      ܲ  �   n
  (          �   o
  d                      �          t�  \�      ��                  	  7  ��              ,��	                    O   ����    e�          O   ����    R�          O   ����    ��      �     
                P                     `                         � ߱        4�  $   ��  ���                           O   5  ��  ��  �               ��          ��  ��    ��                                             ��                            ����                                h      �      L�     T     ��                       ��  �                     �    W  `�  ܵ      �      4   �����                �                      ��                  X  �                  �g�	                       X  p�   �  �   [        �  �   \  �      (�  �   ]  �      <�  �   ^  x      P�  �   _  �      d�  �   `  p      x�  �   a  �      ��  �   b  `      ��  �   c  �      ��  �   d  X      ȶ  �   e  �      ܶ  �   f  H      �  �   g  �          �   h  @      ܹ    �   �  ��      �      4   �����                ��                      ��                  �  x                  |R�	                       �  0�  ��  �   �        Է  �   �  �      �  �   �  �      ��  �   �  t      �  �   �  �      $�  �   �  \      8�  �   �  �      L�  �   �  L      `�  �   �  �      t�  �   �  4       ��  �   �  �       ��  �   �  $!      ��  �   �  �!      ĸ  �   �  "      ظ  �   �  �"      �  �   �  #       �  �   �  �#      �  �   �  $      (�  �   �  �$      <�  �   �  �$      P�  �      x%      d�  �     �%      x�  �     p&      ��  �     �&      ��  �     h'      ��  �     �'      ȹ  �     `(          �     �(      ��    �  ��  t�      D)      4   ����D)                ��                      ��                  �  6                  �T�	                       �  �  ��  �   �  �)      ��  �   �   *      ��  �   �  �*      Ժ  �   �  +      �  �   �  �+      ��  �   �  �+      �  �   �  l,      $�  �   �  �,      8�  �   �  -      L�  �   �  X-      `�  �   �  �-      t�  �   �  .      ��  �   �  |.      ��  �   �  �.      ��  �   �  l/      Ļ  �   �  �/      ػ  �   �  T0      �  �   �  �0       �  �   �  L1      �  �   �  �1      (�  �   �  �1      <�  �   �  p2      P�  �   �  �2      d�  �   �   3      x�  �   �  \3      ��  �   �  �3      ��  �   �  4      ��  �   �  P4      ȼ  �   �  �4      ܼ  �   �  �4      �  �   �  5      �  �   �  @5      �  �   �  |5      ,�  �   �  �5      @�  �   �  ,6      T�  �   �  h6      h�  �   �  �6      |�  �   �  �6      ��  �   �  7      ��  �   �  X7      ��  �   �  �7      ̽  �   �  8      �  �   �  |8      ��  �   �  �8      �  �   �  d9      �  �   �  �9      0�  �   �  \:      D�  �   �  �:      X�  �   �  T;      l�  �   �  �;      ��  �   �  L<      ��  �   �  �<      ��  �   �  =      ��  �   �  @=      о  �   �  |=      �  �   �  �=          �   �  ,>      �    D  �  ��      �>      4   �����>  	              ��                      ��             	     E  �                  �ړ	                       E  $�  ��  �   G  �>      ȿ  �   H  h?      ܿ  �   I  �?      �  �   J  X@      �  �   P  �@      �  �   Q  hA      ,�  �   R  �A      @�  �   S  PB      T�  �   T  �B      h�  �   U  HC      |�  �   V  �C      ��  �   W  8D      ��  �   X  tD      ��  �   Z  �D      ��  �   [  \E      ��  �   \  �E      ��  �   ]  DF      �  �   ^  �F      �  �   _  ,G      0�  �   `  �G      D�  �   a  H      X�  �   b  �H      l�  �   c  I      ��  �   d  �I      ��  �   e  �I      ��  �   g  0J      ��  �   h  �J      ��  �   j  K      ��  �   k  �K      ��  �   l  L          �   m  �L      ��    �  (�  ��      �L      4   �����L  
              ��                      ��             
     �  b                  ���	                       �  8�  ��  �   �  M      ��  �   �  �M          �   �  N      ��    $  �  ��      <N      4   ����<N                ��                      ��                  %  .                  ��	                       %  �  �    '  ��  ��      TN      4   ����TN      $  (  ��  ���                       �N  @         �N              � ߱              +  8�  H�      �N      4   �����N      $  ,  t�  ���                       O  @         �N              � ߱        ��  $  6  ��  ���                       <O     
  	       	           � ߱        ��    o  �  $�      PO      4   ����PO      /   p  P�     `�                          3   ����`O            ��                      3   �����O  ��    y  ��  (�  �  �O      4   �����O                8�                      ��                  z  �                  �b�	                       z  ��  L�  �   ~  �O      ��  $    x�  ���                       (P     
                    � ߱        ��  �   �  HP      �  $   �  ��  ���                       pP  @         \P              � ߱        ��  $  �  <�  ���                       �P       
       
           � ߱        �Q     
                R                     \S  @        
 S              � ߱        \�  V   �  h�  ���                        hS       
       
       �S                     �S       
       
           � ߱        ��  $  �  ��  ���                       �T     
                U                     dV  @        
 $V              � ߱        |�  V   �  ��  ���                        pV     
                �V                     <X  @        
 �W              � ߱            V   �  �  ���                                      ��                      ��                     �                   d�	                          ��  PX     
                �X                     Z  @        
 �Y          �Z  @        
 DZ          �Z  @        
 �Z          D[  @        
 [              � ߱            V     $�  ���                        adm-clone-props �  �              �     U     `                          \  �"                     start-super-proc    �  t�  �           �     V                                  #                     |�    �   �  �      �^      4   �����^      /   �  <�     L�                          3   �����^            l�                      3   ���� _  ��  $  �  ��  ���                        _                         � ߱        ��    �  ��  l�  �  <_      4   ����<_                ��                      ��                  �  �                  �[�	                       �   �  P_                     d_                     x_                         � ߱            $  �  |�  ���                             �  (�  d�      �_      4   �����_  �_                         � ߱            $  �  8�  ���                       ��    �  ��  ��  �  �_      4   �����_      $  �  ��  ���                       �_                         � ߱            �     �_      8`     
                �`                     b  @        
 �a              � ߱        ��  V      (�  ���                        ��  �   S  b      d�    �  ��  ��      Pb      4   ����Pb      /   �  $�     4�                          3   ����`b            T�                      3   �����b  D�    =  ��  ��      �b      4   �����b                �                      ��                  >  A                  �Д	                       >  ��      g   ?  $�         k���                           ��          ��  ��      ��                  @      ��              є	                    O   ����    e�          O   ����    R�          O   ����    ��          /  @  �     (�  �b                      3   �����b  X�     
   H�                      3   �����b         
   x�                      3   �����b    ��                              ��        �                  ����                                        8�              W      ��                      g                               L�  g   C  \�          k�	��                           $�          ��  ��      ��                  C  E  �              �є	                    O   ����    e�          O   ����    R�          O   ����    ��          /  D  P�     `�  �b                      3   �����b            ��                      3   ����c    ��                              ��        �                  ����                                        p�              X      ��                      g                               T�  g   G  d�          k�	��                           ,�          ��  ��      ��                  G  I  �              HҔ	                    O   ����    e�          O   ����    R�          O   ����    ��          /  H  X�     h�  <c                      3   ���� c            ��                      3   ����Dc    ��                              ��        �                  ����                                        x�              Y      ��                      g                               ��    `  p�  ��      `c      4   ����`c                ��                      ��                  a  �                  HS`	                       a  ��  h�  /   b  (�     8�                          3   ����pc            X�                      3   �����c  d�  /  d  ��     ��  �c                      3   �����c  ��     
   ��                      3   �����c  �        ��                      3   �����c  4�        $�                      3   �����c            T�                      3   ����d  ��    l  ��  ��      8d      4   ����8d      /  r  ��     ��  �d                      3   �����d  ��     
   ��                      3   �����d  ,�        �                      3   �����d  \�        L�                      3   �����d            |�                      3   ����e        x  ��  ��      (e      4   ����(e      /  {  ��     ��  |e                      3   ����\e  $�     
   �                      3   �����e  T�        D�                      3   �����e  ��        t�                      3   �����e            ��                      3   �����e  L�     �  �e                                     �e     
                pf                     �g  @        
 �g              � ߱        ��  V   �  ��  ���                        �g     
                Ph                     �i  @        
 `i              � ߱        �  V   !  x�  ���                        ��    R  $�  ��      �i      4   �����i                ��                      ��                  S  X                  H�b	                       S  4�  �  /   T  ��     ��                          3   �����i            �                      3   �����i      /   V  H�     X�                          3   ���� j  ��     
   x�                      3   ���� j  ��        ��                      3   ����(j  ��        ��                      3   ����<j            �                      3   ����Xj  displayObjects  ��  �                      Z      �                               �$                     \�  g   �  ��         k4 �                           d�          4�  �      ��                  �      L�              ���	                    O   ����    e�          O   ����    R�          O   ����    ��          /  �  ��         �j                      3   ����tj    ��                              ��        �                  ����                                        ��              [      ��                      g                               �  g     t�          k0��      }                      <�          �  ��      ��                        $�              Ѐ�	                    O   ����    e�          O   ����    R�          O   ����    ��          /    h�         �j                      3   �����j    ��                            ����                                        ��              \      x�                      g                               ��      0�  ��      �j      4   �����j                ��                      ��                                      L��	                         @�  (�  /   	  ��     ��                          3   �����j            �                      3   �����j      /  
  T�     d�  (k                      3   ����k  ��     
   ��                      3   ����0k  ��        ��                      3   ����8k  ��        ��                      3   ����Lk            �                      3   ����lk  �k                     �k                     �k                     8l                         � ߱        l�  $    $�  ���                       �l     
                m                     Xn  @        
 n          �n  @        
 pn          o  @        
 �n              � ߱        ��  V   $  ��  ���                        0o  @         o          Xo  @         Do              � ߱            $     ��  ���                       adm-create-objects  (�  �              �     ]     $                             G(                     disable_UI   �  |�                      ^                                    Z(  
                    �  �   �����  �                  8   ����       8   ����       4�  @�      toggleData  ,INPUT plEnabled LOGICAL    $�  l�  ��      showMessageProcedure    ,INPUT pcMessage CHARACTER,OUTPUT plAnswer LOGICAL  \�  ��  ��      returnFocus ,INPUT hTarget HANDLE   ��  ��  �      repositionObject    ,INPUT pdRow DECIMAL,INPUT pdCol DECIMAL    ��  L�  X�      removeLink  ,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE <�  ��  ��      removeAllLinks  ,   ��  ��  ��      modifyUserLinks ,INPUT pcMod CHARACTER,INPUT pcLinkName CHARACTER,INPUT phObject HANDLE ��  8�  L�      modifyListProperty  ,INPUT phCaller HANDLE,INPUT pcMode CHARACTER,INPUT pcListName CHARACTER,INPUT pcListValue CHARACTER    (�  ��  ��      hideObject  ,   ��  ��  ��      exitObject  ,   ��  �  �      editInstanceProperties  ,   ��  0�  @�      displayLinks    ,    �  T�  d�      createControls  ,   D�  x�  ��      changeCursor    ,INPUT pcCursor CHARACTER   h�  ��  ��      applyEntry  ,INPUT pcField CHARACTER    ��  ��  ��      adjustTabOrder  ,INPUT phObject HANDLE,INPUT phAnchor HANDLE,INPUT pcPosition CHARACTER ��  T�  `�      addMessage  ,INPUT pcText CHARACTER,INPUT pcField CHARACTER,INPUT pcTable CHARACTER D�  ��  ��      addLink ,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE ��  �  $�      unbindServer    ,INPUT pcMode CHARACTER �  L�  `�      startServerObject   ,   <�  t�  ��      runServerObject ,INPUT phAppService HANDLE  d�  ��  ��      restartServerObject ,   ��  ��  ��      initializeServerObject  ,   ��  �  �      disconnectObject    ,   ��  ,�  @�      destroyServerObject ,   �  T�  `�      bindServer  ,   D�  t�  ��      processAction   ,INPUT pcAction CHARACTER   d�  ��  ��      enableObject    ,   ��  ��  ��      disableObject   ,   ��  ��  �      applyLayout ,   ��  �  $�      viewPage    ,INPUT piPageNum INTEGER    �  P�  \�      viewObject  ,   @�  p�  |�      selectPage  ,INPUT piPageNum INTEGER    `�  ��  ��      removePageNTarget   ,INPUT phTarget HANDLE,INPUT piPage INTEGER ��  ��  �      passThrough ,INPUT pcLinkName CHARACTER,INPUT pcArgument CHARACTER  ��  L�  X�      notifyPage  ,INPUT pcProc CHARACTER <�  ��  ��      initPages   ,INPUT pcPageList CHARACTER p�  ��  ��      initializeVisualContainer   ,   ��  ��  ��      hidePage    ,INPUT piPageNum INTEGER    ��   �  0�      destroyObject   ,   �  D�  P�      deletePage  ,INPUT piPageNum INTEGER    4�  |�  ��      createObjects   ,   l�  ��  ��      constructObject ,INPUT pcProcName CHARACTER,INPUT phParent HANDLE,INPUT pcPropList CHARACTER,OUTPUT phObject HANDLE ��  $�  0�      changePage  ,   �  D�  X�      assignPageProperty  ,INPUT pcProp CHARACTER,INPUT pcValue CHARACTER 4�  ��  ��      validateFields  ,INPUT-OUTPUT pcNotValidFields CHARACTER    ��  ��  ��      updateTitle ,   ��  �  �      updateRecord    ,   ��  (�  4�      updateMode  ,INPUT pcMode CHARACTER �  \�  x�      showDataMessagesProcedure   ,OUTPUT pcReturn CHARACTER  L�  ��  ��      resetRecord ,   ��  ��  ��      queryPosition   ,INPUT pcState CHARACTER    ��   �  �      okToContinueProcedure   ,INPUT pcAction CHARACTER,OUTPUT plAnswer LOGICAL   ��  \�  l�      deleteRecord    ,   L�  ��  ��      dataAvailable   ,INPUT pcRelative CHARACTER p�  ��  ��      confirmExit ,INPUT-OUTPUT plCancel LOGICAL  ��  ��  �      confirmDelete   ,INPUT-OUTPUT plAnswer LOGICAL  ��  8�  H�      confirmContinue ,INPUT-OUTPUT plCancel LOGICAL  (�  x�  ��      collectChanges  ,INPUT-OUTPUT pcChanges CHARACTER,INPUT-OUTPUT pcInfo CHARACTER h�  ��  ��      viewRecord  ,   ��  ��  �      valueChanged    ,   ��  �  (�      updateState ,INPUT pcState CHARACTER    �  T�  \�      toolbar ,INPUT pcValue CHARACTER    D�  ��  ��      initializeObject    ,   x�  ��  ��      enableFields    ,   ��  ��  ��      displayFields   ,INPUT pcColValues CHARACTER    ��  �  $�      disableFields   ,INPUT pcFieldType CHARACTER    �  T�  `�      copyRecord  ,   D�  t�  ��      cancelRecord    ,   d�  ��  ��      addRecord   ,        � 
"     
 �	%     adecomm/as-utils.w 
"   
   �    }        �
"     
    �     }        �� @  :   %               � 
" 	   
 �%              � �  �         `      $              
�    � <   �     
�             �G                      
�            � >   �
" 	   
 �	
�H T   %              �     }        �GG %              � 
"   
   P �L 
�H T   %              �     }        �GG %              
"   
   �            7%               
"   
 ��           H    1� N  
 �� Y   �%               o%   o           � ^    �
"   
 ��           �    1� _   �� Y   �%               o%   o           � m   �
"   
 ��           0    1� t  
 �� Y   �%               o%   o           �    �
"   
 ��           �    1� �   �� Y   �%               o%   o           � �   �
"   
 ��               1� �   �� Y   �%               o%   o           � �   �
"   
 ��           �    1� �   �� �   �%               o%   o           %               
"   
 ��              1� �   �� �     
"   
 ��           D    1� �   �� Y   �%               o%   o           �   � �
"   
 ��           �    1� �   �� Y   �%               o%   o           � �  N �
"   
 ��           ,    1� $   �� �   �%               o%   o           %               
"   
 ��           �    1� 4   �� �   �%               o%   o           %               
"   
 ��           $    1� F   �� �   �%               o%   o           %              
"   
 ��          �    1� S   �� �     
"   
 ��           �    1� b  
 �� �   �%               o%   o           %               
"   
 ��           X    1� m   �� Y   �%               o%   o           � ^    �
"   
 ��          �    1� u   �� �     
"   
 ��           	    1� �   �� Y   �%               o%   o           � �  t �
"   
 ��          |	    1�   
 �� �     
"   
 ��           �	    1�    �� Y   �%               o%   o           � ,  � �
"   
 ��           ,
    1� �   �� Y   �%               o%   o           � ^    �
"   
 ��           �
    1� �  
 �� �   �%               o%   o           %               
"   
 �	�               1� �   �	� �   �%               o%   o           %               
"   
 �	�           �    1� �   �	� Y   �%               o%   o           � ^    �	
"   
 �	�               1� �   �	� Y   �%               o%   o           o%   o           
"   
 �	�           �    1�   
 �	� Y   �%               o%   o           � ^    �	
"   
 �	�           �    1�    �	� $  	 �%               o%   o           � .  / �	
"   
 ��          p    1� ^   �� $  	   
"   
 �	�           �    1� p   �	� $  	 �o%   o           o%   o           � ^    �	
"   
 ��               1� �   �� $  	   
"   
 �	�           \    1� �   �	� $  	 �o%   o           o%   o           � ^    �	
"   
 ��          �    1� �   �� �     
"   
 ��              1� �   �� $  	   
"   
 ��          H    1� �   �� $  	   
"   
 ��          �    1� �   �� $  	   
"   
 �	�           �    1� �   �	� �   �o%   o           o%   o           %              
"   
 ��          <    1� �   �� $  	   
"   
 ��          x    1� �  
 ��      
"   
 ��          �    1� 
   �� $  	   
"   
 ��          �    1�    �� $  	   
"   
 ��          ,    1� ,   �� $  	   
"   
 ��          h    1� A   �� $  	   
"   
 ��          �    1� P  	 �� $  	   
"   
 ��          �    1� Z   �� $  	   
"   
 ��              1� m   �� $  	   
"   
 �	�           X    1� �   �	� Y   �%               o%   o           o%   o           
�H T   %              �     }        �GG %              
"   
   
"   
 �	
"   
   
"   
 �	(�  L ( l       �             �� �   � P   �        ,    �@    
� @  , 
�       8    �� �     p�               �L
�    %              � 8      D    � $         � �          
�    � �     
"   
 �� @  , 
�       T    �� t  
 �p�               �L"      P �L 
�H T   %              �     }        �GG %              
"   
 �	�                1� �  
 �	� Y   �%               o%   o           � ^    �	
"   
 �	�           t    1� �  
 �	� Y   �%               o%   o           o%   o           
"   
 �	�           �    1� �   �	� �   �%               o%   o           o%   o           
"   
 �	�           l    1� �   �	� �   �%               o%   o           %               
"   
 �	�           �    1� �   �	� �   �%               o%   o           %               
"   
 �	�           d    1� �   �	� Y   �%               o%   o           � ^    �	
"   
 �	�           �    1� �   �	� �   �%               o%   o           %              
"   
 �	�           T    1�    �	� �   �%               o%   o           o%   o           
"   
 �	�           �    1�    �	� Y   �%               o%   o           o%   o           
"   
 �	�           L    1� +  	 �	� Y   �%               o%   o           � ^    �	
"   
 �	�           �    1� 5   �	� Y   �%               o%   o           o%   o           
"   
 �	�           <    1� I   �	� Y   �%               o%   o           o%   o           
"   
 �	�           �    1� X   �	� �   �%               o%   o           %               
"   
 �	�           4    1� h   �	� �   �%               o%   o           o%   o           P �L 
�H T   %              �     }        �GG %              
"   
 �	�               1� t   �	� $  	 �%               o%   o           � ^    �	
"   
 �	�           x    1� �   �	� $  	 �%               o%   o           � ^    �	
"   
 �	�           �    1� �   �	� �   �%               o%   o           %               
"   
 �	�           h    1� �   �	� $  	 �%               o%   o           � ^    �	
"   
 �	�           �    1� �   �	� $  	 �%               o%   o           � ^    �	
"   
 �	�           P    1� �   �	� �   �%               o%   o           %               
"   
 �	�           �    1� �   �	� $  	 �%               o%   o           � ^    �	
"   
 �	�           @    1� �   �	� $  	 �%               o%   o           � ^    �	
"   
 �	�           �    1� �   �	� $  	 �%               o%   o           � ^    �	
"   
 �	�           (     1� �   �	� $  	 �%               o%   o           o%   o           
"   
 �	�           �     1�    �	� $  	 �%               o%   o           � ^    �	
"   
 �	�           !    1�    �	� $  	 �%               o%   o           � ^    �	
"   
 �	�           �!    1�    	 �	�    �%               o%   o           %               
"   
 �	�           "    1� *   �	�    �%               o%   o           %               
"   
 �	�           �"    1� 3   �	� �   �%               o%   o           o%   o           
"   
 �	�            #    1� D   �	� �   �%               o%   o           o%   o           
"   
 �	�           |#    1� S   �	� �   �%               o%   o           %               
"   
 �	�           �#    1� a   �	� �   �%               o%   o           %               
"   
 �	�           t$    1� r   �	� �   �%               o%   o           %               
"   
 �	�           �$    1� �   �	� �   �%               o%   o           %       
       
"   
 �	�           l%    1� �   �	� �   �%               o%   o           o%   o           
"   
 �	�           �%    1� �   �	� �   �%               o%   o           %              
"   
 �	�           d&    1� �   �	� �   �%               o%   o           o%   o           
"   
 �	�           �&    1� �   �	� �   �%               o%   o           %              
"   
 �	�           \'    1� �   �	� �   �%               o%   o           o%   o           
"   
 �	�           �'    1� �   �	� �   �%               o%   o           %              
"   
 �	�           T(    1� �   �	� �   �%               o%   o           o%   o           
"   
 �	�           �(    1� �   �	� $  	 �%               o%   o           � ^    �	P �L 
�H T   %              �     }        �GG %              
"   
 �	�           �)    1� �   �	� �   �%               o%   o           %               
"   
 �	�           *    1�    �	� �   �%               o%   o           o%   o           
"   
 �	�           �*    1�    �	� Y   �%               o%   o           � ^    �	
"   
 �	�           +    1� #   �	� Y   �%               o%   o           � 9  - �	
"   
 �	�           x+    1� g   �	� Y   �%               o%   o           � ^    �	
"   
 �	�           �+    1� ~   �	� Y   �%               o%   o           � �   �	
"   
 ��          `,    1� �   �� �     
"   
 �	�           �,    1� �   �	� Y   �%               o%   o           � ^    �	
"   
 ��          -    1� �  
 �� �     
"   
 ��          L-    1� �   �� �     
"   
 �	�           �-    1� �   �	� $  	 �%               o%   o           � ^    �	
"   
 �	�           �-    1� �   �	� Y   �%               o%   o           � ^    �	
"   
 �	�           p.    1�    �	� �   �%               o%   o           o%   o           
"   
 �	�           �.    1�    �	� Y   �%               o%   o           � (  ! �	
"   
 �	�           `/    1� J   �	� Y   �%               o%   o           � ^    �	
"   
 �	�           �/    1� W   �	� Y   �%               o%   o           � j   �	
"   
 �	�           H0    1� y  	 �	� �   �%               o%   o           o%   o           
"   
 �	�           �0    1� �   �	� �   �%               o%   o           %               
"   
 ��          @1    1� �   �� �     
"   
 �	�           |1    1� �   �	� Y   �%               o%   o           � �   �	
"   
 �	�           �1    1� �   �	� $  	 �%               o%   o           � ^    �	
"   
 �	�           d2    1� �   �	� $  	 �%               o%   o           � ^    �	
"   
 ��          �2    1� �   �� �     
"   
 ��          3    1� �   �� $  	   
"   
 �	�           P3    1�    �	� �   �o%   o           o%   o           %               
"   
 ��          �3    1�    �� �     
"   
 ��          4    1� 0   �� $  	   
"   
 ��          D4    1� >   �� $  	   
"   
 ��          �4    1� Q   �� $  	   
"   
 ��          �4    1� b   �� $  	   
"   
 ��          �4    1� s   �� $  	   
"   
 ��          45    1� �   �� �     
"   
 �	�           p5    1� �   �	� Y   �%               o%   o           � �  4 �	
"   
 ��          �5    1� �   �� �     
"   
 ��           6    1� �   �� �     
"   
 ��          \6    1� �   �� �     
"   
 ��          �6    1�    �� $  	   
"   
 ��          �6    1�    �� $  	   
"   
 ��          7    1� 1   �� $  	   
"   
 ��          L7    1� C   �� �     
"   
 �	�           �7    1� P   �	� $  	 �%               o%   o           � ^    �	
"   
 �	�           �7    1� ^   �	� $  	 �%               o%   o           � ^    �	
"   
 �	�           p8    1� j   �	� $  	 �%               o%   o           � ^    �	
"   
 �	�           �8    1�    �	� $  	 �%               o%   o           � ^    �	
"   
 �	�           X9    1� �   �	� �   �%               o%   o           %               
"   
 �	�           �9    1� �   �	� �   �%               o%   o           o%   o           
"   
 �	�           P:    1� �   �	� �   �%               o%   o           %               
"   
 �	�           �:    1� �   �	� �   �%               o%   o           %               
"   
 �	�           H;    1� �   �	� �   �%               o%   o           o%   o           
"   
 �	�           �;    1� �   �	� �   �%               o%   o           %               
"   
 ��          @<    1� �   �� $  	   
"   
 �	�           |<    1�    �	� �   �%               o%   o           %              
"   
 ��          �<    1�    �� $  	   
"   
 ��          4=    1� $   �� $  	   
"   
 ��          p=    1� 3  
 �� $  	   
"   
 �	�           �=    1� >   �	� $  	 �%               o%   o           � �   �	
"   
 �	�            >    1� P   �	� $  	 �%               o%   o           � ^    �	P �L 
�H T   %              �     }        �GG %              
"   
 �	�           �>    1� a   �	� Y   �%               o%   o           � ^    �	
"   
 �	�           \?    1� o   �	� �   �%               o%   o           %               
"   
 �	�           �?    1� |   �	� Y   �%               o%   o           � ^    �	
"   
 �	�     ,      L@    1� �   �	� Y   �%               o%   o           �   � <     � �   �	�    	 �	
"   
 �	�           �@    1� �   �	� �   �%               o%   o           o%   o           
"   
 �	�           \A    1� �   �	� Y   �%               o%   o           � ^    �	
"   
 �	�           �A    1� �   �	� Y   �%               o%   o           � ^    �	
"   
 �	�           DB    1� �   �	� $  	 �%               o%   o           o%   o           
"   
 �	�           �B    1� �   �	� Y   �%               o%   o           o%   o           
"   
 �	�           <C    1� �   �	� Y   �%               o%   o           � ^    �	
"   
 �	�           �C    1� �   �	� �   �%               o%   o           %               
"   
 ��          ,D    1�     �� �     
"   
 �	�           hD    1�     �	� Y   �%               o%   o           � 0   ~ �	
"   
 �	�           �D    1� �    �	� Y   �%               o%   o           � ^    �	
"   
 �	�           PE    1� �    �	� Y   �%               o%   o           � �    �	
"   
 �	�           �E    1� �    �	� $  	 �%               o%   o           � 	!   �	
"   
 �	�           8F    1� !   �	� $  	 �%               o%   o           � !   �	
"   
 �	�           �F    1� $!  	 �	� Y   �%               o%   o           � .!   �	
"   
 �	�            G    1� 1!  
 �	� $  	 �%               o%   o           � <!   �	
"   
 �	�           �G    1� A!   �	� �   �%               o%   o           o%   o           
"   
 �	�           H    1� T!   �	� Y   �%               o%   o           � `!   �	
"   
 �	�           �H    1� r!   �	� Y   �%               o%   o           � ^    �	
"   
 �	�           �H    1� {!  
 �	� �   �%               o%   o           o%   o           
"   
 ��          tI    1� �!   �� �     
"   
 �	�           �I    1� �!   �	� Y   �%               o%   o           � �!  ] �	
"   
 �	�           $J    1� "   �	� Y   �%               o%   o           � ^    �	
"   
 �	�           �J    1� "   �	� Y   �%               o%   o           � ("   �	
"   
 �	�           K    1� 0"   �	� �   �%               o%   o           %               
"   
 �	�           �K    1� �   �	� Y   �%               o%   o           � ^    �	
"   
 �	�           �K    1� 8"   �	� Y   �%               o%   o           o%   o           
"   
 ��          xL    1� J"   �� $  	   P �L 
�H T   %              �     }        �GG %              
"   
 �	�           M    1� ["   �	� �   �%               o%   o           %               
"   
 �	�           �M    1� n"  	 �	� �   �%               o%   o           %               
"   
 ��           N    1� x"   �� Y         
�    
�        �     }         �     }        �     }             �     }        �%                  �     }         �     }        �     }             �     }        �%              
�             �G "    �%     start-super-proc v�%     adm2/smart.p k�	P �L 
�H T   %              �     }        �GG %              
"   
   �       �O    6� �     
"   
   
�        P    8
"   
   �        <P    ��     }        �G 4              
"   
 ߱G %              G %              %� � �   EnabledObjFldsToDisable,ModifyFields,DataSourceNames,UpdateTargetNames,LogicalObjectName,LogicalObjectName,PhysicalObjectName,DynamicObject,RunAttribute,HideOnInit,DisableOnInit,ObjectLayout 
�H T   %              �     }        �GG %              
"   
 �	
"   
 �
"   
 �	
"   
   (�  L ( l       �        �Q    �� �   � P   �        �Q    �@    
� @  , 
�       �Q    �� �   �	p�               �L
�    %              � 8       R    � $         � �          
�    � �   �	
"   
 �p� @  , 
�       S    �� �   �p�               �L"  
  , �   � �"   �	� �"   ��     }        �A      |    "  
    � �"   �	%              (<   \ (    |    �     }        �A� �"   �A"    �	    "  
  �	"    �	  < "  
  �	"    �	(    |    �     }        �A� �"   �A"    �	
�H T   %              �     }        �GG %              
"   
 �	
"   
 �
"   
 �	
"   
   (�  L ( l       �        �T    �� �   � P   �        �T    �@    
� @  , 
�       �T    �� �   �	p�               �L
�    %              � 8      U    � $         � �          
�    � �   �	
"   
 �p� @  , 
�       V    �� N  
 �p�               �L"  
  , 
�H T   %              �     }        �GG %              
"   
 �	
"   
 �
"   
 �	
"   
 �	(�  L ( l       �        �V    �� �   � P   �        �V    �@    
� @  , 
�       �V    �� �   �	p�               �L
�    %              � 8      �V    � $         � �   �	     
�    � �   �
"   
 �p� @  , 
�       �W    �� �   �p�               �L
�             �G
�H T   %              �     }        �GG %              
"   
   
"   
 �	
"   
   
"   
   (�  L ( l       �        �X    �� �   � P   �        �X    �@    
� @  , 
�       �X    �� �     p�               �L
�    %              � 8      �X    � $         � �          
�    � �     
"   
 �p� @  , 
�       �Y    �� t  
 �p�               �L%     SmartDataViewer 
"   
   p� @  , 
�       8Z    �� �     p�               �L%      FRAME   
"   
  p� @  , 
�       �Z    �� �    p�               �L%               
"   
  p� @  , 
�       �Z    �� p    p�               �L(        � ^      � ^      � ^      �     }        �A
�H T   %              �     }        �GG %              
"   
 �	 (   � 
"   
 �	    �        �[    �� �   �
"   
   � 8      $\    � $         � �          
�    � �   �	
"   
   �        |\    �
"   
   �       �\    /
"   
   
"   
   �       �\    6� �     
"   
   
�        �\    8
"   
   �        ]    �
"   
   �       4]    �
"   
   p�    � �"   �	
�    �     }        �G 4              
"   
 ߱G %              G %              
�     }        �
"   
    (   � 
"   
 �	    �        �]    �A"    �A
"   
   
�        D^    �@ � 
"   
 �	"      �       }        �
"   
 �%              %                "    �%     start-super-proc u�%     adm2/appserver.p {�	�    � a#     
�    �     }        �%               %      Server  - �     }        �    "    �	� ^    �%                   "    �	� ^    �%      NONE    p�,  8         $     "    �	        � {#   �	
�    
�H T   %              �     }        �GG %              
"   
 �	
"   
 �
"   
 �	
"   
   (�  L ( l       �        �`    �� �   � P   �        �`    �@    
� @  , 
�       �`    �� �   �	p�               �L
�    %              � 8      �`    � $         � �          
�    � �   �	
"   
 �p� @  , 
�       �a    �� 5   �p�               �L"    , p�,  8         $     "    �	        � �#   �	
�     "    �%     start-super-proc u�%     adm2/visual.p �	� 
"    
 �%     contextHelp 
"    
   
�    
�    %     processAction   
�    %     CTRL-PAGE-UP k�	%     processAction   
�    %     CTRL-PAGE-DOWN 	 "    �%     start-super-proc t�%     adm2/containr.p %     modifyListProperty 
�    
�    %      Add     %      ContainerSourceEvents �	%      initializeDataObjects �	0 0   A    �    � �#   �	
�    � $   �A    �    � �#     
�    � $   �%     modifyListProperty 
�    
�    %      Add     %      ContainerSourceEvents �	%     buildDataRequest ent0 A    �    � �#   �
�    � .$   �	%     modifyListProperty 
�    
�    %      Add     %     SupportedLinks 	%      ContainerToolbar-Target %               
�H T   %              �     }        �GG %              
"   
 �	
"   
 �
"   
 �	
"   
 �	(�  L ( l       �        @f    �� �   � P   �        Lf    �@    
� @  , 
�       Xf    �� �   �	p�               �L
�    %              � 8      df    � $         � �   �	     
�    � �   �
"   
 �p� @  , 
�       tg    �� �   �p�               �L
�             �G
�H T   %              �     }        �GG %              
"   
 �	
"   
 �
"   
 �	
"   
 �	(�  L ( l       �         h    �� �   � P   �        ,h    �@    
� @  , 
�       8h    �� �   �	p�               �L
�    %              � 8      Dh    � $         � �   �	     
�    � �   �	
"   
 �p� @  , 
�       Ti    �� �   �p�               �L%               "    �%     start-super-proc s�%     adm2/datavis.p 	%     modifyListProperty 
�    %      ADD     %     SupportedLinks 	%     Toolbar-Target 	%     valueChanged    
�    %     valueChanged    
�     "    �%     start-super-proc r�%     adm2/viewer.p �	%     modifyListProperty 
�    
�    %      Add     %     DataSourceEvents Δ	%     buildDataRequest Δ	�   � <   �	� �     � �$  / �	�   � <     � �   �	� �$  / �	�@    �    � <   �	� �$   �	     � <   �	"    �	� <   ��@    �    � <     � �$         � <   �	"    �� <     
�H T   %              �     }        �GG %              
"   
 �
"   
 �	
"   
 �
"   
 �(�  L ( l       �        �l    �� �   � P   �        �l    �@    
� @  , 
�       �l    �� �   �p�               �L
�    %              � 8      �l    � $         � �   �     
�    � �     
"   
 �p� @  , 
�       n    �� |   �p�               �L"    , 
"   
   p� @  , 
�       dn    �� �     p�               �L"    , 
"   
  p� @  , 
�       �n    �� {!  
  p�               �L%               �             I%               �             �%              �J     "      %               %     constructObject %$     util/dgn-card.wDB-AWARE k�	
�             �G%LA<  AppServiceASInfoASUsePrompt?CacheDuration0CheckCurrentChangedyesDestroyStatelessyesDisconnectAppServernoServerOperatingModeNONEShareDatanoUpdateFromSourcenoForeignFieldsObjectNamedgn-cardOpenOnInityesPromptColumns(NONE)PromptOnDeleteyesRowsToBatch200RebuildOnReposnoToggleDataTargetsyes �	
"   
   %     repositionObject r�
"   
   %         %           %     constructObject %     adm2/dynselect.w o�	
�             �G           � y&  d   � �&   .w� �&  V�	
"   
  %     repositionObject r�
"   
   %            %           %     resizeObject    
"   
   %       	  %            %      addLink 
"   
   %      Data    
"   
   %     adjustTabOrder  
"   
 �	
�            �G%      AFTER   �     }        �
�                    �           �   l       ��                   1  �               4��	                    O   ����    e�          O   ����    R�          O   ����    ��        $    �   ���                       �[     
                    � ߱                (  �      �[      4   �����[                �                      ��                    0                  <��	                         8  �  �    0\            !  �  `      �\      4   �����\                p                      ��                  "  /                  ȣ�	                       "  �  �  o   #      ,                                 �  �   $  �\      �  �   %  �\      $  $  &  �  ���                        ]     
                    � ߱        8  �   '   ]      L  �   (  @]      `  �   +  `]          $   .  �  ���                       �]  @         |]              � ߱                     T          ,  @   T �            
             
             
             
                 $   4   D          $   4   D   ����        ��                            ����                                            �           �   l       ��                 U  �  �               ��	                    O   ����    e�          O   ����    R�          O   ����    ��      #                      �          �  $  g    ���                       �]     
                    � ߱                  �  �                      ��                   h  j                  ԓ_	                     h  4      4   ����^      $  i  �  ���                       P^     
                    � ߱        �    k  4  D      d^      4   ����d^      /  l  p                               3   ����x^  �  �   �  �^          O   �  ��  ��  �^                               , �                          
                               �      ��                            ����                                                        �   l       ��                  �  �  �               b	                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                                            �           �   l       ��                 >  e  �               ��b	                    O   ����    e�          O   ����    R�          O   ����    ��      lo                         � ߱          $  F  �   ���                           p   H  to  (      c      �     �o                �                      ��                  J  a                  �	                       J  8    /   K  �     �                          3   �����o                                 3   �����o  P     
   @                      3   �����o  �        p                      3   �����o         
   �  �                  3   ����<q      $   K  �  ���                               
                    � ߱        �  /	  P  4     D  hq                      3   ����Hq  t        d                      3   ����tq            �                      3   �����q  �  /   S  �     �                          3   �����q                                 3   �����q  @     
   0                      3   �����q  p        `                      3   �����q         
   �  �                  3   ���� r      $   S  �  ���                               
                    � ߱        �  /	  X  $     4  Lr                      3   ����,r  d        T                      3   ����Xr            �                      3   ����lr  0  /	  Y  �     �  �r                      3   �����r           �                      3   �����r                                   3   �����r  �  /   \  \     l                          3   �����r  �     
   �                      3   �����r  �        �                      3   �����r         
   �                      3   ����s      /   _  (     8                          3   ����s  h     
   X                      3   ����,s  �     
   �                      3   ����8s            �                      3   ����Ls                               �                                             ��                              ��        �                  ����                                            �           �   l       ��                  k  w  �                sb	                    O   ����    e�          O   ����    R�          O   ����    ��      �      u  �� �                       v  �         `s      4   ����`s      �   v  ts    ��                              ��        �                  ����                               �   d d     T   ���$  �$  � �                                               �      �                                                                  d     D                                                                 P   �d d                                                           m(  G   
 X �d d                                                              �  2   g     �       P   �� vd                                                           t(  G   
 X �� �d                                                       !      �  <   g     �       P   ���d                                                           ~(  G   
 X ��d                                             
          0           g             D                                                                    TXS appSrvUtils RowObject NomCli DirCli NroCard Ruc CodDept ASSIGNFOCUSEDWIDGET ASSIGNWIDGETVALUE ASSIGNWIDGETVALUELIST BLANKWIDGET CLEARWIDGET DISABLERADIOBUTTON DISABLEWIDGET ENABLERADIOBUTTON ENABLEWIDGET FORMATTEDWIDGETVALUE FORMATTEDWIDGETVALUELIST HIDEWIDGET HIGHLIGHTWIDGET RESETWIDGETVALUE TOGGLEWIDGET VIEWWIDGET WIDGETHANDLE WIDGETLONGCHARVALUE WIDGETISBLANK WIDGETISFOCUSED WIDGETISMODIFIED WIDGETISTRUE WIDGETVALUE WIDGETVALUELIST h_dgn-card h_dynselect-2 F-Main x(50) Nombre del Cliente x(60) Direcci�n del Cliente x(11) Registro Unico de Contribuyente (Cliente) O:\on_in_co\Util\vgn-clie.w should only be RUN PERSISTENT. GETTARGETPROCEDURE GETOBJECTTYPE GETSHOWPOPUP SETSHOWPOPUP GETCREATEHANDLES GETDATAMODIFIED GETDISPLAYEDFIELDS GETDISPLAYEDTABLES GETENABLEDFIELDS GETENABLEDHANDLES GETFIELDHANDLES GETFIELDSENABLED GETGROUPASSIGNSOURCE GETGROUPASSIGNSOURCEEVENTS GETGROUPASSIGNTARGET GETGROUPASSIGNTARGETEVENTS GETNEWRECORD GETOBJECTPARENT GETRECORDSTATE GETROWIDENT GETTABLEIOSOURCE GETTABLEIOSOURCEEVENTS GETUPDATETARGET GETUPDATETARGETNAMES GETWINDOWTITLEFIELD OKTOCONTINUE SETCONTAINERMODE SETDATAMODIFIED SETDISPLAYEDFIELDS SETENABLEDFIELDS SETGROUPASSIGNSOURCE SETGROUPASSIGNSOURCEEVENTS SETGROUPASSIGNTARGET SETGROUPASSIGNTARGETEVENTS SETLOGICALOBJECTNAME SETOBJECTPARENT SETSAVESOURCE SETTABLEIOSOURCE SETTABLEIOSOURCEEVENTS SETUPDATETARGET SETUPDATETARGETNAMES SETWINDOWTITLEFIELD SHOWDATAMESSAGES DISABLEPAGESINFOLDER ENABLEPAGESINFOLDER GETCALLERPROCEDURE GETCALLERWINDOW GETCONTAINERMODE GETCONTAINERTARGET GETCONTAINERTARGETEVENTS GETCURRENTPAGE GETDISABLEDADDMODETABS GETDYNAMICSDOPROCEDURE GETFILTERSOURCE GETMULTIINSTANCEACTIVATED GETMULTIINSTANCESUPPORTED GETNAVIGATIONSOURCE GETNAVIGATIONSOURCEEVENTS GETNAVIGATIONTARGET GETOUTMESSAGETARGET GETPAGENTARGET GETPAGESOURCE GETPRIMARYSDOTARGET GETREENABLEDATALINKS GETRUNDOOPTIONS GETRUNMULTIPLE GETSAVEDCONTAINERMODE GETSDOFOREIGNFIELDS GETTOPONLY GETUPDATESOURCE GETWAITFOROBJECT GETWINDOWTITLEVIEWER GETSTATUSAREA PAGENTARGETS SETCALLEROBJECT SETCALLERPROCEDURE SETCALLERWINDOW SETCONTAINERTARGET SETCURRENTPAGE SETDISABLEDADDMODETABS SETDYNAMICSDOPROCEDURE SETFILTERSOURCE SETINMESSAGETARGET SETMULTIINSTANCEACTIVATED SETMULTIINSTANCESUPPORTED SETNAVIGATIONSOURCE SETNAVIGATIONSOURCEEVENTS SETNAVIGATIONTARGET SETOUTMESSAGETARGET SETPAGENTARGET SETPAGESOURCE SETPRIMARYSDOTARGET SETREENABLEDATALINKS SETROUTERTARGET SETRUNDOOPTIONS SETRUNMULTIPLE SETSAVEDCONTAINERMODE SETSDOFOREIGNFIELDS SETTOPONLY SETUPDATESOURCE SETWAITFOROBJECT SETWINDOWTITLEVIEWER SETSTATUSAREA GETALLFIELDHANDLES GETALLFIELDNAMES GETCOL GETDEFAULTLAYOUT GETDISABLEONINIT GETENABLEDOBJFLDS GETENABLEDOBJHDLS GETHEIGHT GETHIDEONINIT GETLAYOUTOPTIONS GETLAYOUTVARIABLE GETOBJECTENABLED GETOBJECTLAYOUT GETROW GETWIDTH GETRESIZEHORIZONTAL GETRESIZEVERTICAL SETALLFIELDHANDLES SETALLFIELDNAMES SETDEFAULTLAYOUT SETDISABLEONINIT SETHIDEONINIT SETLAYOUTOPTIONS SETOBJECTLAYOUT SETRESIZEHORIZONTAL SETRESIZEVERTICAL GETOBJECTTRANSLATED GETOBJECTSECURED CREATEUIEVENTS GETAPPSERVICE GETASBOUND GETASDIVISION GETASHANDLE GETASHASSTARTED GETASINFO GETASINITIALIZEONRUN GETASUSEPROMPT GETSERVERFILENAME GETSERVEROPERATINGMODE RUNSERVERPROCEDURE SETAPPSERVICE SETASDIVISION SETASHANDLE SETASINFO SETASINITIALIZEONRUN SETASUSEPROMPT SETSERVERFILENAME SETSERVEROPERATINGMODE gshAstraAppserver gshSessionManager gshRIManager gshSecurityManager gshProfileManager gshRepositoryManager gshTranslationManager gshWebManager gscSessionId gsdSessionObj gshFinManager gshGenManager gshAgnManager gsdTempUniqueID gsdUserObj gsdRenderTypeObj gsdSessionScopeObj ghProp ghADMProps ghADMPropsBuf glADMLoadFromRepos glADMOk ANYMESSAGE ASSIGNLINKPROPERTY FETCHMESSAGES GETCHILDDATAKEY GETCONTAINERHANDLE GETCONTAINERHIDDEN GETCONTAINERSOURCE GETCONTAINERSOURCEEVENTS GETCONTAINERTYPE GETDATALINKSENABLED GETDATASOURCE GETDATASOURCEEVENTS GETDATASOURCENAMES GETDATATARGET GETDATATARGETEVENTS GETDBAWARE GETDESIGNDATAOBJECT GETDYNAMICOBJECT GETINSTANCEPROPERTIES GETLOGICALOBJECTNAME GETLOGICALVERSION GETOBJECTHIDDEN GETOBJECTINITIALIZED GETOBJECTNAME GETOBJECTPAGE GETOBJECTVERSION GETOBJECTVERSIONNUMBER GETPARENTDATAKEY GETPASSTHROUGHLINKS GETPHYSICALOBJECTNAME GETPHYSICALVERSION GETPROPERTYDIALOG GETQUERYOBJECT GETRUNATTRIBUTE GETSUPPORTEDLINKS GETTRANSLATABLEPROPERTIES GETUIBMODE GETUSERPROPERTY INSTANCEPROPERTYLIST LINKHANDLES LINKPROPERTY MAPPEDENTRY MESSAGENUMBER PROPERTYTYPE REVIEWMESSAGES SETCHILDDATAKEY SETCONTAINERHIDDEN SETCONTAINERSOURCE SETCONTAINERSOURCEEVENTS SETDATALINKSENABLED SETDATASOURCE SETDATASOURCEEVENTS SETDATASOURCENAMES SETDATATARGET SETDATATARGETEVENTS SETDBAWARE SETDESIGNDATAOBJECT SETDYNAMICOBJECT SETINSTANCEPROPERTIES SETLOGICALVERSION SETOBJECTNAME SETOBJECTVERSION SETPARENTDATAKEY SETPASSTHROUGHLINKS SETPHYSICALOBJECTNAME SETPHYSICALVERSION SETRUNATTRIBUTE SETSUPPORTEDLINKS SETTRANSLATABLEPROPERTIES SETUIBMODE SETUSERPROPERTY SHOWMESSAGE SIGNATURE , prepareInstance ObjectName CHAR  ObjectVersion ADM2.2 ObjectType SmartDataViewer ContainerType FRAME PropertyDialog adm2/support/viewerd.w QueryObject LOGICAL ContainerHandle HANDLE InstanceProperties EnabledObjFldsToDisable,ModifyFields,DataSourceNames,UpdateTargetNames,LogicalObjectName,LogicalObjectName,PhysicalObjectName,DynamicObject,RunAttribute,HideOnInit,DisableOnInit,ObjectLayout SupportedLinks Data-Target,Update-Source,TableIO-Target,GroupAssign-Source,GroupAssign-Target ContainerHidden ObjectInitialized ObjectHidden ObjectsCreated HideOnInit UIBMode ContainerSource ContainerSourceEvents initializeObject,hideObject,viewObject,destroyObject,enableObject,confirmExit,confirmCancel,confirmOk,isUpdateActive DataSource DataSourceEvents dataAvailable,queryPosition,updateState,deleteComplete,fetchDataSet,confirmContinue,confirmCommit,confirmUndo,assignMaxGuess,isUpdatePending TranslatableProperties ObjectPage INT DBAware DesignDataObject DataSourceNames DataTarget DataTargetEvents CHARACTER updateState,rowObjectState,fetchBatch,LinkState LogicalObjectName PhysicalObjectName LogicalVersion PhysicalVersion DynamicObject RunAttribute ChildDataKey ParentDataKey DataLinksEnabled InactiveLinks InstanceId DECIMAL SuperProcedure SuperProcedureMode SuperProcedureHandle LayoutPosition ClassName RenderingProcedure ThinRenderingProcedure Label cType ADMProps Target WHERE Target = WIDGET-H(" ") AppService ASDivision ASHandle ASHasConnected ASHasStarted ASInfo ASInitializeOnRun ASUsePrompt BindSignature BindScope ServerOperatingMode ServerFileName ServerFirstCall NeedContext ObjectLayout LayoutOptions ObjectEnabled LayoutVariable DefaultLayout DisableOnInit EnabledObjFlds EnabledObjHdls FieldSecurity SecuredTokens AllFieldHandles AllFieldNames MinHeight MinWidth ResizeHorizontal ResizeVertical ObjectSecured ObjectTranslated PopupButtonsInFields ColorInfoBG INTEGER ColorInfoFG ColorWarnBG ColorWarnFG ColorErrorBG ColorErrorFG BGColor FGColor FieldPopupMapping CurrentPage PendingPage ContainerTarget ContainerTargetEvents exitObject,okObject,cancelObject,updateActive ContainerToolbarSource ContainerToolbarSourceEvents toolbar,okObject,cancelObject OutMessageTarget PageNTarget PageSource FilterSource UpdateSource UpdateTarget CommitSource CommitSourceEvents commitTransaction,undoTransaction CommitTarget CommitTargetEvents rowObjectState StartPage RunMultiple WaitForObject DynamicSDOProcedure adm2/dyndata.w RunDOOptions InitialPageList WindowFrameHandle Page0LayoutManager MultiInstanceSupported MultiInstanceActivated ContainerMode SavedContainerMode SdoForeignFields NavigationSource NavigationTarget PrimarySdoTarget NavigationSourceEvents fetchFirst,fetchNext,fetchPrev,fetchLast,startFilter CallerWindow CallerProcedure CallerObject DisabledAddModeTabs ReEnableDataLinks WindowTitleViewer UpdateActive InstanceNames ClientNames ContainedDataObjects ContainedAppServices DataContainer HasDbAwareObjects HasDynamicProxy HideOnClose HideChildContainersOnClose HasObjectMenu RequiredPages RemoveMenuOnHide ProcessList PageLayoutInfo PageTokens DataContainerName WidgetIDFileName CreateHandles DataModified DisplayedFields DisplayedTables   Editable EnabledFields EnabledHandles EnabledObjFldsToDisable EnabledWhenNew FieldHandles FieldsEnabled GroupAssignSource GroupAssignSourceEvents addRecord,copyRecord,updateRecord,resetRecord,undoRecord,cancelRecord,enableFields,disableFields,collectChanges,validateFields GroupAssignTarget GroupAssignTargetEvents updateState,LinkState InternalDisplayFromSource (Large) ModifyFields (All) NewRecord No ObjectMode View PrintPreviewActive RecordState NoRecordAvailable RowIdent SaveSource TableIOSource TableIOSourceEvents addRecord,updateRecord,copyRecord,deleteRecord,resetRecord,undoChange,cancelRecord,updateMode ToolbarSource ToolbarSourceEvents toolbar UndoNew UpdateTargetNames WindowTitleField KeepChildPositions ShowPopup FieldWidgetIDs ghContainer adm2/smart.p cObjectName iStart / \ . hReposBuffer hPropTable hBuffer hTable deleteProperties ADM-CLONE-PROPS pcProcName hProc START-SUPER-PROC cAppService cASDivision cServerOperatingMode adm2/appserver.p getAppService Server NONE setASDivision setAppService cFields adm2/visual.p CTRL-PAGE-UP CTRL-PAGE-DOWN adm2/containr.p Add initializeDataObjects getSupportedLinks data-target data-source buildDataRequest containertoolbar-target ContainerToolbar-Target adm2/datavis.p ADD Toolbar-Target DISPLAYOBJECTS cViewCols cEnabled iCol iEntries cEntry adm2/viewer.p RowObject.NomCli RowObject.DirCli RowObject.Ruc ,RowObject. currentPage util/dgn-card.wDB-AWARE AppServiceASInfoASUsePrompt?CacheDuration0CheckCurrentChangedyesDestroyStatelessyesDisconnectAppServernoServerOperatingModeNONEShareDatanoUpdateFromSourcenoForeignFieldsObjectNamedgn-cardOpenOnInityesPromptColumns(NONE)PromptOnDeleteyesRowsToBatch200RebuildOnReposnoToggleDataTargetsyes adm2/dynselect.w AutoRefreshnoChangedEventDisplayedFieldDataSourceFilterNumRows5OptionalnoOptionalString <none> Label?SortyesViewAscombo-box:drop-down-listToolTipFormat?HelpId0BrowseTitleBrowseFieldsExitBrowseOnActionnoCancelBrowseOnExitnoRepositionDataSourcenoDefineAnyKeyTriggeryesStartBrowseKeysNEXT-FRAMEFieldNameNroCardDisplayFieldyesEnableFieldyesLocalFieldnoHideOnInitnoDisableOnInitnoObjectLayoutKeyField Data AFTER ADM-CREATE-OBJECTS DISABLE_UI default Nombre Direcci�n Ruc 8  #  h  �+      3 �    ��      0         pcFieldType     ��      T         pcColValues     ��      x         pcValue     ��      �         pcState �   ��      �         pcChanges       ��      �         pcChanges       ��               plCancel        ��      $        plAnswer        ��      H        plCancel        ��      l        pcRelative  �  ��      �        pcAction        ��      �        pcAction        ��      �        pcState     ��      �        pcReturn        ��              pcMode      ��      <        pcState     ��      \        pcNotValidFields    �  ��      �        pcProp      ��      �        pcProp      ��      �        plCancel    �  ��      �        pcProcName    ��             
 pcProcName  @  ��      4        pcProcName      ��      X       
 pcProcName      ��      |        piPageNum       ��      �        piPageNum       ��      �        pcPageList      ��      �        pcProc    ��              pcLinkName      ��      ,        pcLinkName  \  ��      P       
 phTarget        ��      t        phTarget        ��      �        piPageNum       ��      �        pcValue     ��      �        piPageNum       ��               pcAction        ��      $       
 phAppService        ��      L        pcMode  x  ��      l       
 phSource    �  ��      �        phSource        ��      �       
 phSource    �  ��      �        pcText     ��      �        pcText      ��              pcText  D  ��      8       
 phObject    h  ��      \       
 phObject        ��      �        phObject        ��      �        pcField     ��      �        pcCursor    �  ��      �       
 phCaller      ��              phCaller    <  ��      0        phCaller        ��      T        phCaller    �  ��      x        pcMod   �  ��      �        pcMod       ��      �       
 pcMod   �  ��      �       
 phSource      ��      �        phSource        ��              
 phSource    L  ��      D        pdRow       ��      d        pdRow       ��      �       
 hTarget �  ��      �        pcMessage       ��      �        pcMessage       ��      �        plEnabled             	     cType       T	     T   �          D	                  getObjectType     5  7  �	        t	  
   hReposBuffer    �	        �	  
   hPropTable  �	        �	  
   hBuffer           �	  
   hTable  	  
     U   `	          
                  adm-clone-props         !  "  #  $  %  &  '  (  +  .  /  0  1            t
  
   hProc             �
        pcProcName  �	  �
  	   V   `
  |
      �
                  start-super-proc    g  h  i  j  k  l  �  �  �  �
  8     W                                   @    l     X                                   D  E  <  �     Y                                   H  I  t  �     Z               �                  displayObjects  �  �        [                                   �  �  T     \                                               l     currentPage $  �     ]   X          �                  adm-create-objects  F  H  J  K  P  S  X  Y  \  _  a  c  e  x  ,     ^                                  disable_UI  u  v  w  �    $     �      �                          x  �     RowObject   �         �         �         �         �         NomCli  DirCli  NroCard Ruc CodDept           �  
   appSrvUtils $         
   h_dgn-card  H       8  
   h_dynselect-2   p        \  
   gshAstraAppserver   �        �  
   gshSessionManager   �        �  
   gshRIManager    �        �  
   gshSecurityManager          �  
   gshProfileManager   8  	 	        
   gshRepositoryManager    d  
 
     L  
   gshTranslationManager   �        x  
   gshWebManager   �        �     gscSessionId    �        �     gsdSessionObj   �        �  
   gshFinManager             
   gshGenManager   <        ,  
   gshAgnManager   `        P     gsdTempUniqueID �        t     gsdUserObj  �        �     gsdRenderTypeObj    �        �     gsdSessionScopeObj  �       �  
   ghProp            
   ghADMProps  0          
   ghADMPropsBuf   X       D     glADMLoadFromRepos  t       l     glADMOk �    	   �  
   ghContainer �    
   �     cObjectName �       �     iStart  �       �     cAppService             cASDivision <       $     cServerOperatingMode    X       P     cFields x       l     cViewCols   �       �     cEnabled    �       �     iCol    �       �     iEntries             �     cEntry        X     RowObject            F   G  H  J  K  �	  �	  �	  �	  
  
  
  
  
  
  
  
  
  
  !
  "
  #
  $
  &
  (
  *
  ,
  -
  .
  1
  3
  4
  6
  7
  8
  9
  :
  @
  B
  H
  J
  L
  M
  S
  T
  U
  V
  Y
  Z
  \
  ]
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
  k
  l
  m
  n
  o
  �
  W  X  [  \  ]  ^  _  `  a  b  c  d  e  f  g  h  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �                   x  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  6  D  E  G  H  I  J  P  Q  R  S  T  U  V  W  X  Z  [  \  ]  ^  _  `  a  b  c  d  e  g  h  j  k  l  m  �  �  �  �  �  �  b  $  %  '  (  +  ,  .  6  o  p  y  z  ~    �  �  �  �  �  �  �  �       �  �  �  �  �  �  �  �  �  �  �  �       S  �  �  =  >  ?  A  C  G  `  a  b  d  l  r  x  {  �  �  �  !  R  S  T  V  X  �        	  
      $        :%  C:\Progress\OpenEdge\src\adm2\viewer.i   �  �Q 2 %C:\Progress\OpenEdge\src\adm2\custom\viewercustom.i  �  } & C:\Progress\OpenEdge\src\adm2\datavis.i    � 1 %C:\Progress\OpenEdge\src\adm2\custom\dataviscustom.i D  f! ' C:\Progress\OpenEdge\src\adm2\containr.i �  � 0 %C:\Progress\OpenEdge\src\adm2\custom\containrcustom.i    �  �� ( C:\Progress\OpenEdge\src\adm2\visual.i   �  # / %C:\Progress\OpenEdge\src\adm2\custom\visualcustom.i  0  �< ) C:\Progress\OpenEdge\src\adm2\appserver.i    p  �� . %C:\Progress\OpenEdge\src\adm2\custom\appservercustom.i   �  I� * C:\Progress\OpenEdge\src\adm2\smart.i    �  Ds - C:\Progress\OpenEdge\gui\fn     tw , %C:\Progress\OpenEdge\src\adm2\custom\smartcustom.i   H  Q. + C:\Progress\OpenEdge\gui\set �  �/  C:\Progress\OpenEdge\src\adm2\viewprop.i �  �� $ %C:\Progress\OpenEdge\src\adm2\custom\viewpropcustom.i    �  ۃ % %C:\Progress\OpenEdge\src\adm2\custom\viewprtocustom.i    (  ��  C:\Progress\OpenEdge\src\adm2\dvisprop.i l  B� " %C:\Progress\OpenEdge\src\adm2\custom\dvispropcustom.i    �  �� # %C:\Progress\OpenEdge\src\adm2\custom\dvisprtocustom.i    �  ��  C:\Progress\OpenEdge\src\adm2\cntnprop.i (  ��   %C:\Progress\OpenEdge\src\adm2\custom\cntnpropcustom.i    \  P ! %C:\Progress\OpenEdge\src\adm2\custom\cntnprtocustom.i    �  F>  C:\Progress\OpenEdge\src\adm2\visprop.i  �  �I  %C:\Progress\OpenEdge\src\adm2\custom\vispropcustom.i   ��  %C:\Progress\OpenEdge\src\adm2\custom\visprtocustom.i X  �l  C:\Progress\OpenEdge\src\adm2\appsprop.i �  ɏ  %C:\Progress\OpenEdge\src\adm2\custom\appspropcustom.i    �  V  %C:\Progress\OpenEdge\src\adm2\custom\appsprtocustom.i      i$  C:\Progress\OpenEdge\src\adm2\smrtprop.i T  �j  C:\Progress\OpenEdge\gui\get �  �  %C:\Progress\OpenEdge\src\adm2\custom\smrtpropcustom.i    �  ��  %C:\Progress\OpenEdge\src\adm2\custom\smrtprtocustom.i    �  ��  C:\Progress\OpenEdge\src\adm2\smrtprto.i 8  Su  C:\Progress\OpenEdge\src\adm2\globals.i  l  M�  %C:\Progress\OpenEdge\src\adm2\custom\globalscustom.i �  )a  %C:\Progress\OpenEdge\src\adm2\custom\smartdefscustom.i   �  �  C:\Progress\OpenEdge\src\adm2\appsprto.i $   ��  %C:\Progress\OpenEdge\src\adm2\custom\appserverdefscustom.i   X   �X  C:\Progress\OpenEdge\src\adm2\visprto.i  �   !�  %C:\Progress\OpenEdge\src\adm2\custom\visualdefscustom.i  �   n�  C:\Progress\OpenEdge\src\adm2\cntnprto.i !  ;  %C:\Progress\OpenEdge\src\adm2\custom\containrdefscustom.i    L!  �7 
 C:\Progress\OpenEdge\src\adm2\dvisprto.i �!  0 	 %C:\Progress\OpenEdge\src\adm2\custom\datavisdefscustom.i �!  ��  C:\Progress\OpenEdge\src\adm2\viewprto.i "  gf  %C:\Progress\OpenEdge\src\adm2\custom\viewerdefscustom.i  @"  ~�  C:\Progress\OpenEdge\src\adm2\widgetprto.i   �"  lo  O:\on_in_co\util\dgn-clie.i  �"  e�  !C:\Progress\OpenEdge\gui\adecomm\appserv.i   �"  Lx    O:\on_in_co\Util\vgn-clie.w      �   
      D#  �   �     T#     �  2   d#  �   �     t#     d  +   �#  �   a     �#     ?  +   �#  �   >     �#       +   �#  \   �     �#  o   �  &   �#     \  1   �#  U   B  &   $  �   ;  '   $       +   $$  �     '   4$     �  +   D$  �   �  '   T$     �  0   d$  �   {  '   t$     y  -   �$  �   r  '   �$     p  -   �$  �   o  '   �$     m  -   �$  r   Q  '   �$  n   9  (   �$     �  /   �$  P   �  (   %  �   �  )   %     b  .   $%  �   ]  )   4%     ;  +   D%  �   :  )   T%       +   d%  �     )   t%     �  +   �%  g   �  )   �%     �     �%  O   �  )   �%  �   -  *   �%     +  -   �%  �   �  *   �%     �  ,   �%  �   �  *   &     v  +   &  �   u  *   $&     S  +   4&  �   R  *   D&     0  +   T&  �   /  *   d&       +   t&  �   �  *   �&     �  +   �&  �   �  *   �&     �  +   �&  }   �  *   �&     �  +   �&       *   �&     �  )   �&     n  (   '     �  '   '     �  &   $'     k     4'  u   b     D'  O   T  $   T'     C  %   d'     �  $   t'  h   �     �'  �   �     �'  O   �  "   �'     �  #   �'     r  "   �'  {   ?     �'  �   6     �'  O   (      �'       !   (     �      (  �   �     $(  �   x     4(  O   j     D(     Y     T(          d(  �   �     t(  x   �     �(  M   �     �(     �     �(     l     �(  a   U     �(  �  4     �(          �(  �  �
     �(  O   �
     )     �
     )     u
     $)  �   �	     4)     q     D)     �     T)  x   �     d)     �     t)     0     �)     ,     �)          �)     �     �)  Q   �     �)     �     �)     ]     �)     I     �)     /     *  f        *     �     $*  "   _     4*     K     D*     *     T*  Z   �     d*     �     t*     �     �*     �     �*     t     �*  X   Q     �*     �  
   �*      c     �*     O  	   �*     0     �*  ]   %     +     �     +     �     $+     �     4+     {     D+     ^     T+  0   �       d+     O      t+     ,       �+     &      �+     !       �+           
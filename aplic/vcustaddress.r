	��V��BY ?    �              �                                 �� 3F000143utf-8 MAIN C:\newsie\on_in_co\aplic\vcustaddress.w,, PROCEDURE disable_UI,, PROCEDURE displayObjects,, PROCEDURE start-super-proc,,INPUT pcProcName CHARACTER PROCEDURE adm-clone-props,, PROCEDURE toggleData,,INPUT plEnabled LOGICAL PROCEDURE showMessageProcedure,,INPUT pcMessage CHARACTER,OUTPUT plAnswer LOGICAL PROCEDURE returnFocus,,INPUT hTarget HANDLE PROCEDURE repositionObject,,INPUT pdRow DECIMAL,INPUT pdCol DECIMAL PROCEDURE removeLink,,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE PROCEDURE removeAllLinks,, PROCEDURE modifyUserLinks,,INPUT pcMod CHARACTER,INPUT pcLinkName CHARACTER,INPUT phObject HANDLE PROCEDURE modifyListProperty,,INPUT phCaller HANDLE,INPUT pcMode CHARACTER,INPUT pcListName CHARACTER,INPUT pcListValue CHARACTER PROCEDURE hideObject,, PROCEDURE exitObject,, PROCEDURE editInstanceProperties,, PROCEDURE displayLinks,, PROCEDURE createControls,, PROCEDURE changeCursor,,INPUT pcCursor CHARACTER PROCEDURE applyEntry,,INPUT pcField CHARACTER PROCEDURE adjustTabOrder,,INPUT phObject HANDLE,INPUT phAnchor HANDLE,INPUT pcPosition CHARACTER PROCEDURE addMessage,,INPUT pcText CHARACTER,INPUT pcField CHARACTER,INPUT pcTable CHARACTER PROCEDURE addLink,,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE PROCEDURE unbindServer,,INPUT pcMode CHARACTER PROCEDURE startServerObject,, PROCEDURE runServerObject,,INPUT phAppService HANDLE PROCEDURE restartServerObject,, PROCEDURE initializeServerObject,, PROCEDURE disconnectObject,, PROCEDURE destroyServerObject,, PROCEDURE bindServer,, PROCEDURE processAction,,INPUT pcAction CHARACTER PROCEDURE enableObject,, PROCEDURE disableObject,, PROCEDURE applyLayout,, PROCEDURE viewPage,,INPUT piPageNum INTEGER PROCEDURE viewObject,, PROCEDURE selectPage,,INPUT piPageNum INTEGER PROCEDURE removePageNTarget,,INPUT phTarget HANDLE,INPUT piPage INTEGER PROCEDURE passThrough,,INPUT pcLinkName CHARACTER,INPUT pcArgument CHARACTER PROCEDURE notifyPage,,INPUT pcProc CHARACTER PROCEDURE initPages,,INPUT pcPageList CHARACTER PROCEDURE initializeVisualContainer,, PROCEDURE hidePage,,INPUT piPageNum INTEGER PROCEDURE destroyObject,, PROCEDURE deletePage,,INPUT piPageNum INTEGER PROCEDURE createObjects,, PROCEDURE constructObject,,INPUT pcProcName CHARACTER,INPUT phParent HANDLE,INPUT pcPropList CHARACTER,OUTPUT phObject HANDLE PROCEDURE changePage,, PROCEDURE assignPageProperty,,INPUT pcProp CHARACTER,INPUT pcValue CHARACTER PROCEDURE validateFields,,INPUT-OUTPUT pcNotValidFields CHARACTER PROCEDURE updateTitle,, PROCEDURE updateRecord,, PROCEDURE updateMode,,INPUT pcMode CHARACTER PROCEDURE showDataMessagesProcedure,,OUTPUT pcReturn CHARACTER PROCEDURE resetRecord,, PROCEDURE queryPosition,,INPUT pcState CHARACTER PROCEDURE okToContinueProcedure,,INPUT pcAction CHARACTER,OUTPUT plAnswer LOGICAL PROCEDURE deleteRecord,, PROCEDURE dataAvailable,,INPUT pcRelative CHARACTER PROCEDURE confirmExit,,INPUT-OUTPUT plCancel LOGICAL PROCEDURE confirmDelete,,INPUT-OUTPUT plAnswer LOGICAL PROCEDURE confirmContinue,,INPUT-OUTPUT plCancel LOGICAL PROCEDURE collectChanges,,INPUT-OUTPUT pcChanges CHARACTER,INPUT-OUTPUT pcInfo CHARACTER PROCEDURE viewRecord,, PROCEDURE valueChanged,, PROCEDURE updateState,,INPUT pcState CHARACTER PROCEDURE toolbar,,INPUT pcValue CHARACTER PROCEDURE initializeObject,, PROCEDURE enableFields,, PROCEDURE displayFields,,INPUT pcColValues CHARACTER PROCEDURE disableFields,,INPUT pcFieldType CHARACTER PROCEDURE copyRecord,, PROCEDURE cancelRecord,, PROCEDURE addRecord,, FUNCTION Signature,CHARACTER,INPUT pcName CHARACTER FUNCTION showmessage,LOGICAL,INPUT pcMessage CHARACTER FUNCTION setUserProperty,LOGICAL,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER FUNCTION setUIBMode,LOGICAL,INPUT pcMode CHARACTER FUNCTION setTranslatableProperties,LOGICAL,INPUT pcPropList CHARACTER FUNCTION setSupportedLinks,LOGICAL,INPUT pcLinkList CHARACTER FUNCTION setRunAttribute,LOGICAL,INPUT cRunAttribute CHARACTER FUNCTION setPhysicalVersion,LOGICAL,INPUT cVersion CHARACTER FUNCTION setPhysicalObjectName,LOGICAL,INPUT cTemp CHARACTER FUNCTION setPassThroughLinks,LOGICAL,INPUT pcLinks CHARACTER FUNCTION setParentDataKey,LOGICAL,INPUT cParentDataKey CHARACTER FUNCTION setObjectVersion,LOGICAL,INPUT cObjectVersion CHARACTER FUNCTION setObjectName,LOGICAL,INPUT pcName CHARACTER FUNCTION setLogicalVersion,LOGICAL,INPUT cVersion CHARACTER FUNCTION setInstanceProperties,LOGICAL,INPUT pcPropList CHARACTER FUNCTION setDynamicObject,LOGICAL,INPUT lTemp LOGICAL FUNCTION setDesignDataObject,LOGICAL,INPUT pcDataObject CHARACTER FUNCTION setDBAware,LOGICAL,INPUT lAware LOGICAL FUNCTION setDataTargetEvents,LOGICAL,INPUT pcEvents CHARACTER FUNCTION setDataTarget,LOGICAL,INPUT pcTarget CHARACTER FUNCTION setDataSourceNames,LOGICAL,INPUT pcSourceNames CHARACTER FUNCTION setDataSourceEvents,LOGICAL,INPUT pcEventsList CHARACTER FUNCTION setDataSource,LOGICAL,INPUT phObject HANDLE FUNCTION setDataLinksEnabled,LOGICAL,INPUT lDataLinksEnabled LOGICAL FUNCTION setContainerSourceEvents,LOGICAL,INPUT pcEvents CHARACTER FUNCTION setContainerSource,LOGICAL,INPUT phObject HANDLE FUNCTION setContainerHidden,LOGICAL,INPUT plHidden LOGICAL FUNCTION setChildDataKey,LOGICAL,INPUT cChildDataKey CHARACTER FUNCTION reviewMessages,CHARACTER, FUNCTION propertyType,CHARACTER,INPUT pcPropName CHARACTER FUNCTION messageNumber,CHARACTER,INPUT piMessage INTEGER FUNCTION mappedEntry,CHARACTER,INPUT pcEntry CHARACTER,INPUT pcList CHARACTER,INPUT plFirst LOGICAL,INPUT pcDelimiter CHARACTER FUNCTION linkProperty,CHARACTER,INPUT pcLink CHARACTER,INPUT pcPropName CHARACTER FUNCTION linkHandles,CHARACTER,INPUT pcLink CHARACTER FUNCTION instancePropertyList,CHARACTER,INPUT pcPropList CHARACTER FUNCTION getUserProperty,CHARACTER,INPUT pcPropName CHARACTER FUNCTION getUIBMode,CHARACTER, FUNCTION getTranslatableProperties,CHARACTER, FUNCTION getSupportedLinks,CHARACTER, FUNCTION getRunAttribute,CHARACTER, FUNCTION getQueryObject,LOGICAL, FUNCTION getPropertyDialog,CHARACTER, FUNCTION getPhysicalVersion,CHARACTER, FUNCTION getPhysicalObjectName,CHARACTER, FUNCTION getPassThroughLinks,CHARACTER, FUNCTION getParentDataKey,CHARACTER, FUNCTION getObjectVersionNumber,CHARACTER, FUNCTION getObjectVersion,CHARACTER, FUNCTION getObjectPage,INTEGER, FUNCTION getObjectName,CHARACTER, FUNCTION getObjectInitialized,LOGICAL, FUNCTION getObjectHidden,LOGICAL, FUNCTION getLogicalVersion,CHARACTER, FUNCTION getLogicalObjectName,CHARACTER, FUNCTION getInstanceProperties,CHARACTER, FUNCTION getDynamicObject,LOGICAL, FUNCTION getDesignDataObject,CHARACTER, FUNCTION getDBAware,LOGICAL, FUNCTION getDataTargetEvents,CHARACTER, FUNCTION getDataTarget,CHARACTER, FUNCTION getDataSourceNames,CHARACTER, FUNCTION getDataSourceEvents,CHARACTER, FUNCTION getDataSource,HANDLE, FUNCTION getDataLinksEnabled,LOGICAL, FUNCTION getContainerType,CHARACTER, FUNCTION getContainerSourceEvents,CHARACTER, FUNCTION getContainerSource,HANDLE, FUNCTION getContainerHidden,LOGICAL, FUNCTION getContainerHandle,HANDLE, FUNCTION getChildDataKey,CHARACTER, FUNCTION fetchMessages,CHARACTER, FUNCTION assignLinkProperty,LOGICAL,INPUT pcLink CHARACTER,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER FUNCTION anyMessage,LOGICAL, FUNCTION setServerOperatingMode,LOGICAL,INPUT pcServerOperatingMode CHARACTER FUNCTION setServerFileName,LOGICAL,INPUT pcFileName CHARACTER FUNCTION setASUsePrompt,LOGICAL,INPUT plFlag LOGICAL FUNCTION setASInitializeOnRun,LOGICAL,INPUT plInitialize LOGICAL FUNCTION setASInfo,LOGICAL,INPUT pcInfo CHARACTER FUNCTION setASHandle,LOGICAL,INPUT phASHandle HANDLE FUNCTION setASDivision,LOGICAL,INPUT pcDivision CHARACTER FUNCTION setAppService,LOGICAL,INPUT pcAppService CHARACTER FUNCTION runServerProcedure,HANDLE,INPUT pcServerFileName CHARACTER,INPUT phAppService HANDLE FUNCTION getServerOperatingMode,CHARACTER, FUNCTION getServerFileName,CHARACTER, FUNCTION getASUsePrompt,LOGICAL, FUNCTION getASInitializeOnRun,LOGICAL, FUNCTION getASInfo,CHARACTER, FUNCTION getASHasStarted,LOGICAL, FUNCTION getASHandle,HANDLE, FUNCTION getAsDivision,CHARACTER, FUNCTION getASBound,LOGICAL, FUNCTION getAppService,CHARACTER, FUNCTION createUiEvents,LOGICAL, FUNCTION getObjectSecured,LOGICAL, FUNCTION getObjectTranslated,LOGICAL, FUNCTION setResizeVertical,LOGICAL,INPUT plResizeVertical LOGICAL FUNCTION setResizeHorizontal,LOGICAL,INPUT plResizeHorizontal LOGICAL FUNCTION setObjectLayout,LOGICAL,INPUT pcLayout CHARACTER FUNCTION setLayoutOptions,LOGICAL,INPUT pcOptions CHARACTER FUNCTION setHideOnInit,LOGICAL,INPUT plHide LOGICAL FUNCTION setDisableOnInit,LOGICAL,INPUT plDisable LOGICAL FUNCTION setDefaultLayout,LOGICAL,INPUT pcDefault CHARACTER FUNCTION setAllFieldNames,LOGICAL,INPUT pcValue CHARACTER FUNCTION setAllFieldHandles,LOGICAL,INPUT pcValue CHARACTER FUNCTION getResizeVertical,LOGICAL, FUNCTION getResizeHorizontal,LOGICAL, FUNCTION getWidth,DECIMAL, FUNCTION getRow,DECIMAL, FUNCTION getObjectLayout,CHARACTER, FUNCTION getObjectEnabled,LOGICAL, FUNCTION getLayoutVariable,CHARACTER, FUNCTION getLayoutOptions,CHARACTER, FUNCTION getHideOnInit,LOGICAL, FUNCTION getHeight,DECIMAL, FUNCTION getEnabledObjHdls,CHARACTER, FUNCTION getEnabledObjFlds,CHARACTER, FUNCTION getDisableOnInit,LOGICAL, FUNCTION getDefaultLayout,CHARACTER, FUNCTION getCol,DECIMAL, FUNCTION getAllFieldNames,CHARACTER, FUNCTION getAllFieldHandles,CHARACTER, FUNCTION setStatusArea,LOGICAL,INPUT plStatusArea LOGICAL FUNCTION setWindowTitleViewer,LOGICAL,INPUT phViewer HANDLE FUNCTION setWaitForObject,LOGICAL,INPUT phObject HANDLE FUNCTION setUpdateSource,LOGICAL,INPUT pcSource CHARACTER FUNCTION setTopOnly,LOGICAL,INPUT plTopOnly LOGICAL FUNCTION setSdoForeignFields,LOGICAL,INPUT cSdoForeignFields CHARACTER FUNCTION setSavedContainerMode,LOGICAL,INPUT cSavedContainerMode CHARACTER FUNCTION setRunMultiple,LOGICAL,INPUT plMultiple LOGICAL FUNCTION setRunDOOptions,LOGICAL,INPUT pcOptions CHARACTER FUNCTION setRouterTarget,LOGICAL,INPUT phObject HANDLE FUNCTION setReEnableDataLinks,LOGICAL,INPUT cReEnableDataLinks CHARACTER FUNCTION setPrimarySdoTarget,LOGICAL,INPUT hPrimarySdoTarget HANDLE FUNCTION setPageSource,LOGICAL,INPUT phObject HANDLE FUNCTION setPageNTarget,LOGICAL,INPUT pcObject CHARACTER FUNCTION setOutMessageTarget,LOGICAL,INPUT phObject HANDLE FUNCTION setNavigationTarget,LOGICAL,INPUT cTarget CHARACTER FUNCTION setNavigationSourceEvents,LOGICAL,INPUT pcEvents CHARACTER FUNCTION setNavigationSource,LOGICAL,INPUT pcSource CHARACTER FUNCTION setMultiInstanceSupported,LOGICAL,INPUT lMultiInstanceSupported LOGICAL FUNCTION setMultiInstanceActivated,LOGICAL,INPUT lMultiInstanceActivated LOGICAL FUNCTION setInMessageTarget,LOGICAL,INPUT phObject HANDLE FUNCTION setFilterSource,LOGICAL,INPUT phObject HANDLE FUNCTION setDynamicSDOProcedure,LOGICAL,INPUT pcProc CHARACTER FUNCTION setDisabledAddModeTabs,LOGICAL,INPUT cDisabledAddModeTabs CHARACTER FUNCTION setCurrentPage,LOGICAL,INPUT iPage INTEGER FUNCTION setContainerTarget,LOGICAL,INPUT pcObject CHARACTER FUNCTION setCallerWindow,LOGICAL,INPUT h HANDLE FUNCTION setCallerProcedure,LOGICAL,INPUT h HANDLE FUNCTION setCallerObject,LOGICAL,INPUT h HANDLE FUNCTION pageNTargets,CHARACTER,INPUT phTarget HANDLE,INPUT piPageNum INTEGER FUNCTION getStatusArea,LOGICAL, FUNCTION getWindowTitleViewer,HANDLE, FUNCTION getWaitForObject,HANDLE, FUNCTION getUpdateSource,CHARACTER, FUNCTION getTopOnly,LOGICAL, FUNCTION getSdoForeignFields,CHARACTER, FUNCTION getSavedContainerMode,CHARACTER, FUNCTION getRunMultiple,LOGICAL, FUNCTION getRunDOOptions,CHARACTER, FUNCTION getReEnableDataLinks,CHARACTER, FUNCTION getPrimarySdoTarget,HANDLE, FUNCTION getPageSource,HANDLE, FUNCTION getPageNTarget,CHARACTER, FUNCTION getOutMessageTarget,HANDLE, FUNCTION getNavigationTarget,HANDLE, FUNCTION getNavigationSourceEvents,CHARACTER, FUNCTION getNavigationSource,CHARACTER, FUNCTION getMultiInstanceSupported,LOGICAL, FUNCTION getMultiInstanceActivated,LOGICAL, FUNCTION getFilterSource,HANDLE, FUNCTION getDynamicSDOProcedure,CHARACTER, FUNCTION getDisabledAddModeTabs,CHARACTER, FUNCTION getCurrentPage,INTEGER, FUNCTION getContainerTargetEvents,CHARACTER, FUNCTION getContainerTarget,CHARACTER, FUNCTION getContainerMode,CHARACTER, FUNCTION getCallerWindow,HANDLE, FUNCTION getCallerProcedure,HANDLE, FUNCTION enablePagesInFolder,LOGICAL,INPUT pcPageInformation CHARACTER FUNCTION disablePagesInFolder,LOGICAL,INPUT pcPageInformation CHARACTER FUNCTION showDataMessages,CHARACTER, FUNCTION setWindowTitleField,LOGICAL,INPUT cWindowTitleField CHARACTER FUNCTION setUpdateTargetNames,LOGICAL,INPUT pcTargetNames CHARACTER FUNCTION setUpdateTarget,LOGICAL,INPUT pcObject CHARACTER FUNCTION setTableIOSourceEvents,LOGICAL,INPUT pcEvents CHARACTER FUNCTION setTableIOSource,LOGICAL,INPUT phObject HANDLE FUNCTION setSaveSource,LOGICAL,INPUT plSave LOGICAL FUNCTION setObjectParent,LOGICAL,INPUT phParent HANDLE FUNCTION setLogicalObjectName,LOGICAL,INPUT pcLogicalObjectName CHARACTER FUNCTION setGroupAssignTargetEvents,LOGICAL,INPUT pcEvents CHARACTER FUNCTION setGroupAssignTarget,LOGICAL,INPUT pcObject CHARACTER FUNCTION setGroupAssignSourceEvents,LOGICAL,INPUT pcEvents CHARACTER FUNCTION setGroupAssignSource,LOGICAL,INPUT phObject HANDLE FUNCTION setEnabledFields,LOGICAL,INPUT pcFieldList CHARACTER FUNCTION setDisplayedFields,LOGICAL,INPUT pcFieldList CHARACTER FUNCTION setDataModified,LOGICAL,INPUT plModified LOGICAL FUNCTION setContainerMode,LOGICAL,INPUT pcContainerMode CHARACTER FUNCTION okToContinue,LOGICAL,INPUT pcAction CHARACTER FUNCTION getWindowTitleField,CHARACTER, FUNCTION getUpdateTargetNames,CHARACTER, FUNCTION getUpdateTarget,CHARACTER, FUNCTION getTableIOSourceEvents,CHARACTER, FUNCTION getTableIOSource,HANDLE, FUNCTION getRowIdent,CHARACTER, FUNCTION getRecordState,CHARACTER, FUNCTION getObjectParent,HANDLE, FUNCTION getNewRecord,CHARACTER, FUNCTION getGroupAssignTargetEvents,CHARACTER, FUNCTION getGroupAssignTarget,CHARACTER, FUNCTION getGroupAssignSourceEvents,CHARACTER, FUNCTION getGroupAssignSource,HANDLE, FUNCTION getFieldsEnabled,LOGICAL, FUNCTION getFieldHandles,CHARACTER, FUNCTION getEnabledHandles,CHARACTER, FUNCTION getEnabledFields,CHARACTER, FUNCTION getDisplayedTables,CHARACTER, FUNCTION getDisplayedFields,CHARACTER, FUNCTION getDataModified,LOGICAL, FUNCTION getCreateHandles,CHARACTER, FUNCTION setShowPopup,LOGICAL,INPUT plShowPopup LOGICAL FUNCTION getShowPopup,LOGICAL, FUNCTION getObjectType,character, FUNCTION getTargetProcedure,HANDLE, FUNCTION widgetValueList,CHARACTER,INPUT pcNameList CHARACTER,INPUT pcDelimiter CHARACTER FUNCTION widgetValue,CHARACTER,INPUT pcName CHARACTER FUNCTION widgetIsTrue,LOGICAL,INPUT pcName CHARACTER FUNCTION widgetIsModified,LOGICAL,INPUT pcNameList CHARACTER FUNCTION widgetIsFocused,LOGICAL,INPUT pcName CHARACTER FUNCTION widgetIsBlank,LOGICAL,INPUT pcNameList CHARACTER FUNCTION widgetLongcharValue,LONGCHAR,INPUT pcName CHARACTER FUNCTION widgetHandle,HANDLE,INPUT pcName CHARACTER FUNCTION viewWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION toggleWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION resetWidgetValue,LOGICAL,INPUT pcNameList CHARACTER FUNCTION highlightWidget,LOGICAL,INPUT pcNameList CHARACTER,INPUT pcHighlightType CHARACTER FUNCTION hideWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION formattedWidgetValueList,CHARACTER,INPUT pcNameList CHARACTER,INPUT pcDelimiter CHARACTER FUNCTION formattedWidgetValue,CHARACTER,INPUT pcName CHARACTER FUNCTION enableWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION enableRadioButton,LOGICAL,INPUT pcNameList CHARACTER,INPUT piButtonNum INTEGER FUNCTION disableWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION disableRadioButton,LOGICAL,INPUT pcNameList CHARACTER,INPUT piButtonNum INTEGER FUNCTION clearWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION blankWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION assignWidgetValueList,LOGICAL,INPUT pcNameList CHARACTER,INPUT pcValueList CHARACTER,INPUT pcDelimiter CHARACTER FUNCTION assignWidgetValue,LOGICAL,INPUT pcName CHARACTER,INPUT pcValue CHARACTER FUNCTION assignFocusedWidget,LOGICAL,INPUT pcName CHARACTER                    4             ѷ    �              �o              ,    +   �� �  U   \� `  V   �� �   Z   �� t  ]           $� �  ? �� -&  iSO8859-1                                                                           �    �                                      �                   �                    p     �   ��    �             ��  �   �      �                                                         PROGRESS                                    
    
                    �              �                                                                                                     
  4         �          X  �  4        �      @                       x          �      �   �  �      ,  
    
                    �             �                                                                                          �          
  \  �      �  
    
                  �  �             H                                                                                          �          
    �      �  
    
                  p  8             �                                                                                          �          
  �  �      0  
    
                    �             �                                                                                          �          
  `  �      �  
    
                  �  �             L                                                                                          �          
    �      �  
    
                  t  <             �                                                                                          �          
  �        4  
    
                     �  	           �                                                                                                    
  d  '      �  
    
                  �  �  
           P                                                                                          '          
    5      �                         x  @             �                                                                                          5            �  B      8                        $  �             �                                                                                          B            h	  P      �  
    
                  �  �	             T	                                                                                          P          
  
  ^      �	  
    
                  |	  D
              
                                                                                          ^          
  �
  l      <
  
    
                  (
  �
             �
                                                                                          l          
  l  z      �
                        �
  �             X                                                                                          z              �      �                        �  H                                                                                                       �            �  �      @                        ,  �             �                                                                                          �                �      �                        �                 \                                                                                          �                          t�                                               x�          �  0  H X�            
             
             
                                         
                                                                                                                                                                        H   X   h   x   �   �   �   �   �   �   �   �       (  8  H      H   X   h   x   �   �   �   �   �   �   �   �      (  8  H                                                                                                                                     	                  
                                                                                                                                                                                                   8  @  H  P              T             p  |  �  �              �             �  �  �  �              �             �  �  �                                ,  4  @              D             \  d  l  t              x             �  �  �  �              �             �  �  �  �         �  P             p  x  �  �      �  �  �             �  �           8    L             x  �  �  �              �             �  �  �  �              �             �                                 0  8  @  H              L             h  t  |  �              �             �  �  �  �      $  �  H             d  l  t  |              �             �  �  �  �              �                                                         Address x(35)   Address     Please enter an address.    Address2    x(35)   Address2        Please enter an address.    Balance ->,>>>,>>9.99   Balance 0   Please enter a balance. City    x(25)   City        Please enter a city.    Comments    x(80)   Comments        Please enter comments.  Contact x(30)   Contact     Please enter a contact. Country x(20)   Country USA Please enter a country. CreditLimit ->,>>>,>>9  Credit Limit    1500    Credit Limit must be >= 0 and <= 9,999,999  CreditLimit >= 0 AND CreditLimit <= 9999999 .   Please enter a Credit Limit.    CustNum >>>>9   Cust Num    0   Customer number must be greater than zero   custnum > 0 .   Please enter a customer number. Discount    >>9%    Discount    0   Discount must be greater or equal to 0  Discount >= 0 .     Please enter a percentage from 0 to 100.    EmailAddress    x(50)   Email       Please enter an full Internet Email Address.    Fax x(20)   Fax     Please enter a fax number.  Name    x(30)   Name        Please enter a name.    Phone   x(20)   Phone       Please enter a phone number PostalCode  x(10)   Postal Code     Please enter the appropriate Postal Code.   SalesRep    x(4)    Sales Rep       The Sales Rep's name you've entered must exist in the Salesrep table.   CAN-FIND ( Salesrep OF RowObject )  Please Enter a Sales Rep.   State   x(20)   State       Please enter standard state abbreviation.   Terms   x(20)   Terms   Net30   Please enter terms  �  %�  ���������      USA�          Net30�    �%                �     i     	       "   +   3   8   A   I   Q   ]   e   n   {      �   �   �   �   �     ��                                               )          ����                            undefined                                                               �           �   l                             �����               ���                    O   ����    e�          O   ����    R�          O   ����    ��      t       �   �              4   ����     /                                    3   ����       $     H  ���                       8      
                       � ߱        �  �      D       p
     S          assignFocusedWidget         �      �     �       LOGICAL,INPUT pcName CHARACTER  assignWidgetValue   �      �      $    �       LOGICAL,INPUT pcName CHARACTER,INPUT pcValue CHARACTER  assignWidgetValueList         \      �    �       LOGICAL,INPUT pcNameList CHARACTER,INPUT pcValueList CHARACTER,INPUT pcDelimiter CHARACTER  blankWidget t      �          �       LOGICAL,INPUT pcNameList CHARACTER  clearWidget �      @      l    �       LOGICAL,INPUT pcNameList CHARACTER  disableRadioButton  L      �      �    �       LOGICAL,INPUT pcNameList CHARACTER,INPUT piButtonNum INTEGER    disableWidget   �            4          LOGICAL,INPUT pcNameList CHARACTER  enableRadioButton         X      �          LOGICAL,INPUT pcNameList CHARACTER,INPUT piButtonNum INTEGER    enableWidget    l      �      �    1      LOGICAL,INPUT pcNameList CHARACTER  formattedWidgetValue    �             X  	  >      CHARACTER,INPUT pcName CHARACTER    formattedWidgetValueList    8      |      �  
  S      CHARACTER,INPUT pcNameList CHARACTER,INPUT pcDelimiter CHARACTER    hideWidget  �      �      (   
 l      LOGICAL,INPUT pcNameList CHARACTER  highlightWidget       L      |    w      LOGICAL,INPUT pcNameList CHARACTER,INPUT pcHighlightType CHARACTER  resetWidgetValue    \      �      �    �      LOGICAL,INPUT pcNameList CHARACTER  toggleWidget    �            H    �      LOGICAL,INPUT pcNameList CHARACTER  viewWidget  (      l      �   
 �      LOGICAL,INPUT pcNameList CHARACTER  widgetHandle    x      �      �    �      HANDLE,INPUT pcName CHARACTER   widgetLongcharValue �            @    �      LONGCHAR,INPUT pcName CHARACTER widgetIsBlank          `      �    �      LOGICAL,INPUT pcNameList CHARACTER  widgetIsFocused p      �      �    �      LOGICAL,INPUT pcName CHARACTER  widgetIsModified    �      	      8	    �      LOGICAL,INPUT pcNameList CHARACTER  widgetIsTrue    	      \	      �	           LOGICAL,INPUT pcName CHARACTER  widgetValue l	      �	      �	          CHARACTER,INPUT pcName CHARACTER    widgetValueList �	      �	      ,
          CHARACTER,INPUT pcNameList CHARACTER,INPUT pcDelimiter CHARACTER    d�    X  �
        d       4   ����d                                       ��                  X  \                  �P3                       X  �
  \  	  Y  L                                        3   ����|       O   [  ��  ��  �   addRecord                                 �      ��                    	                �.                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            cancelRecord                                  �      ��                                     H��                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            copyRecord                                �      ��                                     ���                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            disableFields                                 �      ��                      $              ��                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  <           ��                            ����                            displayFields                               8         ��                      P              pr�                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  h           ��                            ����                            enableFields                                d  L      ��                      |              �4=                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeObject                                l  T      ��                      �              T5=                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            toolbar                             h  P      ��                    !  �              �                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �           ��                            ����                            updateState                             �  x      ��                  #  %  �              �n                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �           ��                            ����                            valueChanged                                �  �      ��                  '  (  �              o                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            viewRecord                              �  �      ��                  *  +  �              �o                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            getTargetProcedure  
      ,      `    )      HANDLE, getObjectType   @      h      �    <      CHARACTER,  getShowPopup    x      �      �    J      LOGICAL,    setShowPopup    �      �          W      LOGICAL,INPUT plShowPopup LOGICAL   addRecord                               �  �      ��                  �  �  �              @>*                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            cancelRecord                                �  �      ��                  �  �  �              @?*                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            collectChanges                              �  �      ��                  �  �  �              ��<                    O   ����    e�          O   ����    R�          O   ����    ��            ��   0             �               ��                  $           ��                            ����                            confirmContinue                                      ��                  �  �  8              8E�                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  P           ��                            ����                            confirmDelete                               L  4      ��                  �  �  d              �d                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  |           ��                            ����                            confirmExit                             t  \      ��                  �  �  �              ��:                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �           ��                            ����                            copyRecord                              �  �      ��                  �  �  �              ���                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            dataAvailable                               �   �       ��                  �  �  �               ���                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �            ��                            ����                            deleteRecord                                �!  �!      ��                  �  �  �!              s�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeObject                                �"  �"      ��                  �  �  �"              tu�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            okToContinueProcedure                               �#  �#      ��                  �  �  �#              (�0                    O   ����    e�          O   ����    R�          O   ����    ��            ��   D$             $               ��                  8$           ��                            ����                            queryPosition                               4%  %      ��                  �  �  L%              ��x                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  d%           ��                            ����                            resetRecord                             \&  D&      ��                  �  �  t&              ��l                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            showDataMessagesProcedure                               l'  T'      ��                  �  �  �'              D�l                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �'           ��                            ����                            updateMode                              �(  |(      ��                  �  �  �(              �[                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �(           ��                            ����                            updateRecord                                �)  �)      ��                  �  �  �)              ļO                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            updateState                             �*  �*      ��                  �  �  �*              $�O                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �*           ��                            ����                            updateTitle                             �+  �+      ��                  �  �   ,              (�O                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            validateFields                              �,  �,      ��                  �  �  -              �:                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  -           ��                            ����                            getCreateHandles    �      �-      �-    d      CHARACTER,  getDataModified �-      �-      �-    u      LOGICAL,    getDisplayedFields  �-       .      4.    �      CHARACTER,  getDisplayedTables  .      @.      t.    �      CHARACTER,  getEnabledFields    T.      �.      �.     �      CHARACTER,  getEnabledHandles   �.      �.      �.  !  �      CHARACTER,  getFieldHandles �.       /      0/  "  �      CHARACTER,  getFieldsEnabled    /      </      p/  #  �      LOGICAL,    getGroupAssignSource    P/      |/      �/  $  �      HANDLE, getGroupAssignSourceEvents  �/      �/      �/  %        CHARACTER,  getGroupAssignTarget    �/      0      <0  &        CHARACTER,  getGroupAssignTargetEvents  0      H0      �0  '  4      CHARACTER,  getNewRecord    d0      �0      �0  (  O      CHARACTER,  getObjectParent �0      �0      �0  )  \      HANDLE, getRecordState  �0      1      41  *  l      CHARACTER,  getRowIdent 1      @1      l1  +  {      CHARACTER,  getTableIOSource    L1      x1      �1  ,  �      HANDLE, getTableIOSourceEvents  �1      �1      �1  -  �      CHARACTER,  getUpdateTarget �1      �1      (2  .  �      CHARACTER,  getUpdateTargetNames    2      42      l2  /  �      CHARACTER,  getWindowTitleField L2      x2      �2  0  �      CHARACTER,  okToContinue    �2      �2      �2  1  �      LOGICAL,INPUT pcAction CHARACTER    setContainerMode    �2      3      @3  2  �      LOGICAL,INPUT pcContainerMode CHARACTER setDataModified  3      h3      �3  3        LOGICAL,INPUT plModified LOGICAL    setDisplayedFields  x3      �3      �3  4        LOGICAL,INPUT pcFieldList CHARACTER setEnabledFields    �3      4      H4  5  )      LOGICAL,INPUT pcFieldList CHARACTER setGroupAssignSource    (4      l4      �4  6  :      LOGICAL,INPUT phObject HANDLE   setGroupAssignSourceEvents  �4      �4       5  7  O      LOGICAL,INPUT pcEvents CHARACTER    setGroupAssignTarget    �4      $5      \5  8  j      LOGICAL,INPUT pcObject CHARACTER    setGroupAssignTargetEvents  <5      �5      �5  9        LOGICAL,INPUT pcEvents CHARACTER    setLogicalObjectName    �5      �5      6  :  �      LOGICAL,INPUT pcLogicalObjectName CHARACTER setObjectParent �5      D6      t6  ;  �      LOGICAL,INPUT phParent HANDLE   setSaveSource   T6      �6      �6  <  �      LOGICAL,INPUT plSave LOGICAL    setTableIOSource    �6      �6      7  =  �      LOGICAL,INPUT phObject HANDLE   setTableIOSourceEvents  �6      87      p7  >  �      LOGICAL,INPUT pcEvents CHARACTER    setUpdateTarget P7      �7      �7  ?  �      LOGICAL,INPUT pcObject CHARACTER    setUpdateTargetNames    �7      �7       8  @        LOGICAL,INPUT pcTargetNames CHARACTER   setWindowTitleField  8      H8      |8  A        LOGICAL,INPUT cWindowTitleField CHARACTER   showDataMessages    \8      �8      �8  B  .      CHARACTER,  assignPageProperty                              �9  h9      ��                  �    �9              $1p                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �9             �9               ��                  �9           ��                            ����                            changePage                              �:  �:      ��                      �:              2                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            confirmExit                             �;  �;      ��                      �;              |4                    O   ����    e�          O   ����    R�          O   ����    ��            ��                   <           ��                            ����                            constructObject                             �<  �<      ��                  
    =              ��V                    O   ����    e�          O   ����    R�          O   ����    ��            ��   `=             ,=               �� 
  �=             T=  
             ��   �=             |=               �� 
                 �=  
         ��                            ����                            createObjects                               �>  �>      ��                      �>              h��                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            deletePage                              �?  �?      ��                      �?              D1                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �?           ��                            ����                            destroyObject                               �@  �@      ��                      �@              @T�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            hidePage                                �A  �A      ��                      �A              �V�                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �A           ��                            ����                            initializeObject                                �B  �B      ��                       C              ��                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeVisualContainer                               D  �C      ��                  "  #  $D              $��                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initPages                               E  �D      ��                  %  '  $E              ܌�                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  <E           ��                            ����                            notifyPage                              4F  F      ��                  )  +  LF              �s�                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  dF           ��                            ����                            passThrough                             \G  DG      ��                  -  0  tG              ��                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �G             �G               ��                  �G           ��                            ����                            removePageNTarget                               �H  �H      ��                  2  5  �H               �]                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  I             �H  
             ��                  I           ��                            ����                            selectPage                              J  �I      ��                  7  9  J              �
H                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  4J           ��                            ����                            toolbar                             (K  K      ��                  ;  =  @K              lh-                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  XK           ��                            ����                            viewObject                              PL  8L      ��                  ?  @  hL              �3:                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            viewPage                                PM  8M      ��                  B  D  hM              <�"                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �M           ��                            ����                            disablePagesInFolder    �8      �M       N  C  ?      LOGICAL,INPUT pcPageInformation CHARACTER   enablePagesInFolder  N      LN      �N  D  T      LOGICAL,INPUT pcPageInformation CHARACTER   getCallerProcedure  `N      �N      �N  E  h      HANDLE, getCallerWindow �N      �N      O  F  {      HANDLE, getContainerMode    �N       O      TO  G  �      CHARACTER,  getContainerTarget  4O      `O      �O  H  �      CHARACTER,  getContainerTargetEvents    tO      �O      �O  I  �      CHARACTER,  getCurrentPage  �O      �O      P  J  �      INTEGER,    getDisabledAddModeTabs  �O      $P      \P  K  �      CHARACTER,  getDynamicSDOProcedure  <P      hP      �P  L  �      CHARACTER,  getFilterSource �P      �P      �P  M        HANDLE, getMultiInstanceActivated   �P      �P       Q  N        LOGICAL,    getMultiInstanceSupported    Q      ,Q      hQ  O  /      LOGICAL,    getNavigationSource HQ      tQ      �Q  P  I      CHARACTER,  getNavigationSourceEvents   �Q      �Q      �Q  Q  ]      CHARACTER,  getNavigationTarget �Q      �Q      0R  R  w      HANDLE, getOutMessageTarget R      8R      lR  S  �      HANDLE, getPageNTarget  LR      tR      �R  T  �      CHARACTER,  getPageSource   �R      �R      �R  U  �      HANDLE, getPrimarySdoTarget �R      �R      S  V  �      HANDLE, getReEnableDataLinks    �R      $S      \S  W  �      CHARACTER,  getRunDOOptions <S      hS      �S  X  �      CHARACTER,  getRunMultiple  xS      �S      �S  Y  �      LOGICAL,    getSavedContainerMode   �S      �S      T  Z        CHARACTER,  getSdoForeignFields �S      $T      XT  [        CHARACTER,  getTopOnly  8T      dT      �T  \ 
 .      LOGICAL,    getUpdateSource pT      �T      �T  ]  9      CHARACTER,  getWaitForObject    �T      �T      U  ^  I      HANDLE, getWindowTitleViewer    �T      U      LU  _  Z      HANDLE, getStatusArea   ,U      TU      �U  `  o      LOGICAL,    pageNTargets    dU      �U      �U  a  }      CHARACTER,INPUT phTarget HANDLE,INPUT piPageNum INTEGER setCallerObject �U      �U      (V  b  �      LOGICAL,INPUT h HANDLE  setCallerProcedure  V      @V      tV  c  �      LOGICAL,INPUT h HANDLE  setCallerWindow TV      �V      �V  d  �      LOGICAL,INPUT h HANDLE  setContainerTarget  �V      �V      W  e  �      LOGICAL,INPUT pcObject CHARACTER    setCurrentPage  �V      ,W      \W  f  �      LOGICAL,INPUT iPage INTEGER setDisabledAddModeTabs  <W      xW      �W  g  �      LOGICAL,INPUT cDisabledAddModeTabs CHARACTER    setDynamicSDOProcedure  �W      �W      X  h  �      LOGICAL,INPUT pcProc CHARACTER  setFilterSource �W      8X      hX  i  	      LOGICAL,INPUT phObject HANDLE   setInMessageTarget  HX      �X      �X  j  	      LOGICAL,INPUT phObject HANDLE   setMultiInstanceActivated   �X      �X      Y  k  0	      LOGICAL,INPUT lMultiInstanceActivated LOGICAL   setMultiInstanceSupported   �X      HY      �Y  l  J	      LOGICAL,INPUT lMultiInstanceSupported LOGICAL   setNavigationSource dY      �Y      �Y  m  d	      LOGICAL,INPUT pcSource CHARACTER    setNavigationSourceEvents   �Y      Z      HZ  n  x	      LOGICAL,INPUT pcEvents CHARACTER    setNavigationTarget (Z      lZ      �Z  o  �	      LOGICAL,INPUT cTarget CHARACTER setOutMessageTarget �Z      �Z      �Z  p  �	      LOGICAL,INPUT phObject HANDLE   setPageNTarget  �Z      [      D[  q  �	      LOGICAL,INPUT pcObject CHARACTER    setPageSource   $[      h[      �[  r  �	      LOGICAL,INPUT phObject HANDLE   setPrimarySdoTarget x[      �[      �[  s  �	      LOGICAL,INPUT hPrimarySdoTarget HANDLE  setReEnableDataLinks    �[      \      L\  t  �	      LOGICAL,INPUT cReEnableDataLinks CHARACTER  setRouterTarget ,\      x\      �\  u   
      LOGICAL,INPUT phObject HANDLE   setRunDOOptions �\      �\      �\  v  
      LOGICAL,INPUT pcOptions CHARACTER   setRunMultiple  �\      ]      L]  w   
      LOGICAL,INPUT plMultiple LOGICAL    setSavedContainerMode   ,]      p]      �]  x  /
      LOGICAL,INPUT cSavedContainerMode CHARACTER setSdoForeignFields �]      �]      ^  y  E
      LOGICAL,INPUT cSdoForeignFields CHARACTER   setTopOnly  �]      4^      `^  z 
 Y
      LOGICAL,INPUT plTopOnly LOGICAL setUpdateSource @^      �^      �^  {  d
      LOGICAL,INPUT pcSource CHARACTER    setWaitForObject    �^      �^      _  |  t
      LOGICAL,INPUT phObject HANDLE   setWindowTitleViewer    �^      (_      `_  }  �
      LOGICAL,INPUT phViewer HANDLE   setStatusArea   @_      �_      �_  ~  �
      LOGICAL,INPUT plStatusArea LOGICAL  applyLayout                             d`  L`      ��                  �  �  |`              xe                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            disableObject                               ha  Pa      ��                  �  �  �a              e                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            enableObject                                lb  Tb      ��                  �  �  �b              h	e                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeObject                                tc  \c      ��                  �  �  �c              �	e                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            processAction                               xd  `d      ��                  �  �  �d              (B3                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �d           ��                            ����                            getAllFieldHandles  �_      e      De    �
      CHARACTER,  getAllFieldNames    $e      Pe      �e  �  �
      CHARACTER,  getCol  de      �e      �e  �  �
      DECIMAL,    getDefaultLayout    �e      �e      �e  �  �
      CHARACTER,  getDisableOnInit    �e      f      8f  �  �
      LOGICAL,    getEnabledObjFlds   f      Df      xf  �  �
      CHARACTER,  getEnabledObjHdls   Xf      �f      �f  �        CHARACTER,  getHeight   �f      �f      �f  � 	       DECIMAL,    getHideOnInit   �f      �f      ,g  �  #      LOGICAL,    getLayoutOptions    g      8g      lg  �  1      CHARACTER,  getLayoutVariable   Lg      xg      �g  �  B      CHARACTER,  getObjectEnabled    �g      �g      �g  �  T      LOGICAL,    getObjectLayout �g      �g      (h  �  e      CHARACTER,  getRow  h      4h      \h  �  u      DECIMAL,    getWidth    <h      hh      �h  �  |      DECIMAL,    getResizeHorizontal th      �h      �h  �  �      LOGICAL,    getResizeVertical   �h      �h      i  �  �      LOGICAL,    setAllFieldHandles  �h       i      Ti  �  �      LOGICAL,INPUT pcValue CHARACTER setAllFieldNames    4i      ti      �i  �  �      LOGICAL,INPUT pcValue CHARACTER setDefaultLayout    �i      �i      �i  �  �      LOGICAL,INPUT pcDefault CHARACTER   setDisableOnInit    �i       j      Tj  �  �      LOGICAL,INPUT plDisable LOGICAL setHideOnInit   4j      tj      �j  �  �      LOGICAL,INPUT plHide LOGICAL    setLayoutOptions    �j      �j      �j  �  �      LOGICAL,INPUT pcOptions CHARACTER   setObjectLayout �j      k      Lk  �        LOGICAL,INPUT pcLayout CHARACTER    setResizeHorizontal ,k      pk      �k  �         LOGICAL,INPUT plResizeHorizontal LOGICAL    setResizeVertical   �k      �k      l  �  4      LOGICAL,INPUT plResizeVertical LOGICAL  getObjectTranslated �k      ,l      `l  �  F      LOGICAL,    getObjectSecured    @l      ll      �l  �  Z      LOGICAL,    createUiEvents  �l      �l      �l  �  k      LOGICAL,    bindServer                              xm  `m      ��                  �  �  �m              DU�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            destroyObject                               |n  dn      ��                  �  �  �n              �U�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            destroyServerObject                             �o  lo      ��                  �  �  �o              �:                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            disconnectObject                                �p  tp      ��                  �  �  �p              ��:                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeServerObject                              �q  �q      ��                  �  �  �q              H�:                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            restartServerObject                             �r  �r      ��                  �  �  �r              ,�:                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            runServerObject                             �s  �s      ��                  �  �  �s              ��:                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
                 �s  
         ��                            ����                            startServerObject                               �t  �t      ��                  �  �  �t              ��                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            unbindServer                                �u  �u      ��                  �  �  �u              H�2                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  v           ��                            ����                            getAppService   �l      pv      �v  �  z      CHARACTER,  getASBound  �v      �v      �v  � 
 �      LOGICAL,    getAsDivision   �v      �v      w  �  �      CHARACTER,  getASHandle �v       w      Lw  �  �      HANDLE, getASHasStarted ,w      Tw      �w  �  �      LOGICAL,    getASInfo   dw      �w      �w  � 	 �      CHARACTER,  getASInitializeOnRun    �w      �w       x  �  �      LOGICAL,    getASUsePrompt  �w      x      <x  �  �      LOGICAL,    getServerFileName   x      Hx      |x  �  �      CHARACTER,  getServerOperatingMode  \x      �x      �x  �  �      CHARACTER,  runServerProcedure  �x      �x       y  �        HANDLE,INPUT pcServerFileName CHARACTER,INPUT phAppService HANDLE   setAppService   �x      Dy      ty  �  '      LOGICAL,INPUT pcAppService CHARACTER    setASDivision   Ty      �y      �y  �  5      LOGICAL,INPUT pcDivision CHARACTER  setASHandle �y      �y      z  �  C      LOGICAL,INPUT phASHandle HANDLE setASInfo   �y      <z      hz  � 	 O      LOGICAL,INPUT pcInfo CHARACTER  setASInitializeOnRun    Hz      �z      �z  �  Y      LOGICAL,INPUT plInitialize LOGICAL  setASUsePrompt  �z      �z      {  �  n      LOGICAL,INPUT plFlag LOGICAL    setServerFileName   �z      4{      h{  �  }      LOGICAL,INPUT pcFileName CHARACTER  setServerOperatingMode  H{      �{      �{  �  �      LOGICAL,INPUT pcServerOperatingMode CHARACTER   addLink                             �|  h|      ��                  �  �  �|              <�K                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  �|             �|  
             ��   }             �|               �� 
                  }  
         ��                            ����                            addMessage                              �}  �}      ��                  �  �  ~              `�K                    O   ����    e�          O   ����    R�          O   ����    ��            ��   \~             (~               ��   �~             P~               ��                  x~           ��                            ����                            adjustTabOrder                              t  \      ��                  �  �  �              ��O                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  �             �  
             �� 
   �             �  
             ��                  �           ��                            ����                            applyEntry                              �  Ԁ      ��                  �  �  �              ��(                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �           ��                            ����                            changeCursor                                �   �      ��                  �  �  0�              � )                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  H�           ��                            ����                            createControls                              D�  ,�      ��                  �  �  \�              0#s                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            destroyObject                               H�  0�      ��                  �  �  `�              (&s                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            displayLinks                                L�  4�      ��                  �  �  d�              's                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            editInstanceProperties                              X�  @�      ��                  �  �  p�              8;                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            exitObject                              X�  @�      ��                  �  �  p�              �;                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            hideObject                              X�  @�      ��                  �  �  p�              P;                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeObject                                `�  H�      ��                  �  �  x�              8;                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            modifyListProperty                              h�  P�      ��                  �  �  ��              H;                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  ̊             ��  
             ��   �             ��               ��   �             �               ��                  �           ��                            ����                            modifyUserLinks                             �  �      ��                  �  �  $�              ��o                    O   ����    e�          O   ����    R�          O   ����    ��            ��   p�             <�               ��   ��             d�               �� 
                 ��  
         ��                            ����                            removeAllLinks                              ��  p�      ��                  �  �  ��              ��L                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            removeLink                              ��  p�      ��                  �  �  ��              �v@                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  �             ��  
             ��   �             ��               �� 
                 �  
         ��                            ����                            repositionObject                                �  ��      ��                  �  �   �              |�                    O   ����    e�          O   ����    R�          O   ����    ��            ��   l�             8�               ��                  `�           ��                            ����                            returnFocus                             X�  @�      ��                  �  �  p�              < �                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
                 ��  
         ��                            ����                            showMessageProcedure                                ��  t�      ��                  �  �  ��              �s                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �             ��               ��                  �           ��                            ����                            toggleData                              ܓ  ē      ��                  �  �  ��              |�s                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �           ��                            ����                            viewObject                              �  �      ��                  �  �  �              0�:                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            anyMessage  �{      t�      ��  � 
 �      LOGICAL,    assignLinkProperty  ��      ��      ��  �  �      LOGICAL,INPUT pcLink CHARACTER,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER   fetchMessages   ��      8�      h�  �        CHARACTER,  getChildDataKey H�      t�      ��  �         CHARACTER,  getContainerHandle  ��      ��      �  �  0      HANDLE, getContainerHidden  Ė      �       �  �  C      LOGICAL,    getContainerSource   �      ,�      `�  �  V      HANDLE, getContainerSourceEvents    @�      h�      ��  �  i      CHARACTER,  getContainerType    ��      ��      �  �  �      CHARACTER,  getDataLinksEnabled ė      �      $�  �  �      LOGICAL,    getDataSource   �      0�      `�  �  �      HANDLE, getDataSourceEvents @�      h�      ��  �  �      CHARACTER,  getDataSourceNames  |�      ��      ܘ  �  �      CHARACTER,  getDataTarget   ��      �      �  �  �      CHARACTER,  getDataTargetEvents ��      $�      X�  �  �      CHARACTER,  getDBAware  8�      d�      ��  � 
 �      LOGICAL,    getDesignDataObject p�      ��      Й  �  	      CHARACTER,  getDynamicObject    ��      ܙ      �  �        LOGICAL,    getInstanceProperties   �      �      T�  �  .      CHARACTER,  getLogicalObjectName    4�      `�      ��  �  D      CHARACTER,  getLogicalVersion   x�      ��      ؚ  �  Y      CHARACTER,  getObjectHidden ��      �      �  �  k      LOGICAL,    getObjectInitialized    ��       �      X�  �  {      LOGICAL,    getObjectName   8�      d�      ��  �  �      CHARACTER,  getObjectPage   t�      ��      Л  �  �      INTEGER,    getObjectVersion    ��      ܛ      �  �  �      CHARACTER,  getObjectVersionNumber  �      �      T�  �  �      CHARACTER,  getParentDataKey    4�      `�      ��  �  �      CHARACTER,  getPassThroughLinks t�      ��      Ԝ  �  �      CHARACTER,  getPhysicalObjectName   ��      ��      �  �  �      CHARACTER,  getPhysicalVersion  ��      $�      X�  �        CHARACTER,  getPropertyDialog   8�      d�      ��  �  "      CHARACTER,  getQueryObject  x�      ��      ԝ  �  4      LOGICAL,    getRunAttribute ��      ��      �  �  C      CHARACTER,  getSupportedLinks   �      �      P�  �  S      CHARACTER,  getTranslatableProperties   0�      \�      ��  �  e      CHARACTER,  getUIBMode  x�      ��      О  � 
       CHARACTER,  getUserProperty ��      ܞ      �  �  �      CHARACTER,INPUT pcPropName CHARACTER    instancePropertyList    �      4�      l�  �  �      CHARACTER,INPUT pcPropList CHARACTER    linkHandles L�      ��      ��  �  �      CHARACTER,INPUT pcLink CHARACTER    linkProperty    ��      �      �  �  �      CHARACTER,INPUT pcLink CHARACTER,INPUT pcPropName CHARACTER mappedEntry ��      P�      |�  �  �      CHARACTER,INPUT pcEntry CHARACTER,INPUT pcList CHARACTER,INPUT plFirst LOGICAL,INPUT pcDelimiter CHARACTER  messageNumber   \�      �      �  �  �      CHARACTER,INPUT piMessage INTEGER   propertyType    ��      <�      l�  �  �      CHARACTER,INPUT pcPropName CHARACTER    reviewMessages  L�      ��      ġ  �  �      CHARACTER,  setChildDataKey ��      С       �  �  �      LOGICAL,INPUT cChildDataKey CHARACTER   setContainerHidden  �      (�      \�  �        LOGICAL,INPUT plHidden LOGICAL  setContainerSource  <�      |�      ��  �  !      LOGICAL,INPUT phObject HANDLE   setContainerSourceEvents    ��      Т      �  �  4      LOGICAL,INPUT pcEvents CHARACTER    setDataLinksEnabled �      0�      d�  �  M      LOGICAL,INPUT lDataLinksEnabled LOGICAL setDataSource   D�      ��      ��  �  a      LOGICAL,INPUT phObject HANDLE   setDataSourceEvents ��      ܣ      �  �  o      LOGICAL,INPUT pcEventsList CHARACTER    setDataSourceNames  �      8�      l�  �  �      LOGICAL,INPUT pcSourceNames CHARACTER   setDataTarget   L�      ��      Ĥ  �  �      LOGICAL,INPUT pcTarget CHARACTER    setDataTargetEvents ��      �      �  �  �      LOGICAL,INPUT pcEvents CHARACTER    setDBAware  ��      @�      l�  � 
 �      LOGICAL,INPUT lAware LOGICAL    setDesignDataObject L�      ��      ��  �  �      LOGICAL,INPUT pcDataObject CHARACTER    setDynamicObject    ��      �      �  �  �      LOGICAL,INPUT lTemp LOGICAL setInstanceProperties   ��      8�      p�  �  �      LOGICAL,INPUT pcPropList CHARACTER  setLogicalVersion   P�      ��      Ȧ  �  �      LOGICAL,INPUT cVersion CHARACTER    setObjectName   ��      �      �  �        LOGICAL,INPUT pcName CHARACTER  setObjectVersion    ��      <�      p�  �        LOGICAL,INPUT cObjectVersion CHARACTER  setParentDataKey    P�      ��      ̧  �  /      LOGICAL,INPUT cParentDataKey CHARACTER  setPassThroughLinks ��      ��      (�  �  @      LOGICAL,INPUT pcLinks CHARACTER setPhysicalObjectName   �      H�      ��  �  T      LOGICAL,INPUT cTemp CHARACTER   setPhysicalVersion  `�      ��      Ԩ  �  j      LOGICAL,INPUT cVersion CHARACTER    setRunAttribute ��      ��      (�  �  }      LOGICAL,INPUT cRunAttribute CHARACTER   setSupportedLinks   �      P�      ��  �  �      LOGICAL,INPUT pcLinkList CHARACTER  setTranslatableProperties   d�      ��      �  �  �      LOGICAL,INPUT pcPropList CHARACTER  setUIBMode  ĩ      �      4�  � 
 �      LOGICAL,INPUT pcMode CHARACTER  setUserProperty �      T�      ��  �  �      LOGICAL,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER  showmessage d�      Ī      �  �  �      LOGICAL,INPUT pcMessage CHARACTER   Signature   Ъ      �      @�  � 	 �      CHARACTER,INPUT pcName CHARACTER    8�    �	  ��  ��      �       4   �����                 �                      ��                  �	  *
                  d��                       �	  ��        �	  (�  ��      �       4   �����                 ��                      ��                  �	  )
                  ȧ�                       �	  8�  ��    
  Ь  L�      �       4   �����                 \�                      ��                  "
  $
                  L��                       "
  �         #
                                  d     
                    � ߱        �  $  &
  ��  ���                           $  (
  �  ���                       �                         � ߱        D�    .
  T�  Ю      �      4   �����                �                      ��                  /
  �
                   ��                       /
  d�  �  o   2
      ,                                 l�  $   3
  @�  ���                       4  @                        � ߱        ��  �   4
  T      ��  �   5
  �      ��  �   7
  <      ��  �   9
  �      Я  �   ;
  $      �  �   =
  �      ��  �   >
        �  �   ?
  P       �  �   B
  �      4�  �   D
  8      H�  �   E
  �      \�  �   G
  0      p�  �   H
  �      ��  �   I
  �      ��  �   J
  d      ��  �   K
  �      ��  �   Q
  	      ԰  �   S
  �	      �  �   Y
  �	      ��  �   [
  8
      �  �   ]
  �
      $�  �   ^
  (      8�  �   d
  �      L�  �   e
        `�  �   f
  �      t�  �   g
        ��  �   j
  |      ��  �   k
  �      ��  �   m
  ,      ı  �   n
  h      ر  �   p
  �      �  �   q
         �  �   r
  T      �  �   s
  �      (�  �   t
  �      <�  �   u
  H      P�  �   v
  �      d�  �   x
  �      x�  �   y
  �      ��  �   z
  8      ��  �   |
  t      ��  �   }
  �      Ȳ  �   ~
  �      ܲ  �   
  (          �   �
  d                      �          t�  \�      ��                    H  ��              |U?                    O   ����    e�          O   ����    R�          O   ����    ��      �     
                P                     `                         � ߱        4�  $ .  ��  ���                           O   F  ��  ��  �               ��          ��  ��    ��                                             ��                            ����                                h      �      L�     T     ��                       ��  <                     �    h  `�  ܵ      �      4   �����                �                      ��                  i  �                  tZ?                       i  p�   �  �   l        �  �   m  �      (�  �   n  �      <�  �   o  x      P�  �   p  �      d�  �   q  p      x�  �   r  �      ��  �   s  `      ��  �   t  �      ��  �   u  X      ȶ  �   v  �      ܶ  �   w  H      �  �   x  �          �   y  @      ܹ    �   �  ��      �      4   �����                ��                      ��                  �  �                  �_                       �  0�  ��  �   �        Է  �   �  �      �  �   �  �      ��  �      t      �  �     �      $�  �     \      8�  �     �      L�  �     L      `�  �     �      t�  �     4       ��  �     �       ��  �     $!      ��  �   	  �!      ĸ  �   
  "      ظ  �     �"      �  �     #       �  �     �#      �  �     $      (�  �     �$      <�  �     �$      P�  �     x%      d�  �     �%      x�  �     p&      ��  �     �&      ��  �     h'      ��  �     �'      ȹ  �     `(          �     �(      ��    �  ��  t�      D)      4   ����D)                ��                      ��                  �  G                  8b                       �  �  ��  �   �  �)      ��  �   �   *      ��  �   �  �*      Ժ  �   �  +      �  �   �  �+      ��  �   �  �+      �  �   �  l,      $�  �   �  �,      8�  �   �  -      L�  �   �  X-      `�  �   �  �-      t�  �   �  .      ��  �   �  |.      ��  �   �  �.      ��  �   �  l/      Ļ  �   �  �/      ػ  �   �  T0      �  �   �  �0       �  �   �  L1      �  �   �  �1      (�  �   �  �1      <�  �   �  p2      P�  �   �  �2      d�  �   �   3      x�  �   �  \3      ��  �   �  �3      ��  �   �  4      ��  �   �  P4      ȼ  �   �  �4      ܼ  �   �  �4      �  �   �  5      �  �   �  @5      �  �   �  |5      ,�  �   �  �5      @�  �   �  ,6      T�  �   �  h6      h�  �   �  �6      |�  �   �  �6      ��  �   �  7      ��  �   �  X7      ��  �   �  �7      ̽  �   �  8      �  �   �  |8      ��  �   �  �8      �  �   �  d9      �  �   �  �9      0�  �   �  \:      D�  �   �  �:      X�  �   �  T;      l�  �   �  �;      ��  �   �  L<      ��  �   �  �<      ��  �   �  =      ��  �   �  @=      о  �   �  |=      �  �   �  �=          �   �  ,>      �    U  �  ��      �>      4   �����>  	              ��                      ��             	     V  �                  �]                       V  $�  ��  �   X  �>      ȿ  �   Y  h?      ܿ  �   Z  �?      �  �   [  X@      �  �   a  �@      �  �   b  hA      ,�  �   c  �A      @�  �   d  PB      T�  �   e  �B      h�  �   f  HC      |�  �   g  �C      ��  �   h  8D      ��  �   i  tD      ��  �   k  �D      ��  �   l  \E      ��  �   m  �E      ��  �   n  DF      �  �   o  �F      �  �   p  ,G      0�  �   q  �G      D�  �   r  H      X�  �   s  �H      l�  �   t  I      ��  �   u  �I      ��  �   v  �I      ��  �   x  0J      ��  �   y  �J      ��  �   {  K      ��  �   |  �K      ��  �   }  L          �   ~  �L      ��    �  (�  ��      �L      4   �����L  
              ��                      ��             
     �  s                  ��5                       �  8�  ��  �   �  M      ��  �   �  �M          �      N      ��    5  �  ��      <N      4   ����<N                ��                      ��                  6  ?                  ��5                       6  �  �    8  ��  ��      TN      4   ����TN      $  9  ��  ���                       �N  @         �N              � ߱              <  8�  H�      �N      4   �����N      $  =  t�  ���                       O  @         �N              � ߱        ��  $  G  ��  ���                       <O     
                    � ߱        ��    �  �  $�      PO      4   ����PO      /   �  P�     `�                          3   ����`O            ��                      3   �����O  ��    �  ��  (�  �  �O      4   �����O                8�                      ��                  �                    4��                       �  ��  L�  �   �  �O      ��  $  �  x�  ���                       (P     
                    � ߱        ��  �   �  HP      �  $   �  ��  ���                       pP  @         \P              � ߱        ��  $  �  <�  ���                       �P                         � ߱        �Q     
                R                     \S  @        
 S              � ߱        \�  V   �  h�  ���                        hS                     �S       	       	       �S                         � ߱        ��  $  �  ��  ���                       �T     
                U                     dV  @        
 $V              � ߱        |�  V   �  ��  ���                        pV     
                �V                     <X  @        
 �W              � ߱            V   �  �  ���                                      ��                      ��                    �                  ���                         ��  PX     
                �X                     Z  @        
 �Y          �Z  @        
 DZ          �Z  @        
 �Z          D[  @        
 [              � ߱            V   &  $�  ���                        adm-clone-props �  �              �     U     `                          \  �#                     start-super-proc    �  t�  �           �     V                                  �#                     |�    �   �  �      �^      4   �����^      /   �  <�     L�                          3   �����^            l�                      3   ���� _  ��  $  �  ��  ���                        _       
       
           � ߱        ��    �  ��  l�  �  <_      4   ����<_                ��                      ��                  �  �                  Pg�                       �   �  P_       
       
       d_                     x_                         � ߱            $  �  |�  ���                             �  (�  d�      �_      4   �����_  �_       
       
           � ߱            $  �  8�  ���                       ��    �  ��  ��  �  �_      4   �����_      $     ��  ���                       �_                         � ߱            �     �_      8`     
                �`                     b  @        
 �a              � ߱        ��  V   1  (�  ���                        ��  �   d  b      d�    �  ��  ��      Pb      4   ����Pb      /   �  $�     4�                          3   ����`b            T�                      3   �����b  D�    N  ��  ��      �b      4   �����b                �                      ��                  O  R                  �k�                       O  ��      g   P  $�         =���                           ��          ��  ��      ��                  Q      ��              �k�                    O   ����    e�          O   ����    R�          O   ����    ��          /  Q  �     (�  �b                      3   �����b  X�     
   H�                      3   �����b         
   x�                      3   �����b    ��                              ��        )                  ����                                        8�              W      ��                      g                               L�  g   T  \�          =�	��                           $�          ��  ��      ��                  T  V  �              �l�                    O   ����    e�          O   ����    R�          O   ����    ��          /  U  P�     `�  �b                      3   �����b            ��                      3   ����c    ��                              ��        )                  ����                                        p�              X      ��                      g                               T�  g   X  d�          =�	��                           ,�          ��  ��      ��                  X  Z  �              `v                    O   ����    e�          O   ����    R�          O   ����    ��          /  Y  X�     h�  <c                      3   ���� c            ��                      3   ����Dc    ��                              ��        )                  ����                                        x�              Y      ��                      g                               ��    q  p�  ��      `c      4   ����`c                ��                      ��                  r  �                  $w                       r  ��  h�  /   s  (�     8�                          3   ����pc            X�                      3   �����c  d�  /  u  ��     ��  �c                      3   �����c  ��     
   ��                      3   �����c  �        ��                      3   �����c  4�        $�                      3   �����c            T�                      3   ����d  ��    }  ��  ��      8d      4   ����8d      /  �  ��     ��  �d                      3   �����d  ��     
   ��                      3   �����d  ,�        �                      3   �����d  \�        L�                      3   �����d            |�                      3   ����e        �  ��  ��      (e      4   ����(e      /  �  ��     ��  |e                      3   ����\e  $�     
   �                      3   �����e  T�        D�                      3   �����e  ��        t�                      3   �����e            ��                      3   �����e  L�     �  �e                                     �e     
                pf                     �g  @        
 �g              � ߱        ��  V     ��  ���                        �g     
                Ph                     �i  @        
 `i              � ߱        �  V   2  x�  ���                        ��    c  $�  ��      �i      4   �����i                ��                      ��                  d  i                  ��0                       d  4�  �  /   e  ��     ��                          3   �����i            �                      3   �����i      /   g  H�     X�                          3   ���� j  ��     
   x�                      3   ���� j  ��        ��                      3   ����(j  ��        ��                      3   ����<j            �                      3   ����Xj  displayObjects  ��  �                      Z      �                               .%                     \�  g     ��         =4 �                           d�          4�  �      ��                        L�              Dc                    O   ����    e�          O   ����    R�          O   ����    ��          /    ��         �j                      3   ����tj    ��                              ��        )                  ����                                        ��              [      ��                      g                               �  g     t�          =0��      }                      <�          �  ��      ��                        $�              `�b                    O   ����    e�          O   ����    R�          O   ����    ��          /    h�         �j                      3   �����j    ��                            ����                                        ��              \      x�                      g                               ��      0�  ��      �j      4   �����j                ��                      ��                                        �V                         @�  (�  /     ��     ��                          3   �����j            �                      3   �����j      /    T�     d�  (k                      3   ����k  ��     
   ��                      3   ����0k  ��        ��                      3   ����8k  ��        ��                      3   ����Lk            �                      3   ����lk  �k                     �k                     �k                     8l                         � ߱        l�  $  %  $�  ���                       �l     
                m                     Xn  @        
 n          �n  @        
 pn          o  @        
 �n              � ߱        ��  V   5  ��  ���                        0o  @         o          Xo  @         Do              � ߱            $   )  ��  ���                       disable_UI  (�  �                      ]                                    �%  
                    �  �   ���  �                ��  ��      toggleData  ,INPUT plEnabled LOGICAL    ��  ��  ��      showMessageProcedure    ,INPUT pcMessage CHARACTER,OUTPUT plAnswer LOGICAL  ��  4�  @�      returnFocus ,INPUT hTarget HANDLE   $�  h�  |�      repositionObject    ,INPUT pdRow DECIMAL,INPUT pdCol DECIMAL    X�  ��  ��      removeLink  ,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE ��  �  (�      removeAllLinks  ,   �  <�  L�      modifyUserLinks ,INPUT pcMod CHARACTER,INPUT pcLinkName CHARACTER,INPUT phObject HANDLE ,�  ��  ��      modifyListProperty  ,INPUT phCaller HANDLE,INPUT pcMode CHARACTER,INPUT pcListName CHARACTER,INPUT pcListValue CHARACTER    ��  0�  <�      hideObject  ,    �  P�  \�      exitObject  ,   @�  p�  ��      editInstanceProperties  ,   `�  ��  ��      displayLinks    ,   ��  ��  ��      createControls  ,   ��  ��  ��      changeCursor    ,INPUT pcCursor CHARACTER   ��   �  ,�      applyEntry  ,INPUT pcField CHARACTER    �  X�  h�      adjustTabOrder  ,INPUT phObject HANDLE,INPUT phAnchor HANDLE,INPUT pcPosition CHARACTER H�  ��  ��      addMessage  ,INPUT pcText CHARACTER,INPUT pcField CHARACTER,INPUT pcTable CHARACTER ��  $�  ,�      addLink ,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE �  ��  ��      unbindServer    ,INPUT pcMode CHARACTER p�  ��  ��      startServerObject   ,   ��  ��  ��      runServerObject ,INPUT phAppService HANDLE  ��  �  0�      restartServerObject ,   �  D�  \�      initializeServerObject  ,   4�  p�  ��      disconnectObject    ,   `�  ��  ��      destroyServerObject ,   ��  ��  ��      bindServer  ,   ��  ��  ��      processAction   ,INPUT pcAction CHARACTER   ��  �  ,�      enableObject    ,   �  @�  P�      disableObject   ,   0�  d�  p�      applyLayout ,   T�  ��  ��      viewPage    ,INPUT piPageNum INTEGER    t�  ��  ��      viewObject  ,   ��  ��  ��      selectPage  ,INPUT piPageNum INTEGER    ��  �  (�      removePageNTarget   ,INPUT phTarget HANDLE,INPUT piPage INTEGER �  d�  p�      passThrough ,INPUT pcLinkName CHARACTER,INPUT pcArgument CHARACTER  T�  ��  ��      notifyPage  ,INPUT pcProc CHARACTER ��  ��  ��      initPages   ,INPUT pcPageList CHARACTER ��  $�  @�      initializeVisualContainer   ,   �  T�  `�      hidePage    ,INPUT piPageNum INTEGER    D�  ��  ��      destroyObject   ,   |�  ��  ��      deletePage  ,INPUT piPageNum INTEGER    ��  ��  ��      createObjects   ,   ��  �  �      constructObject ,INPUT pcProcName CHARACTER,INPUT phParent HANDLE,INPUT pcPropList CHARACTER,OUTPUT phObject HANDLE ��  ��  ��      changePage  ,   ��  ��  ��      assignPageProperty  ,INPUT pcProp CHARACTER,INPUT pcValue CHARACTER ��  �  �      validateFields  ,INPUT-OUTPUT pcNotValidFields CHARACTER    ��  P�  \�      updateTitle ,   @�  p�  ��      updateRecord    ,   `�  ��  ��      updateMode  ,INPUT pcMode CHARACTER ��  ��  ��      showDataMessagesProcedure   ,OUTPUT pcReturn CHARACTER  ��  �  �      resetRecord ,    �  0�  @�      queryPosition   ,INPUT pcState CHARACTER     �  l�  ��      okToContinueProcedure   ,INPUT pcAction CHARACTER,OUTPUT plAnswer LOGICAL   \�  ��  ��      deleteRecord    ,   ��  ��  ��      dataAvailable   ,INPUT pcRelative CHARACTER ��  (�  4�      confirmExit ,INPUT-OUTPUT plCancel LOGICAL  �  d�  t�      confirmDelete   ,INPUT-OUTPUT plAnswer LOGICAL  T�  ��  ��      confirmContinue ,INPUT-OUTPUT plCancel LOGICAL  ��  ��  ��      collectChanges  ,INPUT-OUTPUT pcChanges CHARACTER,INPUT-OUTPUT pcInfo CHARACTER ��  D�  P�      viewRecord  ,   4�  d�  t�      valueChanged    ,   T�  ��  ��      updateState ,INPUT pcState CHARACTER    x�  ��  ��      toolbar ,INPUT pcValue CHARACTER    ��  ��  �      initializeObject    ,   ��  �  ,�      enableFields    ,   �  @�  P�      displayFields   ,INPUT pcColValues CHARACTER    0�  ��  ��      disableFields   ,INPUT pcFieldType CHARACTER    p�  ��  ��      copyRecord  ,   ��  ��  ��      cancelRecord    ,   ��  �  �      addRecord   ,        � 
"     
 %     adecomm/as-utils.w  
"   
   �    }        �
"     
    �     }        �� �  F   %               � 
"    
 � %              � �  �         `      $              
�    � �   �      
�             �G                      
�            � �   � 
"    
 �
�H T   %              �     }        �GG %              � 
"   
   P �L 
�H T   %              �     }        �GG %              
"   
   �            7%               
"   
 A�           H    1� �  
 A�    � %               o%   o           �     A
"   
 A�           �    1�    A�    � %               o%   o           �    A
"   
 A�           0    1� "  
 A�    � %               o%   o           � -   A
"   
 A�           �    1� =   A�    � %               o%   o           � K   A
"   
 A�               1� Q   A�    � %               o%   o           � `   A
"   
 A�           �    1� w   A� �   � %               o%   o           %               
"   
 � �              1� �   � � �     
"   
 A�           D    1� �   A�    � %               o%   o           � �  � A
"   
 A�           �    1� t   A�    � %               o%   o           � �  N A
"   
 A�           ,    1� �   A� �   � %               o%   o           %               
"   
 A�           �    1� �   A� �   � %               o%   o           %               
"   
 A�           $    1� �   A� �   � %               o%   o           %              
"   
 � �          �    1�    � � �     
"   
 A�           �    1�   
 A� �   � %               o%   o           %               
"   
 A�           X    1�    A�    � %               o%   o           �     A
"   
 � �          �    1� #   � � �     
"   
 A�           	    1� 3   A�    � %               o%   o           � I  t A
"   
 � �          |	    1� �  
 � � �     
"   
 A�           �	    1� �   A�    � %               o%   o           � �  � A
"   
 A�           ,
    1� g   A�    � %               o%   o           �     A
"   
 A�           �
    1� ~  
 A� �   � %               o%   o           %               
"   
 ?�               1� �   ?� �   � %               o%   o           %               
"   
 ��           �    1� �   ��    � %               o%   o           �     ?
"   
 ��               1� �   ��    � %               o%   o           o%   o           
"   
 ��           �    1� �  
 ��    � %               o%   o           �     �
"   
 ��           �    1� �   �� �  	 � %               o%   o           � �  / �
"   
 � �          p    1�    � � �  	   
"   
 ��           �    1�    �� �  	 � o%   o           o%   o           �     �
"   
 � �               1� 1   � � �  	   
"   
 ;�           \    1� @   ;� �  	 � o%   o           o%   o           �     ;
"   
 � �          �    1� P   � � �     
"   
 � �              1� ^   � � �  	   
"   
 � �          H    1� k   � � �  	   
"   
 � �          �    1� x   � � �  	   
"   
 "�           �    1� �   "� �   � o%   o           o%   o           %              
"   
 � �          <    1� �   � � �  	   
"   
 � �          x    1� �  
 � � �     
"   
 � �          �    1� �   � � �  	   
"   
 � �          �    1� �   � � �  	   
"   
 � �          ,    1� �   � � �  	   
"   
 � �          h    1� �   � � �  	   
"   
 � �          �    1� �  	 � � �  	   
"   
 � �          �    1�    � � �  	   
"   
 � �              1�    � � �  	   
"   
 ��           X    1� 2   ��    � %               o%   o           o%   o           
�H T   %              �     }        �GG %              
"   
   
"   
 p
"   
   
"   
 �(�  L ( l       �             �� >   � P   �        ,    �@    
� @  , 
�       8    �� G     p�               �L
�    %              � 8      D    � $         � N          
�    � h     
"   
 �� @  , 
�       T    �� "  
 �p�               �L"      P �L 
�H T   %              �     }        �GG %              
"   
 ��                1� k  
 ��    � %               o%   o           �     �
"   
 ��           t    1� v  
 ��    � %               o%   o           o%   o           
"   
 p�           �    1� �   p� �   � %               o%   o           o%   o           
"   
 ��           l    1� �   �� �   � %               o%   o           %               
"   
 ?�           �    1� �   ?� �   � %               o%   o           %               
"   
 �           d    1� �   �    � %               o%   o           �     ?
"   
 "�           �    1� �   "� �   � %               o%   o           %              
"   
 "�           T    1� �   "� �   � %               o%   o           o%   o           
"   
 ��           �    1� �   ��    � %               o%   o           o%   o           
"   
 p�           L    1� �  	 p�    � %               o%   o           �     �
"   
 p�           �    1� �   p�    � %               o%   o           o%   o           
"   
 ��           <    1� �   ��    � %               o%   o           o%   o           
"   
 ?�           �    1�    ?� �   � %               o%   o           %               
"   
 ?�           4    1�    ?� �   � %               o%   o           o%   o           P �L 
�H T   %              �     }        �GG %              
"   
 p�               1� "   p� �  	 � %               o%   o           �     p
"   
 ��           x    1� /   �� �  	 � %               o%   o           �     p
"   
 ��           �    1� =   �� �   � %               o%   o           %               
"   
 �           h    1� K   � �  	 � %               o%   o           �     �
"   
 ��           �    1� Z   �� �  	 � %               o%   o           �     
"   
 "�           P    1� h   "� �   � %               o%   o           %               
"   
 ��           �    1� v   �� �  	 � %               o%   o           �     "
"   
 p�           @    1� �   p� �  	 � %               o%   o           �     �
"   
 p�           �    1� �   p� �  	 � %               o%   o           �     p
"   
 p�           (     1� �   p� �  	 � %               o%   o           o%   o           
"   
 ��           �     1� �   �� �  	 � %               o%   o           �     �
"   
 �           !    1� �   � �  	 � %               o%   o           �     �
"   
 ��           �!    1� �  	 �� �   � %               o%   o           %               
"   
 "�           "    1� �   "� �   � %               o%   o           %               
"   
 "�           �"    1� �   "� �   � %               o%   o           o%   o           
"   
 ��            #    1� �   �� �   � %               o%   o           o%   o           
"   
 p�           |#    1�    p� �   � %               o%   o           %               
"   
 ��           �#    1�    �� �   � %               o%   o           %               
"   
 ��           t$    1�     �� �   � %               o%   o           %               
"   
 �           �$    1� 5   � A   � %               o%   o           %       
       
"   
 �           l%    1� I   � A   � %               o%   o           o%   o           
"   
 ?�           �%    1� U   ?� A   � %               o%   o           %              
"   
 ?�           d&    1� a   ?� A   � %               o%   o           o%   o           
"   
 ��           �&    1� m   �� A   � %               o%   o           %              
"   
 ��           \'    1� z   �� A   � %               o%   o           o%   o           
"   
 ��           �'    1� �   �� A   � %               o%   o           %              
"   
 ��           T(    1� �   �� A   � %               o%   o           o%   o           
"   
 �           �(    1� �   � �  	 � %               o%   o           �     pP �L 
�H T   %              �     }        �GG %              
"   
 ��           �)    1� �   �� �   � %               o%   o           %               
"   
 ��           *    1� �   �� �   � %               o%   o           o%   o           
"   
 "�           �*    1� �   "�    � %               o%   o           �     ?
"   
 ��           +    1� �   ��    � %               o%   o           � �  - "
"   
 ��           x+    1�    ��    � %               o%   o           �     �
"   
 ��           �+    1� ,   ��    � %               o%   o           � I   �
"   
 � �          `,    1� g   � � �     
"   
 p�           �,    1� x   p�    � %               o%   o           �     p
"   
 � �          -    1� �  
 � � �     
"   
 � �          L-    1� �   � � �     
"   
 "�           �-    1� �   "� �  	 � %               o%   o           �     ?
"   
 ��           �-    1� �   ��    � %               o%   o           �     "
"   
 ��           p.    1� �   �� �   � %               o%   o           o%   o           
"   
 ��           �.    1� �   ��    � %               o%   o           � �  ! �
"   
 p�           `/    1� �   p�    � %               o%   o           �     �
"   
 �           �/    1�    �    � %               o%   o           �    p
"   
 �           H0    1� '  	 � �   � %               o%   o           o%   o           
"   
 ?�           �0    1� 1   ?� �   � %               o%   o           %               
"   
 � �          @1    1� =   � � �     
"   
 ��           |1    1� K   ��    � %               o%   o           � _   �
"   
 ��           �1    1� n   �� �  	 � %               o%   o           �     �
"   
 ��           d2    1� {   �� �  	 � %               o%   o           �     �
"   
 � �          �2    1� �   � � �     
"   
 � �          3    1� �   � � �  	   
"   
 �           P3    1� �   � �   � o%   o           o%   o           %               
"   
 � �          �3    1� �   � � �     
"   
 � �          4    1� �   � � �  	   
"   
 � �          D4    1� �   � � �  	   
"   
 � �          �4    1� �   � � �  	   
"   
 � �          �4    1�    � � �  	   
"   
 � �          �4    1� !   � � �  	   
"   
 � �          45    1� 2   � � �     
"   
 ��           p5    1� C   ��    � %               o%   o           � Z  4 �
"   
 � �          �5    1� �   � � �     
"   
 � �           6    1� �   � � �     
"   
 � �          \6    1� �   � � �     
"   
 � �          �6    1� �   � � �  	   
"   
 � �          �6    1� �   � � �  	   
"   
 � �          7    1� �   � � �  	   
"   
 � �          L7    1� �   � � �     
"   
 "�           �7    1� �   "� �  	 � %               o%   o           �     �
"   
 p�           �7    1�    p� �  	 � %               o%   o           �     "
"   
 ��           p8    1�    �� �  	 � %               o%   o           �     p
"   
 ��           �8    1� -   �� �  	 � %               o%   o           �     �
"   
 p�           X9    1� B   p� �   � %               o%   o           %               
"   
 p�           �9    1� P   p� �   � %               o%   o           o%   o           
"   
 ��           P:    1� b   �� �   � %               o%   o           %               
"   
 ��           �:    1� r   �� �   � %               o%   o           %               
"   
 ��           H;    1� ~   �� �   � %               o%   o           o%   o           
"   
 p�           �;    1� �   p� �   � %               o%   o           %               
"   
 � �          @<    1� �   � � �  	   
"   
 p�           |<    1� �   p� �   � %               o%   o           %              
"   
 � �          �<    1� �   � � �  	   
"   
 � �          4=    1� �   � � �  	   
"   
 � �          p=    1� �  
 � � �  	   
"   
 ��           �=    1� �   �� �  	 � %               o%   o           � B   ?
"   
 ��            >    1� �   �� �  	 � %               o%   o           �     �P �L 
�H T   %              �     }        �GG %              
"   
 p�           �>    1�     p�    � %               o%   o           �     p
"   
 ��           \?    1�     �� �   � %               o%   o           %               
"   
 ��           �?    1� *    ��    � %               o%   o           �     �
"   
 ?�     ,      L@    1� :    ?�    � %               o%   o           �   � �     � J    ��    	 ?
"   
 ��           �@    1� L    �� �   � %               o%   o           o%   o           
"   
 ?�           \A    1� U    ?�    � %               o%   o           �     "
"   
 p�           �A    1� c    p�    � %               o%   o           �     ?
"   
 p�           DB    1� r    p� �  	 � %               o%   o           o%   o           
"   
 ��           �B    1� �    ��    � %               o%   o           o%   o           
"   
 �           <C    1� �    �    � %               o%   o           �     p
"   
 ��           �C    1� �    �� �   � %               o%   o           %               
"   
 � �          ,D    1� �    � � �     
"   
 "�           hD    1� �    "�    � %               o%   o           � �   ~ �
"   
 p�           �D    1� ]!   p�    � %               o%   o           �     "
"   
 ��           PE    1� o!   ��    � %               o%   o           � �!   p
"   
 ��           �E    1� �!   �� �  	 � %               o%   o           � �!   �
"   
 p�           8F    1� �!   p� �  	 � %               o%   o           � �!   �
"   
 ��           �F    1� �!  	 ��    � %               o%   o           � �!   p
"   
 ��            G    1� �!  
 �� �  	 � %               o%   o           � �!   �
"   
 ��           �G    1� �!   �� �   � %               o%   o           o%   o           
"   
 "�           H    1� "   "�    � %               o%   o           � "   �
"   
 p�           �H    1�  "   p�    � %               o%   o           �     "
"   
 p�           �H    1� )"  
 p� �   � %               o%   o           o%   o           
"   
 � �          tI    1� 4"   � � �     
"   
 p�           �I    1� B"   p�    � %               o%   o           � V"  ] �
"   
 ?�           $J    1� �"   ?�    � %               o%   o           �     p
"   
 ��           �J    1� �"   ��    � %               o%   o           � �"   ?
"   
 ��           K    1� �"   �� �   � %               o%   o           %               
"   
 p�           �K    1� �   p�    � %               o%   o           �     �
"   
 p�           �K    1� �"   p�    � %               o%   o           o%   o           
"   
 � �          xL    1� �"   � � �  	   P �L 
�H T   %              �     }        �GG %              
"   
 ��           M    1� 	#   �� �   � %               o%   o           %               
"   
 p�           �M    1� #  	 p� �   � %               o%   o           %               
"   
 � �           N    1� &#   � �          
�    
�        �     }         �     }        �     }             �     }        �%                  �     }         �     }        �     }             �     }        �%              
�             �G "    � %     start-super-proc � %     adm2/smart.p =�P �L 
�H T   %              �     }        �GG %              
"   
   �       �O    6� >     
"   
   
�        P    8
"   
   �        <P    ��     }        �G 4              
"   
 ߱G %              G %              %� � �   EnabledObjFldsToDisable,ModifyFields,DataSourceNames,UpdateTargetNames,LogicalObjectName,LogicalObjectName,PhysicalObjectName,DynamicObject,RunAttribute,HideOnInit,DisableOnInit,ObjectLayout  
�H T   %              �     }        �GG %              
"   
 �
"   
 � 
"   
 �
"   
   (�  L ( l       �        �Q    �� >   � P   �        �Q    �@    
� @  , 
�       �Q    �� G   �p�               �L
�    %              � 8       R    � $         � N          
�    � h   �
"   
 �p� @  , 
�       S    �� �   �p�               �L"    , �   � a#   �� c#   � �     }        �A      |    "      � a#   �%              (<   \ (    |    �     }        �A� e#   �A"  	  �    "    �"  	  �  < "    �"  	  �(    |    �     }        �A� e#   �A"  	  �
�H T   %              �     }        �GG %              
"   
 �
"   
 � 
"   
 �
"   
   (�  L ( l       �        �T    �� >   � P   �        �T    �@    
� @  , 
�       �T    �� G   �p�               �L
�    %              � 8      U    � $         � N          
�    � h   �
"   
 �p� @  , 
�       V    �� �  
 �p�               �L"    , 
�H T   %              �     }        �GG %              
"   
 �
"   
 � 
"   
 �
"   
 ;(�  L ( l       �        �V    �� >   � P   �        �V    �@    
� @  , 
�       �V    �� G   �p�               �L
�    %              � 8      �V    � $         � N   �     
�    � h   � 
"   
 �p� @  , 
�       �W    �� �   �p�               �L
�             �G
�H T   %              �     }        �GG %              
"   
   
"   
 t
"   
   
"   
   (�  L ( l       �        �X    �� >   � P   �        �X    �@    
� @  , 
�       �X    �� G     p�               �L
�    %              � 8      �X    � $         � N          
�    � h     
"   
 �p� @  , 
�       �Y    �� "  
 �p�               �L%     SmartDataViewer 
"   
   p� @  , 
�       8Z    �� =     p�               �L%      FRAME   
"   
  p� @  , 
�       �Z    �� @    p�               �L%               
"   
  p� @  , 
�       �Z    ��     p�               �L(        �       �       �       �     }        �A
�H T   %              �     }        �GG %              
"   
 � (   � 
"   
 �    �        �[    �� >   �
"   
   � 8      $\    � $         � N          
�    � h   �
"   
   �        |\    �
"   
   �       �\    /
"   
   
"   
   �       �\    6� >     
"   
   
�        �\    8
"   
   �        ]    �
"   
   �       4]    �
"   
   p�    � �#   �
�    �     }        �G 4              
"   
 ߱G %              G %              
�     }        �
"   
    (   � 
"   
 �    �        �]    �A"    �A
"   
   
�        D^    �@ � 
"   
 �"      �       }        �
"   
 � %              %                "    � %     start-super-proc � %     adm2/appserver.p ���    � $     
�    �     }        �%               %      Server  - �     }        �    "  
  p�     � %                   "    p�     � %      NONE    p�,  8         $     "    ;        � )$   �
�    
�H T   %              �     }        �GG %              
"   
 �
"   
 � 
"   
 �
"   
   (�  L ( l       �        �`    �� >   � P   �        �`    �@    
� @  , 
�       �`    �� G   �p�               �L
�    %              � 8      �`    � $         � N          
�    � h   �
"   
 �p� @  , 
�       �a    �� �   �p�               �L"    , p�,  8         $     "  
  ;        � 7$   �
�     "    � %     start-super-proc � %     adm2/visual.p �� 
"    
 � %     contextHelp 
"    
   
�    
�    %     processAction   
�    %     CTRL-PAGE-UP =�%     processAction   
�    %     CTRL-PAGE-DOWN  "    � %     start-super-proc � %     adm2/containr.p %     modifyListProperty  
�    
�    %      Add     %      ContainerSourceEvents �%      initializeDataObjects �0 0   A    �    � �$   �
�    � �$   � A    �    � �$     
�    � �$   � %     modifyListProperty  
�    
�    %      Add     %      ContainerSourceEvents �%     buildDataRequest ent0 A    �    � �$   � 
�    � �$   �%     modifyListProperty  
�    
�    %      Add     %     SupportedLinks %      ContainerToolbar-Target %               
�H T   %              �     }        �GG %              
"   
 �
"   
 � 
"   
 �
"   
 �(�  L ( l       �        @f    �� >   � P   �        Lf    �@    
� @  , 
�       Xf    �� G   �p�               �L
�    %              � 8      df    � $         � N   �     
�    � h   � 
"   
 �p� @  , 
�       tg    �� �   �p�               �L
�             �G
�H T   %              �     }        �GG %              
"   
 �
"   
 � 
"   
 �
"   
 �(�  L ( l       �         h    �� >   � P   �        ,h    �@    
� @  , 
�       8h    �� G   �p�               �L
�    %              � 8      Dh    � $         � N   �     
�    � h   �
"   
 �p� @  , 
�       Ti    �� B   �p�               �L%               "    � %     start-super-proc � %     adm2/datavis.p %     modifyListProperty  
�    %      ADD     %     SupportedLinks %     Toolbar-Target %     valueChanged    
�    %     valueChanged    
�     "    � %     start-super-proc � %     adm2/viewer.p �%     modifyListProperty  
�    
�    %      Add     %     DataSourceEvents ��%     buildDataRequest ���   � �   p� J      � s%  j ��   � �     � J    �� s%  j p�@    �    � �   �� �%   �     � �   �"    �� �   � �@    �    � �     � �%         � �   �"    � � �     
�H T   %              �     }        �GG %              
"   
 � 
"   
 �
"   
 � 
"   
 � (�  L ( l       �        �l    �� >   � P   �        �l    �@    
� @  , 
�       �l    �� G   � p�               �L
�    %              � 8      �l    � $         � N   �      
�    � h     
"   
 �p� @  , 
�       n    �� *    �p�               �L"    , 
"   
   p� @  , 
�       dn    �� U      p�               �L"    , 
"   
  p� @  , 
�       �n    �� )"  
  p�               �L%               �             I%               �             �%              �     }        �
�                    �           �   l       ��                   B  �               $��                    O   ����    e�          O   ����    R�          O   ����    ��        $  -  �   ���                       �[     
                    � ߱              .  (  �      �[      4   �����[                �                      ��                  /  A                  �KB                       /  8  �  �  0  0\            2  �  `      �\      4   �����\                p                      ��                  3  @                  @LB                       3  �  �  o   4      ,                                 �  �   5  �\      �  �   6  �\      $  $  7  �  ���                        ]     
                    � ߱        8  �   8   ]      L  �   9  @]      `  �   <  `]          $   ?  �  ���                       �]  @         |]              � ߱                     T          ,  @   T �            
             
             
             
                 $   4   D          $   4   D   ����        ��                            ����                                            �           �   l       ��                 f  �  �               �MB                    O   ����    e�          O   ����    R�          O   ����    ��      �#                      �          �  $  x    ���                       �]     
                    � ߱                  �  �                      ��                   y  {                  NB                     y  4      4   ����^      $  z  �  ���                       P^     
                    � ߱        �    |  4  D      d^      4   ����d^      /  }  p                               3   ����x^  �  �   �  �^          O   �  ��  ��  �^                               , �                          
                               �      ��                            ����                                                        �   l       ��                  �  �  �               ��0                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                                            �           �   l       ��                  O  [  �               ��8                    O   ����    e�          O   ����    R�          O   ����    ��      �      Y  �� �                       Z  �         lo      4   ����lo      �   Z  �o    ��                              ��        )                  ����                               Y   d d     |   ���  �  � �                                               )      �                                                                  d     D                                                                 P   �d .d                                                           �%  G   
 X �d ;d                                                              0  #   g     6       P   �� �d                                                           &  G   
 X �� ;d                                                       "      0  #   g     6       P   �,�d                                                           &  G   
 X �,S
d                                                       3      O     g     U       P   ��d                                                           &  G   
 X ��_d                                                       �      j     g     p       P   ��wd                                                           &  G   
 X ���d                                             
          �      �  
   g     �       P   �X�d                                                           %&  G   
 X �X_d                                                       I      j     g     �        D                                                                    TXS appSrvUtils RowObject Address Address2 Balance City Comments Contact Country CreditLimit CustNum Discount EmailAddress Fax Name Phone PostalCode SalesRep State Terms ASSIGNFOCUSEDWIDGET ASSIGNWIDGETVALUE ASSIGNWIDGETVALUELIST BLANKWIDGET CLEARWIDGET DISABLERADIOBUTTON DISABLEWIDGET ENABLERADIOBUTTON ENABLEWIDGET FORMATTEDWIDGETVALUE FORMATTEDWIDGETVALUELIST HIDEWIDGET HIGHLIGHTWIDGET RESETWIDGETVALUE TOGGLEWIDGET VIEWWIDGET WIDGETHANDLE WIDGETLONGCHARVALUE WIDGETISBLANK WIDGETISFOCUSED WIDGETISMODIFIED WIDGETISTRUE WIDGETVALUE WIDGETVALUELIST F-Main x(35) Please enter an address. x(25) Please enter a city. x(20) Please enter standard state abbreviation. x(10) Please enter the appropriate Postal Code. Please enter a country. C:\newsie\on_in_co\aplic\vcustaddress.w should only be RUN PERSISTENT. GETTARGETPROCEDURE GETOBJECTTYPE GETSHOWPOPUP SETSHOWPOPUP GETCREATEHANDLES GETDATAMODIFIED GETDISPLAYEDFIELDS GETDISPLAYEDTABLES GETENABLEDFIELDS GETENABLEDHANDLES GETFIELDHANDLES GETFIELDSENABLED GETGROUPASSIGNSOURCE GETGROUPASSIGNSOURCEEVENTS GETGROUPASSIGNTARGET GETGROUPASSIGNTARGETEVENTS GETNEWRECORD GETOBJECTPARENT GETRECORDSTATE GETROWIDENT GETTABLEIOSOURCE GETTABLEIOSOURCEEVENTS GETUPDATETARGET GETUPDATETARGETNAMES GETWINDOWTITLEFIELD OKTOCONTINUE SETCONTAINERMODE SETDATAMODIFIED SETDISPLAYEDFIELDS SETENABLEDFIELDS SETGROUPASSIGNSOURCE SETGROUPASSIGNSOURCEEVENTS SETGROUPASSIGNTARGET SETGROUPASSIGNTARGETEVENTS SETLOGICALOBJECTNAME SETOBJECTPARENT SETSAVESOURCE SETTABLEIOSOURCE SETTABLEIOSOURCEEVENTS SETUPDATETARGET SETUPDATETARGETNAMES SETWINDOWTITLEFIELD SHOWDATAMESSAGES DISABLEPAGESINFOLDER ENABLEPAGESINFOLDER GETCALLERPROCEDURE GETCALLERWINDOW GETCONTAINERMODE GETCONTAINERTARGET GETCONTAINERTARGETEVENTS GETCURRENTPAGE GETDISABLEDADDMODETABS GETDYNAMICSDOPROCEDURE GETFILTERSOURCE GETMULTIINSTANCEACTIVATED GETMULTIINSTANCESUPPORTED GETNAVIGATIONSOURCE GETNAVIGATIONSOURCEEVENTS GETNAVIGATIONTARGET GETOUTMESSAGETARGET GETPAGENTARGET GETPAGESOURCE GETPRIMARYSDOTARGET GETREENABLEDATALINKS GETRUNDOOPTIONS GETRUNMULTIPLE GETSAVEDCONTAINERMODE GETSDOFOREIGNFIELDS GETTOPONLY GETUPDATESOURCE GETWAITFOROBJECT GETWINDOWTITLEVIEWER GETSTATUSAREA PAGENTARGETS SETCALLEROBJECT SETCALLERPROCEDURE SETCALLERWINDOW SETCONTAINERTARGET SETCURRENTPAGE SETDISABLEDADDMODETABS SETDYNAMICSDOPROCEDURE SETFILTERSOURCE SETINMESSAGETARGET SETMULTIINSTANCEACTIVATED SETMULTIINSTANCESUPPORTED SETNAVIGATIONSOURCE SETNAVIGATIONSOURCEEVENTS SETNAVIGATIONTARGET SETOUTMESSAGETARGET SETPAGENTARGET SETPAGESOURCE SETPRIMARYSDOTARGET SETREENABLEDATALINKS SETROUTERTARGET SETRUNDOOPTIONS SETRUNMULTIPLE SETSAVEDCONTAINERMODE SETSDOFOREIGNFIELDS SETTOPONLY SETUPDATESOURCE SETWAITFOROBJECT SETWINDOWTITLEVIEWER SETSTATUSAREA GETALLFIELDHANDLES GETALLFIELDNAMES GETCOL GETDEFAULTLAYOUT GETDISABLEONINIT GETENABLEDOBJFLDS GETENABLEDOBJHDLS GETHEIGHT GETHIDEONINIT GETLAYOUTOPTIONS GETLAYOUTVARIABLE GETOBJECTENABLED GETOBJECTLAYOUT GETROW GETWIDTH GETRESIZEHORIZONTAL GETRESIZEVERTICAL SETALLFIELDHANDLES SETALLFIELDNAMES SETDEFAULTLAYOUT SETDISABLEONINIT SETHIDEONINIT SETLAYOUTOPTIONS SETOBJECTLAYOUT SETRESIZEHORIZONTAL SETRESIZEVERTICAL GETOBJECTTRANSLATED GETOBJECTSECURED CREATEUIEVENTS GETAPPSERVICE GETASBOUND GETASDIVISION GETASHANDLE GETASHASSTARTED GETASINFO GETASINITIALIZEONRUN GETASUSEPROMPT GETSERVERFILENAME GETSERVEROPERATINGMODE RUNSERVERPROCEDURE SETAPPSERVICE SETASDIVISION SETASHANDLE SETASINFO SETASINITIALIZEONRUN SETASUSEPROMPT SETSERVERFILENAME SETSERVEROPERATINGMODE gshAstraAppserver gshSessionManager gshRIManager gshSecurityManager gshProfileManager gshRepositoryManager gshTranslationManager gshWebManager gscSessionId gsdSessionObj gshFinManager gshGenManager gshAgnManager gsdTempUniqueID gsdUserObj gsdRenderTypeObj gsdSessionScopeObj ghProp ghADMProps ghADMPropsBuf glADMLoadFromRepos glADMOk ANYMESSAGE ASSIGNLINKPROPERTY FETCHMESSAGES GETCHILDDATAKEY GETCONTAINERHANDLE GETCONTAINERHIDDEN GETCONTAINERSOURCE GETCONTAINERSOURCEEVENTS GETCONTAINERTYPE GETDATALINKSENABLED GETDATASOURCE GETDATASOURCEEVENTS GETDATASOURCENAMES GETDATATARGET GETDATATARGETEVENTS GETDBAWARE GETDESIGNDATAOBJECT GETDYNAMICOBJECT GETINSTANCEPROPERTIES GETLOGICALOBJECTNAME GETLOGICALVERSION GETOBJECTHIDDEN GETOBJECTINITIALIZED GETOBJECTNAME GETOBJECTPAGE GETOBJECTVERSION GETOBJECTVERSIONNUMBER GETPARENTDATAKEY GETPASSTHROUGHLINKS GETPHYSICALOBJECTNAME GETPHYSICALVERSION GETPROPERTYDIALOG GETQUERYOBJECT GETRUNATTRIBUTE GETSUPPORTEDLINKS GETTRANSLATABLEPROPERTIES GETUIBMODE GETUSERPROPERTY INSTANCEPROPERTYLIST LINKHANDLES LINKPROPERTY MAPPEDENTRY MESSAGENUMBER PROPERTYTYPE REVIEWMESSAGES SETCHILDDATAKEY SETCONTAINERHIDDEN SETCONTAINERSOURCE SETCONTAINERSOURCEEVENTS SETDATALINKSENABLED SETDATASOURCE SETDATASOURCEEVENTS SETDATASOURCENAMES SETDATATARGET SETDATATARGETEVENTS SETDBAWARE SETDESIGNDATAOBJECT SETDYNAMICOBJECT SETINSTANCEPROPERTIES SETLOGICALVERSION SETOBJECTNAME SETOBJECTVERSION SETPARENTDATAKEY SETPASSTHROUGHLINKS SETPHYSICALOBJECTNAME SETPHYSICALVERSION SETRUNATTRIBUTE SETSUPPORTEDLINKS SETTRANSLATABLEPROPERTIES SETUIBMODE SETUSERPROPERTY SHOWMESSAGE SIGNATURE , prepareInstance ObjectName CHAR  ObjectVersion ADM2.2 ObjectType SmartDataViewer ContainerType FRAME PropertyDialog adm2/support/viewerd.w QueryObject LOGICAL ContainerHandle HANDLE InstanceProperties EnabledObjFldsToDisable,ModifyFields,DataSourceNames,UpdateTargetNames,LogicalObjectName,LogicalObjectName,PhysicalObjectName,DynamicObject,RunAttribute,HideOnInit,DisableOnInit,ObjectLayout SupportedLinks Data-Target,Update-Source,TableIO-Target,GroupAssign-Source,GroupAssign-Target ContainerHidden ObjectInitialized ObjectHidden ObjectsCreated HideOnInit UIBMode ContainerSource ContainerSourceEvents initializeObject,hideObject,viewObject,destroyObject,enableObject,confirmExit,confirmCancel,confirmOk,isUpdateActive DataSource DataSourceEvents dataAvailable,queryPosition,updateState,deleteComplete,fetchDataSet,confirmContinue,confirmCommit,confirmUndo,assignMaxGuess,isUpdatePending TranslatableProperties ObjectPage INT DBAware DesignDataObject DataSourceNames DataTarget DataTargetEvents CHARACTER updateState,rowObjectState,fetchBatch,LinkState LogicalObjectName PhysicalObjectName LogicalVersion PhysicalVersion DynamicObject RunAttribute ChildDataKey ParentDataKey DataLinksEnabled InactiveLinks InstanceId DECIMAL SuperProcedure SuperProcedureMode SuperProcedureHandle LayoutPosition ClassName RenderingProcedure ThinRenderingProcedure Label cType ADMProps Target WHERE Target = WIDGET-H(" ") AppService ASDivision ASHandle ASHasConnected ASHasStarted ASInfo ASInitializeOnRun ASUsePrompt BindSignature BindScope ServerOperatingMode ServerFileName ServerFirstCall NeedContext ObjectLayout LayoutOptions ObjectEnabled LayoutVariable DefaultLayout DisableOnInit EnabledObjFlds EnabledObjHdls FieldSecurity SecuredTokens AllFieldHandles AllFieldNames MinHeight MinWidth ResizeHorizontal ResizeVertical ObjectSecured ObjectTranslated PopupButtonsInFields ColorInfoBG INTEGER ColorInfoFG ColorWarnBG ColorWarnFG ColorErrorBG ColorErrorFG BGColor FGColor FieldPopupMapping CurrentPage PendingPage ContainerTarget ContainerTargetEvents exitObject,okObject,cancelObject,updateActive ContainerToolbarSource ContainerToolbarSourceEvents toolbar,okObject,cancelObject OutMessageTarget PageNTarget PageSource FilterSource UpdateSource UpdateTarget CommitSource CommitSourceEvents commitTransaction,undoTransaction CommitTarget CommitTargetEvents rowObjectState StartPage RunMultiple WaitForObject DynamicSDOProcedure adm2/dyndata.w RunDOOptions InitialPageList WindowFrameHandle Page0LayoutManager MultiInstanceSupported MultiInstanceActivated ContainerMode SavedContainerMode SdoForeignFields NavigationSource NavigationTarget PrimarySdoTarget NavigationSourceEvents fetchFirst,fetchNext,fetchPrev,fetchLast,startFilter CallerWindow CallerProcedure CallerObject DisabledAddModeTabs ReEnableDataLinks WindowTitleViewer UpdateActive InstanceNames ClientNames ContainedDataObjects ContainedAppServices DataContainer HasDbAwareObjects HasDynamicProxy HideOnClose HideChildContainersOnClose HasObjectMenu RequiredPages RemoveMenuOnHide ProcessList PageLayoutInfo PageTokens DataContainerName WidgetIDFileName CreateHandles DataModified DisplayedFields DisplayedTables   Editable EnabledFields EnabledHandles EnabledObjFldsToDisable EnabledWhenNew FieldHandles FieldsEnabled GroupAssignSource GroupAssignSourceEvents addRecord,copyRecord,updateRecord,resetRecord,undoRecord,cancelRecord,enableFields,disableFields,collectChanges,validateFields GroupAssignTarget GroupAssignTargetEvents updateState,LinkState InternalDisplayFromSource (Large) ModifyFields (All) NewRecord No ObjectMode View PrintPreviewActive RecordState NoRecordAvailable RowIdent SaveSource TableIOSource TableIOSourceEvents addRecord,updateRecord,copyRecord,deleteRecord,resetRecord,undoChange,cancelRecord,updateMode ToolbarSource ToolbarSourceEvents toolbar UndoNew UpdateTargetNames WindowTitleField KeepChildPositions ShowPopup FieldWidgetIDs ghContainer adm2/smart.p cObjectName iStart / \ . hReposBuffer hPropTable hBuffer hTable deleteProperties ADM-CLONE-PROPS pcProcName hProc START-SUPER-PROC cAppService cASDivision cServerOperatingMode adm2/appserver.p getAppService Server NONE setASDivision setAppService cFields adm2/visual.p CTRL-PAGE-UP CTRL-PAGE-DOWN adm2/containr.p Add initializeDataObjects getSupportedLinks data-target data-source buildDataRequest containertoolbar-target ContainerToolbar-Target adm2/datavis.p ADD Toolbar-Target DISPLAYOBJECTS cViewCols cEnabled iCol iEntries cEntry adm2/viewer.p RowObject.Address RowObject.Address2 RowObject.City RowObject.State RowObject.PostalCode RowObject.Country ,RowObject. DISABLE_UI default Address Address2 City State Postal Code Country �  X#  �  �+      3 �    ��      0         pcFieldType     ��      T         pcColValues     ��      x         pcValue     ��      �         pcState �   ��      �         pcChanges       ��      �         pcChanges       ��               plCancel        ��      $        plAnswer        ��      H        plCancel        ��      l        pcRelative  �  ��      �        pcAction        ��      �        pcAction        ��      �        pcState     ��      �        pcReturn        ��              pcMode      ��      <        pcState     ��      \        pcNotValidFields    �  ��      �        pcProp      ��      �        pcProp      ��      �        plCancel    �  ��      �        pcProcName    ��             
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
 hTarget �  ��      �        pcMessage       ��      �        pcMessage       ��      �        plEnabled             	     cType       T	     T   �          D	                  getObjectType   .  F  H  �	        t	  
   hReposBuffer    �	        �	  
   hPropTable  �	        �	  
   hBuffer           �	  
   hTable  	  
     U   `	          
                  adm-clone-props -  .  /  0  2  3  4  5  6  7  8  9  <  ?  @  A  B            t
  
   hProc             �
        pcProcName  �	  �
  	   V   `
  |
      �
                  start-super-proc    x  y  z  {  |  }  �  �  �  �
  8     W                                   Q    l     X                                   U  V  <  �     Y                                   Y  Z  t  �     Z               �                  displayObjects  �  �        [                                     �  T     \                                     $  �     ]               �                  disable_UI  Y  Z  [  X  T  $    
 p      8                          �  �     RowObject   �         �         �         �         �         �         �                                    $         4         8         @         H         T         `         h         Address Address2    Balance City    Comments    Contact Country CreditLimit CustNum Discount    EmailAddress    Fax Name    Phone   PostalCode  SalesRep    State   Terms   �          �  
   appSrvUtils �        �  
   gshAstraAppserver   �        �  
   gshSessionManager           �  
   gshRIManager    ,          
   gshSecurityManager  T        @  
   gshProfileManager   �        h  
   gshRepositoryManager    �  	 	     �  
   gshTranslationManager   �  
 
     �  
   gshWebManager   �        �     gscSessionId                 gsdSessionObj   <        ,  
   gshFinManager   `        P  
   gshGenManager   �        t  
   gshAgnManager   �        �     gsdTempUniqueID �        �     gsdUserObj  �        �     gsdRenderTypeObj                 gsdSessionScopeObj  4       ,  
   ghProp  T       H  
   ghADMProps  x       h  
   ghADMPropsBuf   �       �     glADMLoadFromRepos  �       �     glADMOk �       �  
   ghContainer �       �     cObjectName     	        iStart  8    
   ,     cAppService X       L     cASDivision �       l     cServerOperatingMode    �       �     cFields �       �     cViewCols   �       �     cEnabled    �       �     iCol                iEntries             0     cEntry        X  H  RowObject            S   X  Y  [  \  �	  �	  �	  �	  
  "
  #
  $
  &
  (
  )
  *
  .
  /
  2
  3
  4
  5
  7
  9
  ;
  =
  >
  ?
  B
  D
  E
  G
  H
  I
  J
  K
  Q
  S
  Y
  [
  ]
  ^
  d
  e
  f
  g
  j
  k
  m
  n
  p
  q
  r
  s
  t
  u
  v
  x
  y
  z
  |
  }
  ~
  
  �
  �
  h  i  l  m  n  o  p  q  r  s  t  u  v  w  x  y  �  �  �  �  �  �                     	  
                              �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  G  U  V  X  Y  Z  [  a  b  c  d  e  f  g  h  i  k  l  m  n  o  p  q  r  s  t  u  v  x  y  {  |  }  ~  �  �  �  �  �     s  5  6  8  9  <  =  ?  G  �  �  �  �  �  �  �  �  �  �  �  �  �      &  �  �  �  �  �  �  �  �  �  �  �       1  d  �  �  N  O  P  R  T  X  q  r  s  u  }  �  �  �  �  �    2  c  d  e  g  i                 %  5  )      :%  C:\Progress\OpenEdge\src\adm2\viewer.i   �  �Q 2 %C:\Progress\OpenEdge\src\adm2\custom\viewercustom.i    } & C:\Progress\OpenEdge\src\adm2\datavis.i  X  � 1 %C:\Progress\OpenEdge\src\adm2\custom\dataviscustom.i �  f! ' C:\Progress\OpenEdge\src\adm2\containr.i �  � 0 %C:\Progress\OpenEdge\src\adm2\custom\containrcustom.i       �� ( C:\Progress\OpenEdge\src\adm2\visual.i   D  # / %C:\Progress\OpenEdge\src\adm2\custom\visualcustom.i  x  �< ) C:\Progress\OpenEdge\src\adm2\appserver.i    �  �� . %C:\Progress\OpenEdge\src\adm2\custom\appservercustom.i   �  I� * C:\Progress\OpenEdge\src\adm2\smart.i    4  Ds - C:\Progress\OpenEdge\gui\fn  h  tw , %C:\Progress\OpenEdge\src\adm2\custom\smartcustom.i   �  Q. + C:\Progress\OpenEdge\gui\set �  �/  C:\Progress\OpenEdge\src\adm2\viewprop.i �  �� $ %C:\Progress\OpenEdge\src\adm2\custom\viewpropcustom.i    ,  ۃ % %C:\Progress\OpenEdge\src\adm2\custom\viewprtocustom.i    p  ��  C:\Progress\OpenEdge\src\adm2\dvisprop.i �  B� " %C:\Progress\OpenEdge\src\adm2\custom\dvispropcustom.i    �  �� # %C:\Progress\OpenEdge\src\adm2\custom\dvisprtocustom.i    ,  ��  C:\Progress\OpenEdge\src\adm2\cntnprop.i p  ��   %C:\Progress\OpenEdge\src\adm2\custom\cntnpropcustom.i    �  P ! %C:\Progress\OpenEdge\src\adm2\custom\cntnprtocustom.i    �  F>  C:\Progress\OpenEdge\src\adm2\visprop.i  ,  �I  %C:\Progress\OpenEdge\src\adm2\custom\vispropcustom.i `  ��  %C:\Progress\OpenEdge\src\adm2\custom\visprtocustom.i �  �l  C:\Progress\OpenEdge\src\adm2\appsprop.i �  ɏ  %C:\Progress\OpenEdge\src\adm2\custom\appspropcustom.i      V  %C:\Progress\OpenEdge\src\adm2\custom\appsprtocustom.i    X  i$  C:\Progress\OpenEdge\src\adm2\smrtprop.i �  �j  C:\Progress\OpenEdge\gui\get �  �  %C:\Progress\OpenEdge\src\adm2\custom\smrtpropcustom.i    �  ��  %C:\Progress\OpenEdge\src\adm2\custom\smrtprtocustom.i    <  ��  C:\Progress\OpenEdge\src\adm2\smrtprto.i �  Su  C:\Progress\OpenEdge\src\adm2\globals.i  �  M�  %C:\Progress\OpenEdge\src\adm2\custom\globalscustom.i �  )a  %C:\Progress\OpenEdge\src\adm2\custom\smartdefscustom.i   (   �  C:\Progress\OpenEdge\src\adm2\appsprto.i l   ��  %C:\Progress\OpenEdge\src\adm2\custom\appserverdefscustom.i   �   �X  C:\Progress\OpenEdge\src\adm2\visprto.i  �   !�  %C:\Progress\OpenEdge\src\adm2\custom\visualdefscustom.i  !  n�  C:\Progress\OpenEdge\src\adm2\cntnprto.i `!  ;  %C:\Progress\OpenEdge\src\adm2\custom\containrdefscustom.i    �!  �7 
 C:\Progress\OpenEdge\src\adm2\dvisprto.i �!  0 	 %C:\Progress\OpenEdge\src\adm2\custom\datavisdefscustom.i "  ��  C:\Progress\OpenEdge\src\adm2\viewprto.i T"  gf  %C:\Progress\OpenEdge\src\adm2\custom\viewerdefscustom.i  �"  ~�  C:\Progress\OpenEdge\src\adm2\widgetprto.i   �"  �D  .\aplic\dcust.i  #  e�  !C:\Progress\OpenEdge\gui\adecomm\appserv.i    #  �;    C:\newsie\on_in_co\aplic\vcustaddress.w      �         �#  �   �     �#     �  2   �#  �   �     �#     u  +   �#  �   r     �#     P  +   �#  �   O     �#     -  +   $  \   �     $  o   �  &   ,$     m  1   <$  U   S  &   L$  �   L  '   \$     *  +   l$  �   %  '   |$       +   �$  �   �  '   �$     �  0   �$  �   �  '   �$     �  -   �$  �   �  '   �$     �  -   �$  �   �  '   �$     ~  -   %  r   b  '   %  n   J  (   ,%     �  /   <%  P   �  (   L%  �   �  )   \%     s  .   l%  �   n  )   |%     L  +   �%  �   K  )   �%     )  +   �%  �   '  )   �%       +   �%  g   �  )   �%     �     �%  O   �  )   �%  �   >  *   &     <  -   &  �     *   ,&     �  ,   <&  �   �  *   L&     �  +   \&  �   �  *   l&     d  +   |&  �   c  *   �&     A  +   �&  �   @  *   �&       +   �&  �     *   �&     �  +   �&  �   �  *   �&     �  +   �&  }   �  *   '     �  +   '       *   ,'     �  )   <'       (   L'       '   \'     �  &   l'     |     |'  u   s     �'  O   e  $   �'     T  %   �'       $   �'  h   �     �'  �   �     �'  O   �  "   �'     �  #   �'     �  "   (  {   P     (  �   G     ,(  O   9      <(     (  !   L(     �      \(  �   �     l(  �   �     |(  O   {     �(     j     �(          �(  �   �     �(  x   �     �(  M   �     �(     �     �(     }     �(  a   f     )  �  E     )     &     ,)  �  �
     <)  O   �
     L)     �
     \)     �
     l)  �   �	     |)     �     �)     �     �)  x   �     �)     �     �)     A     �)     =     �)     )     �)          �)  Q         *     �     *     n     ,*     Z     <*     @     L*  f        \*     �     l*  "   p     |*     \     �*     ;     �*  Z   �     �*     �     �*     �     �*     �     �*     �     �*  X   b     �*     �  
   +      t     +     `  	   ,+     A     <+  ]   6     L+     �     \+     �     l+     �     |+     �     �+     o     �+  0   �       �+     \      �+     9       �+     &      �+     !       �+           
	��V�~L\85  X�                                              ǳ 3538010Butf-8 MAIN D:\newsie\on_in_co\aplic\logis\d-reasignar-orden.w,,INPUT pCodPHR CHARACTER,INPUT pNroPHR CHARACTER,INPUT pCodOrden CHARACTER,INPUT pNroOrden CHARACTER PROCEDURE Reasignar-Orden,, PROCEDURE initializeObject,, PROCEDURE enable_UI,, PROCEDURE disable_UI,, PROCEDURE adm-create-objects,, PROCEDURE start-super-proc,,INPUT pcProcName CHARACTER PROCEDURE adm-clone-props,, PROCEDURE toggleData,,INPUT plEnabled LOGICAL PROCEDURE showMessageProcedure,,INPUT pcMessage CHARACTER,OUTPUT plAnswer LOGICAL PROCEDURE returnFocus,,INPUT hTarget HANDLE PROCEDURE repositionObject,,INPUT pdRow DECIMAL,INPUT pdCol DECIMAL PROCEDURE removeLink,,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE PROCEDURE removeAllLinks,, PROCEDURE modifyUserLinks,,INPUT pcMod CHARACTER,INPUT pcLinkName CHARACTER,INPUT phObject HANDLE PROCEDURE modifyListProperty,,INPUT phCaller HANDLE,INPUT pcMode CHARACTER,INPUT pcListName CHARACTER,INPUT pcListValue CHARACTER PROCEDURE hideObject,, PROCEDURE exitObject,, PROCEDURE editInstanceProperties,, PROCEDURE displayLinks,, PROCEDURE createControls,, PROCEDURE changeCursor,,INPUT pcCursor CHARACTER PROCEDURE applyEntry,,INPUT pcField CHARACTER PROCEDURE adjustTabOrder,,INPUT phObject HANDLE,INPUT phAnchor HANDLE,INPUT pcPosition CHARACTER PROCEDURE addMessage,,INPUT pcText CHARACTER,INPUT pcField CHARACTER,INPUT pcTable CHARACTER PROCEDURE addLink,,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE PROCEDURE unbindServer,,INPUT pcMode CHARACTER PROCEDURE startServerObject,, PROCEDURE runServerObject,,INPUT phAppService HANDLE PROCEDURE restartServerObject,, PROCEDURE initializeServerObject,, PROCEDURE disconnectObject,, PROCEDURE destroyServerObject,, PROCEDURE bindServer,, PROCEDURE processAction,,INPUT pcAction CHARACTER PROCEDURE enableObject,, PROCEDURE disableObject,, PROCEDURE applyLayout,, PROCEDURE viewPage,,INPUT piPageNum INTEGER PROCEDURE viewObject,, PROCEDURE toolbar,,INPUT pcValue CHARACTER PROCEDURE selectPage,,INPUT piPageNum INTEGER PROCEDURE removePageNTarget,,INPUT phTarget HANDLE,INPUT piPage INTEGER PROCEDURE passThrough,,INPUT pcLinkName CHARACTER,INPUT pcArgument CHARACTER PROCEDURE notifyPage,,INPUT pcProc CHARACTER PROCEDURE initPages,,INPUT pcPageList CHARACTER PROCEDURE initializeVisualContainer,, PROCEDURE hidePage,,INPUT piPageNum INTEGER PROCEDURE destroyObject,, PROCEDURE deletePage,,INPUT piPageNum INTEGER PROCEDURE createObjects,, PROCEDURE constructObject,,INPUT pcProcName CHARACTER,INPUT phParent HANDLE,INPUT pcPropList CHARACTER,OUTPUT phObject HANDLE PROCEDURE confirmExit,,INPUT-OUTPUT plCancel LOGICAL PROCEDURE changePage,, PROCEDURE assignPageProperty,,INPUT pcProp CHARACTER,INPUT pcValue CHARACTER FUNCTION Signature,CHARACTER,INPUT pcName CHARACTER FUNCTION showmessage,LOGICAL,INPUT pcMessage CHARACTER FUNCTION setUserProperty,LOGICAL,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER FUNCTION setUIBMode,LOGICAL,INPUT pcMode CHARACTER FUNCTION setTranslatableProperties,LOGICAL,INPUT pcPropList CHARACTER FUNCTION setSupportedLinks,LOGICAL,INPUT pcLinkList CHARACTER FUNCTION setRunAttribute,LOGICAL,INPUT cRunAttribute CHARACTER FUNCTION setPhysicalVersion,LOGICAL,INPUT cVersion CHARACTER FUNCTION setPhysicalObjectName,LOGICAL,INPUT cTemp CHARACTER FUNCTION setPassThroughLinks,LOGICAL,INPUT pcLinks CHARACTER FUNCTION setParentDataKey,LOGICAL,INPUT cParentDataKey CHARACTER FUNCTION setObjectVersion,LOGICAL,INPUT cObjectVersion CHARACTER FUNCTION setObjectParent,LOGICAL,INPUT phParent HANDLE FUNCTION setObjectName,LOGICAL,INPUT pcName CHARACTER FUNCTION setLogicalVersion,LOGICAL,INPUT cVersion CHARACTER FUNCTION setLogicalObjectName,LOGICAL,INPUT c CHARACTER FUNCTION setInstanceProperties,LOGICAL,INPUT pcPropList CHARACTER FUNCTION setDynamicObject,LOGICAL,INPUT lTemp LOGICAL FUNCTION setDesignDataObject,LOGICAL,INPUT pcDataObject CHARACTER FUNCTION setDBAware,LOGICAL,INPUT lAware LOGICAL FUNCTION setDataTargetEvents,LOGICAL,INPUT pcEvents CHARACTER FUNCTION setDataTarget,LOGICAL,INPUT pcTarget CHARACTER FUNCTION setDataSourceNames,LOGICAL,INPUT pcSourceNames CHARACTER FUNCTION setDataSourceEvents,LOGICAL,INPUT pcEventsList CHARACTER FUNCTION setDataSource,LOGICAL,INPUT phObject HANDLE FUNCTION setDataLinksEnabled,LOGICAL,INPUT lDataLinksEnabled LOGICAL FUNCTION setContainerSourceEvents,LOGICAL,INPUT pcEvents CHARACTER FUNCTION setContainerSource,LOGICAL,INPUT phObject HANDLE FUNCTION setContainerHidden,LOGICAL,INPUT plHidden LOGICAL FUNCTION setChildDataKey,LOGICAL,INPUT cChildDataKey CHARACTER FUNCTION reviewMessages,CHARACTER, FUNCTION propertyType,CHARACTER,INPUT pcPropName CHARACTER FUNCTION messageNumber,CHARACTER,INPUT piMessage INTEGER FUNCTION mappedEntry,CHARACTER,INPUT pcEntry CHARACTER,INPUT pcList CHARACTER,INPUT plFirst LOGICAL,INPUT pcDelimiter CHARACTER FUNCTION linkProperty,CHARACTER,INPUT pcLink CHARACTER,INPUT pcPropName CHARACTER FUNCTION linkHandles,CHARACTER,INPUT pcLink CHARACTER FUNCTION instancePropertyList,CHARACTER,INPUT pcPropList CHARACTER FUNCTION getUserProperty,CHARACTER,INPUT pcPropName CHARACTER FUNCTION getUIBMode,CHARACTER, FUNCTION getTranslatableProperties,CHARACTER, FUNCTION getSupportedLinks,CHARACTER, FUNCTION getRunAttribute,CHARACTER, FUNCTION getQueryObject,LOGICAL, FUNCTION getPropertyDialog,CHARACTER, FUNCTION getPhysicalVersion,CHARACTER, FUNCTION getPhysicalObjectName,CHARACTER, FUNCTION getPassThroughLinks,CHARACTER, FUNCTION getParentDataKey,CHARACTER, FUNCTION getObjectVersionNumber,CHARACTER, FUNCTION getObjectVersion,CHARACTER, FUNCTION getObjectParent,HANDLE, FUNCTION getObjectPage,INTEGER, FUNCTION getObjectName,CHARACTER, FUNCTION getObjectInitialized,LOGICAL, FUNCTION getObjectHidden,LOGICAL, FUNCTION getLogicalVersion,CHARACTER, FUNCTION getLogicalObjectName,CHARACTER, FUNCTION getInstanceProperties,CHARACTER, FUNCTION getDynamicObject,LOGICAL, FUNCTION getDesignDataObject,CHARACTER, FUNCTION getDBAware,LOGICAL, FUNCTION getDataTargetEvents,CHARACTER, FUNCTION getDataTarget,CHARACTER, FUNCTION getDataSourceNames,CHARACTER, FUNCTION getDataSourceEvents,CHARACTER, FUNCTION getDataSource,HANDLE, FUNCTION getDataLinksEnabled,LOGICAL, FUNCTION getContainerType,CHARACTER, FUNCTION getContainerSourceEvents,CHARACTER, FUNCTION getContainerSource,HANDLE, FUNCTION getContainerHidden,LOGICAL, FUNCTION getContainerHandle,HANDLE, FUNCTION getChildDataKey,CHARACTER, FUNCTION fetchMessages,CHARACTER, FUNCTION assignLinkProperty,LOGICAL,INPUT pcLink CHARACTER,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER FUNCTION anyMessage,LOGICAL, FUNCTION setServerOperatingMode,LOGICAL,INPUT pcServerOperatingMode CHARACTER FUNCTION setServerFileName,LOGICAL,INPUT pcFileName CHARACTER FUNCTION setASUsePrompt,LOGICAL,INPUT plFlag LOGICAL FUNCTION setASInitializeOnRun,LOGICAL,INPUT plInitialize LOGICAL FUNCTION setASInfo,LOGICAL,INPUT pcInfo CHARACTER FUNCTION setASHandle,LOGICAL,INPUT phASHandle HANDLE FUNCTION setASDivision,LOGICAL,INPUT pcDivision CHARACTER FUNCTION setAppService,LOGICAL,INPUT pcAppService CHARACTER FUNCTION runServerProcedure,HANDLE,INPUT pcServerFileName CHARACTER,INPUT phAppService HANDLE FUNCTION getServerOperatingMode,CHARACTER, FUNCTION getServerFileName,CHARACTER, FUNCTION getASUsePrompt,LOGICAL, FUNCTION getASInitializeOnRun,LOGICAL, FUNCTION getASInfo,CHARACTER, FUNCTION getASHasStarted,LOGICAL, FUNCTION getASHandle,HANDLE, FUNCTION getAsDivision,CHARACTER, FUNCTION getASBound,LOGICAL, FUNCTION getAppService,CHARACTER, FUNCTION createUiEvents,LOGICAL, FUNCTION getObjectSecured,LOGICAL, FUNCTION getObjectTranslated,LOGICAL, FUNCTION setResizeVertical,LOGICAL,INPUT plResizeVertical LOGICAL FUNCTION setResizeHorizontal,LOGICAL,INPUT plResizeHorizontal LOGICAL FUNCTION setObjectLayout,LOGICAL,INPUT pcLayout CHARACTER FUNCTION setLayoutOptions,LOGICAL,INPUT pcOptions CHARACTER FUNCTION setHideOnInit,LOGICAL,INPUT plHide LOGICAL FUNCTION setDisableOnInit,LOGICAL,INPUT plDisable LOGICAL FUNCTION setDefaultLayout,LOGICAL,INPUT pcDefault CHARACTER FUNCTION setAllFieldNames,LOGICAL,INPUT pcValue CHARACTER FUNCTION setAllFieldHandles,LOGICAL,INPUT pcValue CHARACTER FUNCTION getResizeVertical,LOGICAL, FUNCTION getResizeHorizontal,LOGICAL, FUNCTION getWidth,DECIMAL, FUNCTION getRow,DECIMAL, FUNCTION getObjectLayout,CHARACTER, FUNCTION getObjectEnabled,LOGICAL, FUNCTION getLayoutVariable,CHARACTER, FUNCTION getLayoutOptions,CHARACTER, FUNCTION getHideOnInit,LOGICAL, FUNCTION getHeight,DECIMAL, FUNCTION getEnabledObjHdls,CHARACTER, FUNCTION getEnabledObjFlds,CHARACTER, FUNCTION getDisableOnInit,LOGICAL, FUNCTION getDefaultLayout,CHARACTER, FUNCTION getCol,DECIMAL, FUNCTION getAllFieldNames,CHARACTER, FUNCTION getAllFieldHandles,CHARACTER, FUNCTION setStatusArea,LOGICAL,INPUT plStatusArea LOGICAL FUNCTION getObjectType,character, FUNCTION setWindowTitleViewer,LOGICAL,INPUT phViewer HANDLE FUNCTION setWaitForObject,LOGICAL,INPUT phObject HANDLE FUNCTION setUpdateTarget,LOGICAL,INPUT pcTarget CHARACTER FUNCTION setUpdateSource,LOGICAL,INPUT pcSource CHARACTER FUNCTION setTopOnly,LOGICAL,INPUT plTopOnly LOGICAL FUNCTION setSdoForeignFields,LOGICAL,INPUT cSdoForeignFields CHARACTER FUNCTION setSavedContainerMode,LOGICAL,INPUT cSavedContainerMode CHARACTER FUNCTION setRunMultiple,LOGICAL,INPUT plMultiple LOGICAL FUNCTION setRunDOOptions,LOGICAL,INPUT pcOptions CHARACTER FUNCTION setRouterTarget,LOGICAL,INPUT phObject HANDLE FUNCTION setReEnableDataLinks,LOGICAL,INPUT cReEnableDataLinks CHARACTER FUNCTION setPrimarySdoTarget,LOGICAL,INPUT hPrimarySdoTarget HANDLE FUNCTION setPageSource,LOGICAL,INPUT phObject HANDLE FUNCTION setPageNTarget,LOGICAL,INPUT pcObject CHARACTER FUNCTION setOutMessageTarget,LOGICAL,INPUT phObject HANDLE FUNCTION setNavigationTarget,LOGICAL,INPUT cTarget CHARACTER FUNCTION setNavigationSourceEvents,LOGICAL,INPUT pcEvents CHARACTER FUNCTION setNavigationSource,LOGICAL,INPUT pcSource CHARACTER FUNCTION setMultiInstanceSupported,LOGICAL,INPUT lMultiInstanceSupported LOGICAL FUNCTION setMultiInstanceActivated,LOGICAL,INPUT lMultiInstanceActivated LOGICAL FUNCTION setInMessageTarget,LOGICAL,INPUT phObject HANDLE FUNCTION setFilterSource,LOGICAL,INPUT phObject HANDLE FUNCTION setDynamicSDOProcedure,LOGICAL,INPUT pcProc CHARACTER FUNCTION setDisabledAddModeTabs,LOGICAL,INPUT cDisabledAddModeTabs CHARACTER FUNCTION setCurrentPage,LOGICAL,INPUT iPage INTEGER FUNCTION setContainerTarget,LOGICAL,INPUT pcObject CHARACTER FUNCTION setContainerMode,LOGICAL,INPUT cContainerMode CHARACTER FUNCTION setCallerWindow,LOGICAL,INPUT h HANDLE FUNCTION setCallerProcedure,LOGICAL,INPUT h HANDLE FUNCTION setCallerObject,LOGICAL,INPUT h HANDLE FUNCTION pageNTargets,CHARACTER,INPUT phTarget HANDLE,INPUT piPageNum INTEGER FUNCTION getStatusArea,LOGICAL, FUNCTION getWindowTitleViewer,HANDLE, FUNCTION getWaitForObject,HANDLE, FUNCTION getUpdateTarget,CHARACTER, FUNCTION getUpdateSource,CHARACTER, FUNCTION getTopOnly,LOGICAL, FUNCTION getSdoForeignFields,CHARACTER, FUNCTION getSavedContainerMode,CHARACTER, FUNCTION getRunMultiple,LOGICAL, FUNCTION getRunDOOptions,CHARACTER, FUNCTION getReEnableDataLinks,CHARACTER, FUNCTION getPrimarySdoTarget,HANDLE, FUNCTION getPageSource,HANDLE, FUNCTION getPageNTarget,CHARACTER, FUNCTION getOutMessageTarget,HANDLE, FUNCTION getNavigationTarget,HANDLE, FUNCTION getNavigationSourceEvents,CHARACTER, FUNCTION getNavigationSource,CHARACTER, FUNCTION getMultiInstanceSupported,LOGICAL, FUNCTION getMultiInstanceActivated,LOGICAL, FUNCTION getFilterSource,HANDLE, FUNCTION getDynamicSDOProcedure,CHARACTER, FUNCTION getDisabledAddModeTabs,CHARACTER, FUNCTION getCurrentPage,INTEGER, FUNCTION getContainerTargetEvents,CHARACTER, FUNCTION getContainerTarget,CHARACTER, FUNCTION getContainerMode,CHARACTER, FUNCTION getCallerWindow,HANDLE, FUNCTION getCallerProcedure,HANDLE, FUNCTION enablePagesInFolder,LOGICAL,INPUT pcPageInformation CHARACTER FUNCTION disablePagesInFolder,LOGICAL,INPUT pcPageInformation CHARACTER FUNCTION widgetValueList,CHARACTER,INPUT pcNameList CHARACTER,INPUT pcDelimiter CHARACTER FUNCTION widgetValue,CHARACTER,INPUT pcName CHARACTER FUNCTION widgetIsTrue,LOGICAL,INPUT pcName CHARACTER FUNCTION widgetIsModified,LOGICAL,INPUT pcNameList CHARACTER FUNCTION widgetIsFocused,LOGICAL,INPUT pcName CHARACTER FUNCTION widgetIsBlank,LOGICAL,INPUT pcNameList CHARACTER FUNCTION widgetLongcharValue,LONGCHAR,INPUT pcName CHARACTER FUNCTION widgetHandle,HANDLE,INPUT pcName CHARACTER FUNCTION viewWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION toggleWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION resetWidgetValue,LOGICAL,INPUT pcNameList CHARACTER FUNCTION highlightWidget,LOGICAL,INPUT pcNameList CHARACTER,INPUT pcHighlightType CHARACTER FUNCTION hideWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION formattedWidgetValueList,CHARACTER,INPUT pcNameList CHARACTER,INPUT pcDelimiter CHARACTER FUNCTION formattedWidgetValue,CHARACTER,INPUT pcName CHARACTER FUNCTION enableWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION enableRadioButton,LOGICAL,INPUT pcNameList CHARACTER,INPUT piButtonNum INTEGER FUNCTION disableWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION disableRadioButton,LOGICAL,INPUT pcNameList CHARACTER,INPUT piButtonNum INTEGER FUNCTION clearWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION blankWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION assignWidgetValueList,LOGICAL,INPUT pcNameList CHARACTER,INPUT pcValueList CHARACTER,INPUT pcDelimiter CHARACTER FUNCTION assignWidgetValue,LOGICAL,INPUT pcName CHARACTER,INPUT pcValue CHARACTER FUNCTION assignFocusedWidget,LOGICAL,INPUT pcName CHARACTER      0!              (�              �� 0!  ��              �d              %    +   O �  7   �S `  8   W �   >   X 8  ?   <Y \  @   �]    A   �_ �  B           Pg �  (j �  ? $n �   iSO8859-1                                                                           $     �                                       �              �  $�                �       8   ��    �  �          ��  �   �       !          4                                             PROGRESS                         8           
    
                    <              �                                                                                                     
          �             �                 �             �                                �         *                                �             �                                                                                          �             �                                                                                                        �                          INTEGRAL                         PROGRESS                         �     �  4      �   C                      �Q�[            �  k�                              �                        �    �e     CODCIACODPEDNROPEDFCHPEDFCHVENCODCLINOMCLIDIRCLICODDIVLUGENTLUGENT2FCHENTSEDECODSEDRUCCLINROORDCODALMCODMONTPOCMBUSUARIOUSRAPROBACIONDNICLIUSRANUUSRMODUSRDSCTOFCHCREFCHANUFCHMODFCHDSCTOOBSERVAFLGIGVIMPBRTIMPEXOIMPIGVIMPDTOIMPTOTSDOACTIMPISCIMPVTAIMPFLEIMPINTIMPCTOPORDTOPORIGVPESMATMRGUTICODORINROORIFLGESTFLGSITFCHSITNROCARDFMAPGONROREFCODREFCODDEPTCODPROVCODDISTCODPOSFLGENVCMPBNTETPOLICCODVENHORALIBRE_C01LIBRE_C02LIBRE_C03LIBRE_C04LIBRE_C05LIBRE_D01LIBRE_D02LIBRE_F01LIBRE_F02GLOSACODTERIMPORTEFCHAPROBACIONFECSACHORSACUBIGEOUSRSACDIVDESUSRIMPODFCHIMPODUSRACTFECACTHORACTMOTREPOSICIONVTAPUNTUALORDCMPFLGIMPODZONAPICKEOUSRSACASIGNUSRSACRECEPUSUARIOINICIOUSUARIOFINITEMSPESOVOLUMENFCHFINFCHINICIO                                                                        	          
                                                                                                                                                                             "          "          "          "       !          "          #          $          %          &          '          (          )          *          +          ,          -          .          /          0          1          2          3          4          5          6   "       7          8          9          :          ;          <          =          >          ?          @          A          B          C          D          E          F          G          H          I          J          K          L          M          N          O         P          Q          R          S         T          U          V          W   "       X          Y          Z          [          \          ]          ^          _          `          a          b          c          d          e          f          h   "       i   "       h     2  4      2                         p��[            ;  E�                              �                        �    �4     CODCIACODPROCODDOCNRODOCFCHDOCFLGESTUSUARIOCODRUTDESRUTNOMTRACODCOBTPOTRACODMONCODVEHCTORUTFCHRETHORSALHORRETOBSERVCODDIVKMTINIKMTFINRESPONSABLEAYUDANTE-1AYUDANTE-2FCHSALGUIATRANSPORTISTAUSRCIERREFCHCIERRELIBRE_C01LIBRE_C02LIBRE_C03LIBRE_C04LIBRE_C05LIBRE_D01LIBRE_D02LIBRE_D03LIBRE_D04LIBRE_D05LIBRE_L01LIBRE_L02LIBRE_L03LIBRE_L04LIBRE_L05LIBRE_F01LIBRE_F02LIBRE_F03LIBRE_F04LIBRE_F05FCHAPROBACIONGLOSAAPROBACIONUSRAPROBACION                                                                        	          
                                                                                                                                                                                                                             !          "          #          $          %          &          '          (          )          *          +          ,          -          .          /          0          1          2          3          4          5          6          �  a
      `  
    
                  L               �                                                                                          a
          
  �  s
        
    
                  �  �             |                                                                                          s
          
  <  �
      �  
    
                  �  l  	           (                                                                                          �
          
  �  �
      d  
    
                  P    
           �                                                                                          �
          
  �  �
        
    
                  �  �             �                                                                                          �
          
  @  �
      �  
    
                  �  p             ,                                                                                          �
          
  �  �
      h  
    
                  T               �                                                                                          �
          
  �  �
        
    
                     �             �                                                                                          �
          
  D  �
      �                         �  t             0                                                                                          �
            �  �
      l                        X                �                                                                                          �
            �          
    
                    �             �                                                                                                    
  H        �  
    
                  �  x             4                                                                                                    
  �  '      p  
    
                  \  $             �                                                                                          '          
  �  5                                �             �                                                                                          5            L  E      �                        �  |             8                                                                                          E            �  P      t                        `  (             �                                                                                          P                a                                 �             �                                                                                          a                      4                                �Q�[               �                              �  $                      �  4  �      CODCIACODDOCNRODOCCODREFNROREFFLGESTCODDIVHORATEGLOSAIMPCOBMONCOBHORESTHORLLEHORPARFLGESTDETLIBRE_C01LIBRE_C02LIBRE_C03LIBRE_C04LIBRE_C05LIBRE_D01LIBRE_D02LIBRE_F01LIBRE_F02                                                                         	          
                                                                                                                                                                             	 ��                                              3 ��          �  �  H pl                            Seleccione la PHR destino!!! �                               
             
             
                                         
                                                                                                                H   X   �   �   �   �   �   �   �   �           0  @  P  `      H   X   �   �   �   �   �   �   �   �          0  @  P  `    ��                                                                              q          ����                            L    ��  2                 �c    �    �E    �    (�    �    =�    �    W
    undefined                                                               �       Ľ  �   l   Խ                        �����               �>                    O   ����    e�          O   ����    R�          O   ����    ��      t       �   �              4   ����     /                                    3   ����       $     H  ���                       8      
                       � ߱        �  �      D            <          assignFocusedWidget         �      �     4       LOGICAL,INPUT pcName CHARACTER  assignWidgetValue   �      �      $    H       LOGICAL,INPUT pcName CHARACTER,INPUT pcValue CHARACTER  assignWidgetValueList         \      �    Z       LOGICAL,INPUT pcNameList CHARACTER,INPUT pcValueList CHARACTER,INPUT pcDelimiter CHARACTER  blankWidget t      �          p       LOGICAL,INPUT pcNameList CHARACTER  clearWidget �      @      l    |       LOGICAL,INPUT pcNameList CHARACTER  disableRadioButton  L      �      �    �       LOGICAL,INPUT pcNameList CHARACTER,INPUT piButtonNum INTEGER    disableWidget   �            4    �       LOGICAL,INPUT pcNameList CHARACTER  enableRadioButton         X      �    �       LOGICAL,INPUT pcNameList CHARACTER,INPUT piButtonNum INTEGER    enableWidget    l      �      �    �       LOGICAL,INPUT pcNameList CHARACTER  formattedWidgetValue    �             X  	  �       CHARACTER,INPUT pcName CHARACTER    formattedWidgetValueList    8      |      �  
  �       CHARACTER,INPUT pcNameList CHARACTER,INPUT pcDelimiter CHARACTER    hideWidget  �      �      (   
 �       LOGICAL,INPUT pcNameList CHARACTER  highlightWidget       L      |          LOGICAL,INPUT pcNameList CHARACTER,INPUT pcHighlightType CHARACTER  resetWidgetValue    \      �      �          LOGICAL,INPUT pcNameList CHARACTER  toggleWidget    �            H    "      LOGICAL,INPUT pcNameList CHARACTER  viewWidget  (      l      �   
 /      LOGICAL,INPUT pcNameList CHARACTER  widgetHandle    x      �      �    :      HANDLE,INPUT pcName CHARACTER   widgetLongcharValue �            @    G      LONGCHAR,INPUT pcName CHARACTER widgetIsBlank          `      �    [      LOGICAL,INPUT pcNameList CHARACTER  widgetIsFocused p      �      �    i      LOGICAL,INPUT pcName CHARACTER  widgetIsModified    �      	      8	    y      LOGICAL,INPUT pcNameList CHARACTER  widgetIsTrue    	      \	      �	    �      LOGICAL,INPUT pcName CHARACTER  widgetValue l	      �	      �	    �      CHARACTER,INPUT pcName CHARACTER    widgetValueList �	      �	      ,
    �      CHARACTER,INPUT pcNameList CHARACTER,INPUT pcDelimiter CHARACTER        u   ����  �             �   �           �   �          �   �             �              � ߱            Z   �����
   �p
                     8�    �  $  �      4      4   ����4                �                      ��                  �  �                  ��;                       �  4  4    �  �  �      L      4   ����L      $  �    ���                       �  @         |              � ߱              �  P  `      �      4   �����      $  �  �  ���                         @         �              � ߱        assignPageProperty                              P  8      ��                  .  1  h              �Q�                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �             �               ��                  �           ��                            ����                            changePage                              �  �      ��                  3  4  �              dW�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            confirmExit                             �  �      ��                  6  8  �              �a`                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �           ��                            ����                            constructObject                             �  �      ��                  :  ?  �              �e`                    O   ����    e�          O   ����    R�          O   ����    ��            ��   0             �               �� 
  X             $  
             ��   �             L               �� 
                 t  
         ��                            ����                            createObjects                               p  X      ��                  A  B  �              �QN                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            deletePage                              p  X      ��                  D  F  �              �TN                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �           ��                            ����                            destroyObject                               �  �      ��                  H  I  �              YN                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            hidePage                                �  �      ��                  K  M  �              �YN                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �           ��                            ����                            initializeObject                                �  �      ��                  O  P  �              z�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeVisualContainer                               �  �      ��                  R  S  �              lz�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initPages                               �  �      ��                  U  W  �              ${�                    O   ����    e�          O   ����    R�          O   ����    ��            ��                             ��                            ����                            notifyPage                                �      ��                  Y  [                h�                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  4           ��                            ����                            passThrough                             ,        ��                  ]  `  D              h��                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �             \               ��                  �           ��                            ����                            removePageNTarget                               �  l      ��                  b  e  �              ���                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  �             �  
             ��                  �           ��                            ����                            selectPage                              �  �      ��                  g  i  �              ���                    O   ����    e�          O   ����    R�          O   ����    ��            ��                             ��                            ����                            toolbar                             �  �      ��                  k  m                @��                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  (           ��                            ����                            viewObject                                         ��                  o  p  8               ���                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            viewPage                                 !  !      ��                  r  t  8!              4��                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  P!           ��                            ����                            disablePagesInFolder    
      �!      �!    �      LOGICAL,INPUT pcPageInformation CHARACTER   enablePagesInFolder �!      "      P"    �      LOGICAL,INPUT pcPageInformation CHARACTER   getCallerProcedure  0"      |"      �"    �      HANDLE, getCallerWindow �"      �"      �"    �      HANDLE, getContainerMode    �"      �"      $#          CHARACTER,  getContainerTarget  #      0#      d#          CHARACTER,  getContainerTargetEvents    D#      p#      �#    +      CHARACTER,  getCurrentPage  �#      �#      �#    D      INTEGER,    getDisabledAddModeTabs  �#      �#      ,$     S      CHARACTER,  getDynamicSDOProcedure  $      8$      p$  !  j      CHARACTER,  getFilterSource P$      |$      �$  "  �      HANDLE, getMultiInstanceActivated   �$      �$      �$  #  �      LOGICAL,    getMultiInstanceSupported   �$      �$      8%  $  �      LOGICAL,    getNavigationSource %      D%      x%  %  �      CHARACTER,  getNavigationSourceEvents   X%      �%      �%  &  �      CHARACTER,  getNavigationTarget �%      �%       &  '  �      HANDLE, getOutMessageTarget �%      &      <&  (        HANDLE, getPageNTarget  &      D&      t&  )        CHARACTER,  getPageSource   T&      �&      �&  *  *      HANDLE, getPrimarySdoTarget �&      �&      �&  +  8      HANDLE, getReEnableDataLinks    �&      �&      ,'  ,  L      CHARACTER,  getRunDOOptions '      8'      h'  -  a      CHARACTER,  getRunMultiple  H'      t'      �'  .  q      LOGICAL,    getSavedContainerMode   �'      �'      �'  /  �      CHARACTER,  getSdoForeignFields �'      �'      ((  0  �      CHARACTER,  getTopOnly  (      4(      `(  1 
 �      LOGICAL,    getUpdateSource @(      l(      �(  2  �      CHARACTER,  getUpdateTarget |(      �(      �(  3  �      CHARACTER,  getWaitForObject    �(      �(      )  4  �      HANDLE, getWindowTitleViewer    �(       )      X)  5  �      HANDLE, getStatusArea   8)      `)      �)  6  �      LOGICAL,    pageNTargets    p)      �)      �)  7  	      CHARACTER,INPUT phTarget HANDLE,INPUT piPageNum INTEGER setCallerObject �)      *      4*  8        LOGICAL,INPUT h HANDLE  setCallerProcedure  *      L*      �*  9  &      LOGICAL,INPUT h HANDLE  setCallerWindow `*      �*      �*  :  9      LOGICAL,INPUT h HANDLE  setContainerMode    �*      �*      +  ;  I      LOGICAL,INPUT cContainerMode CHARACTER  setContainerTarget  �*      <+      p+  <  Z      LOGICAL,INPUT pcObject CHARACTER    setCurrentPage  P+      �+      �+  =  m      LOGICAL,INPUT iPage INTEGER setDisabledAddModeTabs  �+      �+      ,  >  |      LOGICAL,INPUT cDisabledAddModeTabs CHARACTER    setDynamicSDOProcedure  �+      H,      �,  ?  �      LOGICAL,INPUT pcProc CHARACTER  setFilterSource `,      �,      �,  @  �      LOGICAL,INPUT phObject HANDLE   setInMessageTarget  �,      �,      $-  A  �      LOGICAL,INPUT phObject HANDLE   setMultiInstanceActivated   -      D-      �-  B  �      LOGICAL,INPUT lMultiInstanceActivated LOGICAL   setMultiInstanceSupported   `-      �-      �-  C  �      LOGICAL,INPUT lMultiInstanceSupported LOGICAL   setNavigationSource �-      .      P.  D        LOGICAL,INPUT pcSource CHARACTER    setNavigationSourceEvents   0.      t.      �.  E        LOGICAL,INPUT pcEvents CHARACTER    setNavigationTarget �.      �.      /  F  /      LOGICAL,INPUT cTarget CHARACTER setOutMessageTarget �.      (/      \/  G  C      LOGICAL,INPUT phObject HANDLE   setPageNTarget  </      |/      �/  H  W      LOGICAL,INPUT pcObject CHARACTER    setPageSource   �/      �/       0  I  f      LOGICAL,INPUT phObject HANDLE   setPrimarySdoTarget �/       0      T0  J  t      LOGICAL,INPUT hPrimarySdoTarget HANDLE  setReEnableDataLinks    40      |0      �0  K  �      LOGICAL,INPUT cReEnableDataLinks CHARACTER  setRouterTarget �0      �0      1  L  �      LOGICAL,INPUT phObject HANDLE   setRunDOOptions �0      01      `1  M  �      LOGICAL,INPUT pcOptions CHARACTER   setRunMultiple  @1      �1      �1  N  �      LOGICAL,INPUT plMultiple LOGICAL    setSavedContainerMode   �1      �1      2  O  �      LOGICAL,INPUT cSavedContainerMode CHARACTER setSdoForeignFields �1      <2      p2  P  �      LOGICAL,INPUT cSdoForeignFields CHARACTER   setTopOnly  P2      �2      �2  Q 
 �      LOGICAL,INPUT plTopOnly LOGICAL setUpdateSource �2      �2      3  R        LOGICAL,INPUT pcSource CHARACTER    setUpdateTarget �2      <3      l3  S        LOGICAL,INPUT pcTarget CHARACTER    setWaitForObject    L3      �3      �3  T  !      LOGICAL,INPUT phObject HANDLE   setWindowTitleViewer    �3      �3      4  U  2      LOGICAL,INPUT phViewer HANDLE   getObjectType   �3      <4      l4  V  G      CHARACTER,  setStatusArea   L4      x4      �4  W  U      LOGICAL,INPUT plStatusArea LOGICAL  applyLayout                             \5  D5      ��                  �  �  t5              8��                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            disableObject                               `6  H6      ��                  �  �  x6              ء�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            enableObject                                d7  L7      ��                  �  �  |7              |��                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeObject                                l8  T8      ��                  �  �  �8              X<                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            processAction                               p9  X9      ��                  �  �  �9              h<                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �9           ��                            ����                            getAllFieldHandles  �4      :      <:  X  c      CHARACTER,  getAllFieldNames    :      H:      |:  Y  v      CHARACTER,  getCol  \:      �:      �:  Z  �      DECIMAL,    getDefaultLayout    �:      �:      �:  [  �      CHARACTER,  getDisableOnInit    �:      �:      0;  \  �      LOGICAL,    getEnabledObjFlds   ;      <;      p;  ]  �      CHARACTER,  getEnabledObjHdls   P;      |;      �;  ^  �      CHARACTER,  getHeight   �;      �;      �;  _ 	 �      DECIMAL,    getHideOnInit   �;      �;      $<  `  �      LOGICAL,    getLayoutOptions    <      0<      d<  a  �      CHARACTER,  getLayoutVariable   D<      p<      �<  b  �      CHARACTER,  getObjectEnabled    �<      �<      �<  c        LOGICAL,    getObjectLayout �<      �<       =  d         CHARACTER,  getRow   =      ,=      T=  e  0      DECIMAL,    getWidth    4=      `=      �=  f  7      DECIMAL,    getResizeHorizontal l=      �=      �=  g  @      LOGICAL,    getResizeVertical   �=      �=      >  h  T      LOGICAL,    setAllFieldHandles  �=      >      L>  i  f      LOGICAL,INPUT pcValue CHARACTER setAllFieldNames    ,>      l>      �>  j  y      LOGICAL,INPUT pcValue CHARACTER setDefaultLayout    �>      �>      �>  k  �      LOGICAL,INPUT pcDefault CHARACTER   setDisableOnInit    �>      ?      L?  l  �      LOGICAL,INPUT plDisable LOGICAL setHideOnInit   ,?      l?      �?  m  �      LOGICAL,INPUT plHide LOGICAL    setLayoutOptions    |?      �?      �?  n  �      LOGICAL,INPUT pcOptions CHARACTER   setObjectLayout �?      @      D@  o  �      LOGICAL,INPUT pcLayout CHARACTER    setResizeHorizontal $@      h@      �@  p  �      LOGICAL,INPUT plResizeHorizontal LOGICAL    setResizeVertical   |@      �@      �@  q  �      LOGICAL,INPUT plResizeVertical LOGICAL  getObjectTranslated �@      $A      XA  r  	      LOGICAL,    getObjectSecured    8A      dA      �A  s  	      LOGICAL,    createUiEvents  xA      �A      �A  t  &	      LOGICAL,    bindServer                              pB  XB      ��                  �  �  �B              4Á                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            destroyObject                               tC  \C      ��                  �  �  �C              �Á                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            destroyServerObject                             |D  dD      ��                  �  �  �D              D�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            disconnectObject                                �E  lE      ��                  �  �  �E              ��                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeServerObject                              �F  xF      ��                  �  �  �F              (�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            restartServerObject                             �G  �G      ��                  �  �  �G              ��                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            runServerObject                             �H  �H      ��                  �  �  �H              4�                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
                 �H  
         ��                            ����                            startServerObject                               �I  �I      ��                  �  �  �I              (�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            unbindServer                                �J  �J      ��                  �  �  �J              ��                    O   ����    e�          O   ����    R�          O   ����    ��            ��                   K           ��                            ����                            getAppService   �A      hK      �K  u  5	      CHARACTER,  getASBound  xK      �K      �K  v 
 C	      LOGICAL,    getAsDivision   �K      �K      L  w  N	      CHARACTER,  getASHandle �K      L      DL  x  \	      HANDLE, getASHasStarted $L      LL      |L  y  h	      LOGICAL,    getASInfo   \L      �L      �L  z 	 x	      CHARACTER,  getASInitializeOnRun    �L      �L      �L  {  �	      LOGICAL,    getASUsePrompt  �L      M      4M  |  �	      LOGICAL,    getServerFileName   M      @M      tM  }  �	      CHARACTER,  getServerOperatingMode  TM      �M      �M  ~  �	      CHARACTER,  runServerProcedure  �M      �M      �M    �	      HANDLE,INPUT pcServerFileName CHARACTER,INPUT phAppService HANDLE   setAppService   �M      <N      lN  �  �	      LOGICAL,INPUT pcAppService CHARACTER    setASDivision   LN      �N      �N  �  �	      LOGICAL,INPUT pcDivision CHARACTER  setASHandle �N      �N      O  �  �	      LOGICAL,INPUT phASHandle HANDLE setASInfo   �N      4O      `O  � 	 

      LOGICAL,INPUT pcInfo CHARACTER  setASInitializeOnRun    @O      �O      �O  �  
      LOGICAL,INPUT plInitialize LOGICAL  setASUsePrompt  �O      �O      P  �  )
      LOGICAL,INPUT plFlag LOGICAL    setServerFileName   �O      ,P      `P  �  8
      LOGICAL,INPUT pcFileName CHARACTER  setServerOperatingMode  @P      �P      �P  �  J
      LOGICAL,INPUT pcServerOperatingMode CHARACTER   addLink                             xQ  `Q      ��                  �  �  �Q              �|                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  �Q             �Q  
             ��   R             �Q               �� 
                 �Q  
         ��                            ����                            addMessage                              �R  �R      ��                  �  �  S              ܃                    O   ����    e�          O   ����    R�          O   ����    ��            ��   TS              S               ��   |S             HS               ��                  pS           ��                            ����                            adjustTabOrder                              lT  TT      ��                  �  �  �T              X�                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  �T             �T  
             �� 
  �T             �T  
             ��                  �T           ��                            ����                            applyEntry                              �U  �U      ��                  �  �  �U              ��                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  V           ��                            ����                            changeCursor                                W  �V      ��                  �  �  (W              D�                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  @W           ��                            ����                            createControls                              <X  $X      ��                  �  �  TX              ė                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            destroyObject                               @Y  (Y      ��                  �  �  XY              О                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            displayLinks                                DZ  ,Z      ��                  �  �  \Z              ��                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            editInstanceProperties                              P[  8[      ��                  �  �  h[              @�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            exitObject                              P\  8\      ��                  �  �  h\               �                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            hideObject                              P]  8]      ��                  �  �  h]              ̣                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeObject                                X^  @^      ��                  �  �  p^              Ц                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            modifyListProperty                              `_  H_      ��                  �  �  x_              ȧ                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  �_             �_  
             ��   �_             �_               ��   `             �_               ��                  `           ��                            ����                            modifyUserLinks                             a  �`      ��                  �  �  a              @�                    O   ����    e�          O   ����    R�          O   ����    ��            ��   ha             4a               ��   �a             \a               �� 
                 �a  
         ��                            ����                            removeAllLinks                              �b  hb      ��                  �  �  �b              ��                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            removeLink                              �c  hc      ��                  �    �c              ��                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  �c             �c  
             ��   d             �c               �� 
                  d  
         ��                            ����                            repositionObject                                 e  �d      ��                      e              ط�                    O   ����    e�          O   ����    R�          O   ����    ��            ��   de             0e               ��                  Xe           ��                            ����                            returnFocus                             Pf  8f      ��                    
  hf              Ľ�                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
                 �f  
         ��                            ����                            showMessageProcedure                                �g  lg      ��                      �g              ¿                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �g             �g               ��                  �g           ��                            ����                            toggleData                              �h  �h      ��                      �h              �ǿ                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  i           ��                            ����                            viewObject                              �i  �i      ��                      j              |ȿ                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            anyMessage  �P      lj      �j  � 
 �      LOGICAL,    assignLinkProperty  xj      �j      �j  �  �      LOGICAL,INPUT pcLink CHARACTER,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER   fetchMessages   �j      0k      `k  �  �      CHARACTER,  getChildDataKey @k      lk      �k  �  �      CHARACTER,  getContainerHandle  |k      �k      �k  �  �      HANDLE, getContainerHidden  �k      �k      l  �  �      LOGICAL,    getContainerSource  �k      $l      Xl  �        HANDLE, getContainerSourceEvents    8l      `l      �l  �  $      CHARACTER,  getContainerType    |l      �l      �l  �  =      CHARACTER,  getDataLinksEnabled �l      �l      m  �  N      LOGICAL,    getDataSource   �l      (m      Xm  �  b      HANDLE, getDataSourceEvents 8m      `m      �m  �  p      CHARACTER,  getDataSourceNames  tm      �m      �m  �  �      CHARACTER,  getDataTarget   �m      �m      n  �  �      CHARACTER,  getDataTargetEvents �m      n      Pn  �  �      CHARACTER,  getDBAware  0n      \n      �n  � 
 �      LOGICAL,    getDesignDataObject hn      �n      �n  �  �      CHARACTER,  getDynamicObject    �n      �n      o  �  �      LOGICAL,    getInstanceProperties   �n      o      Lo  �  �      CHARACTER,  getLogicalObjectName    ,o      Xo      �o  �  �      CHARACTER,  getLogicalVersion   po      �o      �o  �        CHARACTER,  getObjectHidden �o      �o      p  �  &      LOGICAL,    getObjectInitialized    �o      p      Pp  �  6      LOGICAL,    getObjectName   0p      \p      �p  �  K      CHARACTER,  getObjectPage   lp      �p      �p  �  Y      INTEGER,    getObjectParent �p      �p      q  �  g      HANDLE, getObjectVersion    �p      q      @q  �  w      CHARACTER,  getObjectVersionNumber   q      Lq      �q  �  �      CHARACTER,  getParentDataKey    dq      �q      �q  �  �      CHARACTER,  getPassThroughLinks �q      �q      r  �  �      CHARACTER,  getPhysicalObjectName   �q      r      Hr  �  �      CHARACTER,  getPhysicalVersion  (r      Tr      �r  �  �      CHARACTER,  getPropertyDialog   hr      �r      �r  �  �      CHARACTER,  getQueryObject  �r      �r      s  �  �      LOGICAL,    getRunAttribute �r      s      @s  �        CHARACTER,  getSupportedLinks    s      Ls      �s  �        CHARACTER,  getTranslatableProperties   `s      �s      �s  �  0      CHARACTER,  getUIBMode  �s      �s       t  � 
 J      CHARACTER,  getUserProperty �s      t      <t  �  U      CHARACTER,INPUT pcPropName CHARACTER    instancePropertyList    t      dt      �t  �  e      CHARACTER,INPUT pcPropList CHARACTER    linkHandles |t      �t      �t  �  z      CHARACTER,INPUT pcLink CHARACTER    linkProperty    �t      u      Du  �  �      CHARACTER,INPUT pcLink CHARACTER,INPUT pcPropName CHARACTER mappedEntry $u      �u      �u  �  �      CHARACTER,INPUT pcEntry CHARACTER,INPUT pcList CHARACTER,INPUT plFirst LOGICAL,INPUT pcDelimiter CHARACTER  messageNumber   �u      v      Hv  �  �      CHARACTER,INPUT piMessage INTEGER   propertyType    (v      lv      �v  �  �      CHARACTER,INPUT pcPropName CHARACTER    reviewMessages  |v      �v      �v  �  �      CHARACTER,  setChildDataKey �v       w      0w  �  �      LOGICAL,INPUT cChildDataKey CHARACTER   setContainerHidden  w      Xw      �w  �  �      LOGICAL,INPUT plHidden LOGICAL  setContainerSource  lw      �w      �w  �  �      LOGICAL,INPUT phObject HANDLE   setContainerSourceEvents    �w       x      <x  �  �      LOGICAL,INPUT pcEvents CHARACTER    setDataLinksEnabled x      `x      �x  �        LOGICAL,INPUT lDataLinksEnabled LOGICAL setDataSource   tx      �x      �x  �  ,      LOGICAL,INPUT phObject HANDLE   setDataSourceEvents �x      y      @y  �  :      LOGICAL,INPUT pcEventsList CHARACTER    setDataSourceNames   y      hy      �y  �  N      LOGICAL,INPUT pcSourceNames CHARACTER   setDataTarget   |y      �y      �y  �  a      LOGICAL,INPUT pcTarget CHARACTER    setDataTargetEvents �y      z      Lz  �  o      LOGICAL,INPUT pcEvents CHARACTER    setDBAware  ,z      pz      �z  � 
 �      LOGICAL,INPUT lAware LOGICAL    setDesignDataObject |z      �z      �z  �  �      LOGICAL,INPUT pcDataObject CHARACTER    setDynamicObject    �z      {      L{  �  �      LOGICAL,INPUT lTemp LOGICAL setInstanceProperties   ,{      h{      �{  �  �      LOGICAL,INPUT pcPropList CHARACTER  setLogicalObjectName    �{      �{      �{  �  �      LOGICAL,INPUT c CHARACTER   setLogicalVersion   �{      |      L|  �  �      LOGICAL,INPUT cVersion CHARACTER    setObjectName   ,|      p|      �|  �  �      LOGICAL,INPUT pcName CHARACTER  setObjectParent �|      �|      �|  �  �      LOGICAL,INPUT phParent HANDLE   setObjectVersion    �|      }      D}  �        LOGICAL,INPUT cObjectVersion CHARACTER  setParentDataKey    $}      l}      �}  �        LOGICAL,INPUT cParentDataKey CHARACTER  setPassThroughLinks �}      �}      �}  �  0      LOGICAL,INPUT pcLinks CHARACTER setPhysicalObjectName   �}      ~      T~  �  D      LOGICAL,INPUT cTemp CHARACTER   setPhysicalVersion  4~      t~      �~  �  Z      LOGICAL,INPUT cVersion CHARACTER    setRunAttribute �~      �~      �~  �  m      LOGICAL,INPUT cRunAttribute CHARACTER   setSupportedLinks   �~      $      X  �  }      LOGICAL,INPUT pcLinkList CHARACTER  setTranslatableProperties   8      |      �  �  �      LOGICAL,INPUT pcPropList CHARACTER  setUIBMode  �      �      �  � 
 �      LOGICAL,INPUT pcMode CHARACTER  setUserProperty �      (�      X�  �  �      LOGICAL,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER  showmessage 8�      ��      Ā  �  �      LOGICAL,INPUT pcMessage CHARACTER   Signature   ��      �      �  � 	 �      CHARACTER,INPUT pcName CHARACTER    �    ,  T�  Ё      4      4   ����4                ��                      ��                  -  Z                  ��                       -  d�        .  ��  x�      D      4   ����D                ��                      ��                  /  Y                  h�                       /  �  ��    F  ��   �      X      4   ����X                0�                      ��                  R  T                  ��                       R  ��         S                                  4     
                    � ߱        ��  $  V  \�  ���                           $  X  ��  ���                       �       	       	           � ߱        �    ^  (�  ��      �      4   �����                ��                      ��                  _  #	                  ���                       _  8�  �  o   b      ,                                 @�  $   c  �  ���                         @         �              � ߱        T�  �   d  $      h�  �   e  �      |�  �   g        ��  �   i  �      ��  �   k  �      ��  �   m  h      ̅  �   n  �      ��  �   o         �  �   r  �      �  �   t        �  �   u  �      0�  �   w   	      D�  �   x  |	      X�  �   y  �	      l�  �   z  4
      ��  �   {  �
      ��  �   �  �
      ��  �   �  X      ��  �   �  �      І  �   �        �  �   �  |      ��  �   �  �      �  �   �  t       �  �   �  �      4�  �   �  d      H�  �   �  �      \�  �   �  L      p�  �   �  �      ��  �   �  �      ��  �   �  8      ��  �   �  �      ��  �   �  �      ԇ  �   �  $      �  �   �  `      ��  �   �  �      �  �   �        $�  �   �  T      8�  �   �  �      L�  �   �  �      `�  �   �        t�  �   �  D      ��  �   �  �      ��  �   �  �      ��  �   �  �          �   �  4                      ܉          H�  0�      ��                  J	  x	  `�              X��                    O   ����    e�          O   ����    R�          O   ����    ��      �     
                        
       
       0                         � ߱        �  $ ^	  x�  ���                           O   v	  ��  ��  p               t�          d�  l�    T�                                             ��                            ����                                <4      Ĉ       �     6     |�                      V x�  G                     ،    �	  4�  ��      |      4   ����|                ��                      ��                  �	  
                  ��                       �	  D�  ԋ  �   �	  �      �  �   �	  P      ��  �   �	  �      �  �   �	  H      $�  �   �	  �      8�  �   �	  @      L�  �   �	  �      `�  �   �	  0      t�  �   �	  �      ��  �   �	  (      ��  �   �	  �      ��  �   �	        Č  �   �	  �          �   �	        ��    *
  �  p�      �      4   �����                ��                      ��                  +
  �
                  l�                       +
  �  ��  �   -
  �      ��  �   .
  T      ��  �   /
  �      Ѝ  �   0
  D      �  �   1
  �      ��  �   2
  ,       �  �   3
  �        �  �   4
  !      4�  �   5
  �!      H�  �   6
  "      \�  �   7
  �"      p�  �   8
  �"      ��  �   9
  h#      ��  �   :
  �#      ��  �   ;
  `$      ��  �   <
  �$      Ԏ  �   =
  X%      �  �   >
  �%      ��  �   ?
  P&      �  �   @
  �&      $�  �   A
  H'      8�  �   B
  �'      L�  �   C
  @(      `�  �   D
  �(      t�  �   E
  8)      ��  �   F
  �)      ��  �   G
  0*          �   H
  �*      ̔    �
  ̏  H�      +      4   ����+                X�                      ��                  �
  w                  �ʁ                       �
  ܏  l�  �   �
  t+      ��  �   �
  �+      ��  �   �
  l,      ��  �   �
  �,      ��  �   �
  T-      А  �   �
  �-      �  �   �
  <.      ��  �   �
  x.      �  �   �
  �.       �  �   �
  (/      4�  �   �
  d/      H�  �   �
  �/      \�  �   �
  L0      p�  �   �
  �0      ��  �   �
  <1      ��  �   �
  �1      ��  �   �
  $2      ��  �   �
  �2      ԑ  �   �
  3      �  �   �
  X3      ��  �   �
  �3      �  �   �
  @4      $�  �   �
  �4      8�  �   �
  �4      L�  �   �
  ,5      `�  �   �
  �5      t�  �   �
  �5      ��  �   �
   6      ��  �   �
  \6      ��  �   �
  �6      Ē  �   �
  �6      ؒ  �   �
  7      �  �   �
  L7       �  �   �
  �7      �  �   �
  �7      (�  �   �
  88      <�  �   �
  t8      P�  �   �
  �8      d�  �   �
  �8      x�  �   �
  (9      ��  �   �
  d9      ��  �   �
  �9      ��  �   �
  L:      ȓ  �   �
  �:      ܓ  �   �
  4;      �  �   �
  �;      �  �   �
  ,<      �  �   �
  �<      ,�  �   �
  $=      @�  �   �
  �=      T�  �      >      h�  �     X>      |�  �     �>      ��  �     ?      ��  �     L?      ��  �     �?          �     �?      $�  $  �  ��  ���                       d@     
                    � ߱        ��    �  @�  P�      x@      4   ����x@      /   �  |�     ��                          3   �����@            ��                      3   �����@  �    �  ؕ  T�  @�  �@      4   �����@  	              d�                      ��             	     �  K                  ���                       �  �  x�  �   �  $A      Ж  $  �  ��  ���                       PA     
                    � ߱        �  �   �  pA      <�  $   �  �  ���                       �A  @         �A              � ߱        ��  $  �  h�  ���                       �A                         � ߱        `B     
                �B       
       
       ,D  @        
 �C              � ߱        ��  V   �  ��  ���                        8D                     lD                     �D                         � ߱        �  $  �  $�  ���                       hE     
                �E       
       
       4G  @        
 �F              � ߱        ��  V   
  ��  ���                        @G     
                �G       
       
       I  @        
 �H              � ߱            V   /  D�  ���                        
              �                      ��             
     M  �                  h��                       M  ԙ   I     
                �I       
       
       �J  @        
 �J          PK  @        
 K          �K  @        
 tK          L  @        
 �K              � ߱            V   b  P�  ���                        adm-clone-props ��  4�              �     7     `                          \                       start-super-proc    D�  ��  �           �     8                                  #                     ��      ,�  <�      �O      4   �����O      /     h�     x�                          3   �����O            ��                      3   �����O   �  $    Ԝ  ���                       �O                         � ߱        ��    -  �  ��  8�  P      4   ����P                �                      ��                  .  2                  ���                       .  ,�   P                     4P                     HP                         � ߱            $  /  ��  ���                             3  T�  ��      `P      4   ����`P  �P                         � ߱            $  4  d�  ���                       ��    ;  ؞  �  @�  �P      4   �����P      $  <  �  ���                       �P                         � ߱            �   Y  �P      Q     
                �Q       
       
       �R  @        
 �R              � ߱        �  V   m  T�  ���                        ��  �   �  �R      ��    "  �  $�       S      4   ���� S      /   #  P�     `�                          3   ����0S            ��                      3   ����PS  L�  $  '  ��  ���                       lS                         � ߱        �S     
                T       
       
       dU  @        
 $U              � ߱        x�  V   1  �  ���                        X�    �  ��  �      pU      4   ����pU                 �                      ��                  �  �                  Dؼ                       �  ��      g   �  8�         ����                            �          Т  ��      ��                  �      �              �ؼ                    O   ����    e�          O   ����    R�          O   ����    ��          /  �  ,�     <�  �U                      3   �����U  l�     
   \�                      3   �����U         
   ��                      3   �����U    ��                              ��        q                  ����                                        L�              9      ��                      g                               `�  g   �  p�          ��	�                           8�          �  �      ��                  �  �   �              ȁ                    O   ����    e�          O   ����    R�          O   ����    ��          /  �  d�     t�  �U                      3   �����U            ��                      3   �����U    ��                              ��        q                  ����                                        ��              :      ��                      g                               h�  g   �  x�          ��	�                           @�          �  ��      ��                  �  �  (�              |ȁ                    O   ����    e�          O   ����    R�          O   ����    ��          /  �  l�     |�  V                      3   �����U            ��                      3   ����V    ��                              ��        q                  ����                                        ��              ;      ��                      g                               Ȭ    �  ��   �      4V      4   ����4V                �                      ��                  �  �                  Ɂ                       �  ��  |�  /   �  <�     L�                          3   ����DV            l�                      3   ����dV  x�  /  �  ��     ��  �V                      3   �����V  �     
   ة                      3   �����V  �        �                      3   �����V  H�        8�                      3   �����V            h�                      3   �����V  ��    �  ��  ��      W      4   ����W      /  �  Ъ     �  �W                      3   ����tW  �     
    �                      3   �����W  @�        0�                      3   �����W  p�        `�                      3   �����W            ��                      3   �����W        �  ��  ̫      �W      4   �����W      /  �  ��     �  PX                      3   ����0X  8�     
   (�                      3   ����XX  h�        X�                      3   ����`X  ��        ��                      3   ����tX            ��                      3   �����X  `�     �  �X                                     �X     
                DY       
       
       �Z  @        
 TZ              � ߱        �  V   i  ��  ���                        �Z     
                $[       
       
       t\  @        
 4\              � ߱        d�  V   �  ��  ���                        �\  @         �\          �\  @         �\              � ߱        ��  $   �  �  ���                       D�  g   �  ��         �6�                            p�          @�  (�      ��                  �  �  X�              hB�                    O   ����    e�          O   ����    R�          O   ����    ��            �  �\  }        ��                              ��        q                  ����                                        ��              <      ��                      g                               ��  g   �  \�         �"��                           $�          ��  ܰ      ��                      �              C�                    O   ����    e�          O   ����    R�          O   ����    ��      |�  $     P�  ���                       ]  @         �\              � ߱        ��      ��  �      ]      4   ����]                $�                      ��                                      �C�                         ��  l�    	  @�  ��  |�   ]      4   ���� ]                ̲                      ��                  	                    ,D�                       	  P�      	  
   �                                        3   ����@]                ��                      ��                                      �D�                         �  �  	    ��                         �]        г  3   ����L]  �  3   �����]      3   �����]  T�  V     �  ���                                                    ߱                            p�  �      �]      4   �����]                ��                      ��                                      0F�                         ��  T�  $     (�  ���                       ^  @         �]              � ߱            O    ������  $^  ��  /     ��                                 3   ����8^          ĵ  @�      T^      4   ����T^                P�                      ��                                      �F�                         Ե  ��  	    ��                                        3   ����l^      O    ������  x^      $     ض  ���                       �^  @         �^              � ߱                      8�                                   �       ��                              ��        q                  ����                            ��          p�  �         =     @�                      g   <�                          L�    ;  �  ��      �^      4   �����^                ��                      ��                  ;  C                  ���                       ;  (�  �  	  <  ظ                                        3   �����^  $�  /   @  �                                 3   ����<_  4�  �   A  T_      O   B  ��  ��  \_  й    F  h�  x�      p_      4   ����p_      $   G  ��  ���                       �_  @         �_              � ߱        x�  /   I  ��                                 3   �����_                ��          ��  ��      ��                 N  R                  d��                (�     N  �      O   N    ��          O   N    ��      ��  /   P  �                                 3   �����_      k   Q  �                    ��        �       /   U  T�                                 3   ����`  adm-create-objects  ��  d�                      >      �                               �                     disable_UI  x�  Ի                      ?      �                               �  
                   enable_UI   �  <�                      @      �             8              �  	                   initializeObject    H�  ��                      A      �                              �                     Reasignar-Orden ��  �                      B      8                                                     �    ���   �  +  Seleccione la PHR destino!!!  ���  �           �  8   ����   ��  8   ����   �  8   ����   �  8   ����       8   ����       8   ����       4�  @�      toggleData  ,INPUT plEnabled LOGICAL    $�  l�  ��      showMessageProcedure    ,INPUT pcMessage CHARACTER,OUTPUT plAnswer LOGICAL  \�  Ⱦ  Ծ      returnFocus ,INPUT hTarget HANDLE   ��  ��  �      repositionObject    ,INPUT pdRow DECIMAL,INPUT pdCol DECIMAL    �  L�  X�      removeLink  ,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE <�  ��  ��      removeAllLinks  ,   ��  п  �      modifyUserLinks ,INPUT pcMod CHARACTER,INPUT pcLinkName CHARACTER,INPUT phObject HANDLE ��  8�  L�      modifyListProperty  ,INPUT phCaller HANDLE,INPUT pcMode CHARACTER,INPUT pcListName CHARACTER,INPUT pcListValue CHARACTER    (�  ��  ��      hideObject  ,   ��  ��  ��      exitObject  ,   ��  �  �      editInstanceProperties  ,   ��  0�  @�      displayLinks    ,    �  T�  d�      createControls  ,   D�  x�  ��      changeCursor    ,INPUT pcCursor CHARACTER   h�  ��  ��      applyEntry  ,INPUT pcField CHARACTER    ��  ��  ��      adjustTabOrder  ,INPUT phObject HANDLE,INPUT phAnchor HANDLE,INPUT pcPosition CHARACTER ��  T�  `�      addMessage  ,INPUT pcText CHARACTER,INPUT pcField CHARACTER,INPUT pcTable CHARACTER D�  ��  ��      addLink ,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE ��  �  $�      unbindServer    ,INPUT pcMode CHARACTER �  L�  `�      startServerObject   ,   <�  t�  ��      runServerObject ,INPUT phAppService HANDLE  d�  ��  ��      restartServerObject ,   ��  ��  ��      initializeServerObject  ,   ��  �  �      disconnectObject    ,   ��  ,�  @�      destroyServerObject ,   �  T�  `�      bindServer  ,   D�  t�  ��      processAction   ,INPUT pcAction CHARACTER   d�  ��  ��      enableObject    ,   ��  ��  ��      disableObject   ,   ��  ��  �      applyLayout ,   ��  �  $�      viewPage    ,INPUT piPageNum INTEGER    �  P�  \�      viewObject  ,   @�  p�  x�      toolbar ,INPUT pcValue CHARACTER    `�  ��  ��      selectPage  ,INPUT piPageNum INTEGER    ��  ��  ��      removePageNTarget   ,INPUT phTarget HANDLE,INPUT piPage INTEGER ��  ,�  8�      passThrough ,INPUT pcLinkName CHARACTER,INPUT pcArgument CHARACTER  �  ��  ��      notifyPage  ,INPUT pcProc CHARACTER p�  ��  ��      initPages   ,INPUT pcPageList CHARACTER ��  ��  �      initializeVisualContainer   ,   ��  �  (�      hidePage    ,INPUT piPageNum INTEGER    �  T�  d�      destroyObject   ,   D�  x�  ��      deletePage  ,INPUT piPageNum INTEGER    h�  ��  ��      createObjects   ,   ��  ��  ��      constructObject ,INPUT pcProcName CHARACTER,INPUT phParent HANDLE,INPUT pcPropList CHARACTER,OUTPUT phObject HANDLE ��  X�  d�      confirmExit ,INPUT-OUTPUT plCancel LOGICAL  H�  ��  ��      changePage  ,   ��  ��  ��      assignPageProperty  ,INPUT pcProp CHARACTER,INPUT pcValue CHARACTER      � 
"     
 �%     adecomm/as-utils.w  
"   
   �    }        �
"     
   %              %              %              %       	       %              %              "      "      "      "      %       	       %                  
�    
�        �     }         �     }        �     }             �     }        �%                  �     }         �     }        �     }             �     }        �%              � 
"    
 � %              � ��  �         �      \     H     $              
�    � �   �      
�             �G� �   �G     
�             �G                      
�            � �     
"    
 �
�H T   %              �     }        �GG %              � 
"   
   P �L 
�H T   %              �     }        �GG %              
"   
   �        �    7%               
"   
 ��               1� �  
 �� �   � %               o%   o           � �    �
"   
 ��           �    1� �   �� �   � %               o%   o           �    �
"   
 ��                1�   
 �� �   � %               o%   o           �    �
"   
 ��           t    1� )   �� �   � %               o%   o           � 7  
 �
"   
 ��           �    1� B   �� �   � %               o%   o           � Q   �
"   
 ��           \    1� h   �� t   � %               o%   o           %               
"   
 � �          �    1� |   � � �     
"   
 ��               1� �   �� �   � %               o%   o           � �  e �
"   
 ��           �    1�    �� �   � %               o%   o           �   ? �
"   
 ��           �    1� [   �� t   � %               o%   o           %               
"   
 ��           x    1� k   �� t   � %               o%   o           %               
"   
 ��           �    1� }   �� t   � %               o%   o           %              
"   
 � �          p	    1� �   � � t     
"   
 ��           �	    1� �  
 �� t   � %               o%   o           %               
"   
 ��           (
    1� �   �� �   � %               o%   o           � �    �
"   
 � �          �
    1� �   � � �     
"   
 ��           �
    1� �   �� �   � %               o%   o           � �  t �
"   
 � �          L    1� G  
 � � �     
"   
 ��           �    1� R   �� �   � %               o%   o           � c  � �
"   
 ��           �    1� �   �� �   � %               o%   o           � �    �
"   
 ��           p    1�   
 ��    � %               o%   o           %               
"   
 ��           �    1�    �� t   � %               o%   o           %               
"   
 ��           h    1�    �� �   � %               o%   o           � �    �
"   
 ��           �    1� /   �� �   � %               o%   o           o%   o           
"   
 ��           X    1� ?  
 �� �   � %               o%   o           � �    �
"   
 ��           �    1� J   �� [  	 � %               o%   o           � e  / �
"   
 � �          @    1� �   � � [  	   
"   
 ��           |    1� �   �� [  	 � o%   o           o%   o           � �    �
"   
 � �          �    1� �   � � [  	   
"   
 ��           ,    1� �   �� [  	 � o%   o           o%   o           � �    �
"   
 � �          �    1� �   � � t     
"   
 � �          �    1� �   � � [  	   
"   
 � �              1� �   � � [  	   
"   
 � �          T    1�    � � [  	   
"   
 ��           �    1�    �� t   � o%   o           o%   o           %              
"   
 � �              1�     � � [  	   
"   
 � �          H    1� .  
 � � 9     
"   
 � �          �    1� A   � � [  	   
"   
 � �          �    1� P   � � [  	   
"   
 � �          �    1� c   � � [  	   
"   
 � �          8    1� x   � � [  	   
"   
 � �          t    1� �  	 � � [  	   
"   
 � �          �    1� �   � � [  	   
"   
 � �          �    1� �   � � [  	   
"   
 ��           (    1� �   �� �   � %               o%   o           o%   o           
�H T   %              �     }        �GG %              
"   
   
"   
 �
"   
   
"   
 �(�  L ( l       �        �    �� �   � P   �        �    �@    
� @  , 
�           �� �     p�               �L
�    %              � 8          � $         � �          
�    � �     
"   
 �� @  , 
�       $    ��   
 �p�               �L"      P �L 
�H T   %              �     }        �GG %              
"   
 ��           �    1� �  
 �� �   � %               o%   o           � �    �
"   
 ��           D    1� �  
 �� �   � %               o%   o           o%   o           
"   
 ��           �    1� 
   �� �   � %               o%   o           o%   o           
"   
 ��           <    1�    �� t   � %               o%   o           %               
"   
 ��           �    1� "   �� t   � %               o%   o           %               
"   
 ��           4    1� /   �� �   � %               o%   o           � �    �
"   
 ��           �    1� 6   �� t   � %               o%   o           %              
"   
 ��           $    1� H   �� t   � %               o%   o           o%   o           
"   
 ��           �    1� T   �� �   � %               o%   o           o%   o           
"   
 ��               1� b  	 �� �   � %               o%   o           � �    �
"   
 ��           �    1� l   �� �   � %               o%   o           o%   o           
"   
 ��               1� �   �� �   � %               o%   o           o%   o           
"   
 ��           �    1� �   �� t   � %               o%   o           %               
"   
 ��               1� �   �� t   � %               o%   o           o%   o           P �L 
�H T   %              �     }        �GG %              
"   
 ��           �    1� �   �� [  	 � %               o%   o           � �    �
"   
 ��           H    1� �   �� [  	 � %               o%   o           � �    �
"   
 ��           �    1� �   �� t   � %               o%   o           %               
"   
 ��           8    1� �   �� [  	 � %               o%   o           � �    �
"   
 ��           �    1� �   �� [  	 � %               o%   o           � �    �
"   
 ��                 1� �   �� t   � %               o%   o           %               
"   
 ��           �     1� �   �� [  	 � %               o%   o           � �    �
"   
 ��           !    1�    �� [  	 � %               o%   o           � �    �
"   
 ��           �!    1�    �� [  	 � %               o%   o           � �    �
"   
 ��           �!    1� +   �� [  	 � %               o%   o           o%   o           
"   
 ��           t"    1� 9   �� [  	 � %               o%   o           � �    �
"   
 ��           �"    1� I   �� [  	 � %               o%   o           � �    �
"   
 ��           \#    1� W  	 �� 9   � %               o%   o           %               
"   
 ��           �#    1� a   �� 9   � %               o%   o           %               
"   
 ��           T$    1� j   �� t   � %               o%   o           o%   o           
"   
 ��           �$    1� {   �� t   � %               o%   o           o%   o           
"   
 ��           L%    1� �   �� t   � %               o%   o           %               
"   
 ��           �%    1� �   �� t   � %               o%   o           %               
"   
 ��           D&    1� �   �� t   � %               o%   o           %               
"   
 ��           �&    1� �   �� �   � %               o%   o           %       
       
"   
 ��           <'    1� �   �� �   � %               o%   o           o%   o           
"   
 ��           �'    1� �   �� �   � %               o%   o           %              
"   
 ��           4(    1� �   �� �   � %               o%   o           o%   o           
"   
 ��           �(    1� �   �� �   � %               o%   o           %              
"   
 ��           ,)    1�    �� �   � %               o%   o           o%   o           
"   
 ��           �)    1�    �� �   � %               o%   o           %              
"   
 ��           $*    1�    �� �   � %               o%   o           o%   o           
"   
 ��           �*    1�     �� [  	 � %               o%   o           � �    �P �L 
�H T   %              �     }        �GG %              
"   
 ��           h+    1� 2   ��    � %               o%   o           %               
"   
 ��           �+    1� >   ��    � %               o%   o           o%   o           
"   
 ��           `,    1� J   �� �   � %               o%   o           � �    �
"   
 ��           �,    1� Z   �� �   � %               o%   o           � p  - �
"   
 ��           H-    1� �   �� �   � %               o%   o           � �    �
"   
 ��           �-    1� �   �� �   � %               o%   o           � �   �
"   
 � �          0.    1� �   � � �     
"   
 ��           l.    1�    �� �   � %               o%   o           � �    �
"   
 � �          �.    1�   
 � � �     
"   
 � �          /    1�    � � �     
"   
 ��           X/    1� %   �� [  	 � %               o%   o           � �    �
"   
 ��           �/    1� 2   �� �   � %               o%   o           � �    �
"   
 ��           @0    1� ?   �� �   � %               o%   o           o%   o           
"   
 ��           �0    1� L   �� �   � %               o%   o           � _  ! �
"   
 ��           01    1� �   �� �   � %               o%   o           � �    �
"   
 ��           �1    1� �   �� �   � %               o%   o           � �   �
"   
 ��           2    1� �  	 ��    � %               o%   o           o%   o           
"   
 ��           �2    1� �   �� t   � %               o%   o           %               
"   
 � �          3    1� �   � � �     
"   
 ��           L3    1� �   �� �   � %               o%   o           � �   �
"   
 ��           �3    1� �   �� [  	 � %               o%   o           � �    �
"   
 ��           44    1�    �� [  	 � %               o%   o           � �    �
"   
 � �          �4    1�    � � �     
"   
 � �          �4    1� &   � � [  	   
"   
 ��            5    1� 9   �� t   � o%   o           o%   o           %               
"   
 � �          �5    1� P   � � t     
"   
 � �          �5    1� g   � � [  	   
"   
 � �          6    1� u   � � [  	   
"   
 � �          P6    1� �   � � [  	   
"   
 � �          �6    1� �   � � [  	   
"   
 � �          �6    1� �   � � [  	   
"   
 � �          7    1� �   � � �     
"   
 ��           @7    1� �   �� �   � %               o%   o           � �  4 �
"   
 � �          �7    1�    � � �     
"   
 � �          �7    1� %   � � �     
"   
 � �          ,8    1� 5   � � �     
"   
 � �          h8    1� B   � � [  	   
"   
 � �          �8    1� V   � � [  	   
"   
 � �          �8    1� h   � � [  	   
"   
 � �          9    1� z   � � t     
"   
 ��           X9    1� �   �� [  	 � %               o%   o           � �    �
"   
 ��           �9    1� �   �� [  	 � %               o%   o           � �    �
"   
 ��           @:    1� �   �� [  	 � %               o%   o           � �    �
"   
 ��           �:    1� �   �� [  	 � %               o%   o           � �    �
"   
 ��           (;    1� �   �� t   � %               o%   o           %               
"   
 ��           �;    1� �   �� t   � %               o%   o           o%   o           
"   
 ��            <    1� �   �� t   � %               o%   o           %               
"   
 ��           �<    1� �   �� t   � %               o%   o           %               
"   
 ��           =    1�    �� t   � %               o%   o           o%   o           
"   
 ��           �=    1� "   �� t   � %               o%   o           %               
"   
 � �          >    1� 0   � � [  	   
"   
 ��           L>    1� >   �� t   � %               o%   o           %              
"   
 � �          �>    1� O   � � [  	   
"   
 � �          ?    1� [   � � [  	   
"   
 � �          @?    1� j  
 � � [  	   
"   
 ��           |?    1� u   �� [  	 � %               o%   o           � �   �
"   
 ��           �?    1� �   �� [  	 � %               o%   o           � �    �
�             �G "  	  � %     start-super-proc � %     adm2/smart.p �P �L 
�H T   %              �     }        �GG %              
"   
   �       A    6� �     
"   
   
�        DA    8
"   
   �        dA    ��     }        �G 4              
"   
 ߱G %              G %              %p e `   LogicalObjectName,PhysicalObjectName,DynamicObject,RunAttribute,HideOnInit,DisableOnInit,ObjectLayout �
�H T   %              �     }        �GG %              
"   
 �
"   
 � 
"   
 �
"   
   (�  L ( l       �        �B    �� �   � P   �        �B    �@    
� @  , 
�       �B    �� �   �p�               �L
�    %              � 8      �B    � $         � �          
�    � �   �
"   
 �p� @  , 
�       �C    �� �   �p�               �L"    , �   � �   �� �   � �     }        �A      |    "      � �   �%              (<   \ (    |    �     }        �A� �   �A"    �    "    �"    �  < "    �"    �(    |    �     }        �A� �   �A"    �
�H T   %              �     }        �GG %              
"   
 �
"   
 � 
"   
 �
"   
   (�  L ( l       �        �E    �� �   � P   �        �E    �@    
� @  , 
�       �E    �� �   �p�               �L
�    %              � 8      �E    � $         � �          
�    � �   �
"   
 �p� @  , 
�       �F    �� �  
 �p�               �L"    , 
�H T   %              �     }        �GG %              
"   
 �
"   
 � 
"   
 �
"   
 �(�  L ( l       �        �G    �� �   � P   �        �G    �@    
� @  , 
�       �G    �� �   �p�               �L
�    %              � 8      �G    � $         � �   �     
�    � �   � 
"   
 �p� @  , 
�       �H    �� |   �p�               �L
�             �G
�H T   %              �     }        �GG %              
"   
   
"   
 �
"   
   
"   
   (�  L ( l       �        lI    �� �   � P   �        xI    �@    
� @  , 
�       �I    �� �     p�               �L
�    %              � 8      �I    � $         � �          
�    � �     
"   
 �p� @  , 
�       �J    ��   
 �p�               �L%     SmartDialog 
"   
   p� @  , 
�       K    �� )     p�               �L% 
    DIALOG-BOX  
"   
  p� @  , 
�       hK    �� �    p�               �L%               
"   
  p� @  , 
�       �K    �� �    p�               �L(        � �      � �      � �      �     }        �A
�H T   %              �     }        �GG %              
"   
 � (   � 
"   
 �    �        �L    �� �   �
"   
   � 8      �L    � $         � �          
�    � �   �
"   
   �        LM    �
"   
   �       lM    /
"   
   
"   
   �       �M    6� �     
"   
   
�        �M    8
"   
   �        �M    �
"   
   �       N    �
"   
   p�    � �   �
�    �     }        �G 4              
"   
 ߱G %              G %              
�     }        �
"   
    (   � 
"   
 �    �        �N    �A"    �A
"   
   
�        O    �@ � 
"   
 �"      �       }        �
"   
 � %              %                "  	  � %     start-super-proc � %     adm2/appserver.p 
��    � r     
�    �     }        �%               %      Server  - �     }        �    "    �� �    � %                   "    �� �    � %      NONE    p�,  8         $     "    �        � �   �
�    
�H T   %              �     }        �GG %              
"   
 �
"   
 � 
"   
 �
"   
   (�  L ( l       �        TQ    �� �   � P   �        `Q    �@    
� @  , 
�       lQ    �� �   �p�               �L
�    %              � 8      xQ    � $         � �          
�    � �   �
"   
 �p� @  , 
�       �R    �� l   �p�               �L"    , p�,  8         $     "    �        � �   �
�     "  	  � %     start-super-proc � %     adm2/visual.p ��   � �     � �     � �     
�H T   %              �     }        �GG %              
"   
 �
"   
 � 
"   
 �
"   
   (�  L ( l       �        �S    �� �   � P   �        �S    �@    
� @  , 
�       �S    �� �   �p�               �L
�    %              � 8      T    � $         � �          
�    � �   �
"   
 �p� @  , 
�       U    �� �   �p�               �L"    , � 
"    
 � %     contextHelp 
"    
   
�    
�    %     processAction   
�    %     CTRL-PAGE-UP �%     processAction   
�    %     CTRL-PAGE-DOWN  "  	  � %     start-super-proc � %     adm2/containr.p %     modifyListProperty  
�    
�    %      Add     %      ContainerSourceEvents �%      initializeDataObjects �0 0   A    �    � "   �
�    � 4   � A    �    � "     
�    � @   � %     modifyListProperty  
�    
�    %      Add     %      ContainerSourceEvents �%     buildDataRequest ent0 A    �    � "   � 
�    � ]   �%     modifyListProperty  
�    
�    %      Add     %     SupportedLinks %      ContainerToolbar-Target %               
�H T   %              �     }        �GG %              
"   
 �
"   
 � 
"   
 �
"   
 �(�  L ( l       �        Y    �� �   � P   �         Y    �@    
� @  , 
�       ,Y    �� �   �p�               �L
�    %              � 8      8Y    � $         � �   �     
�    � �   � 
"   
 �p� @  , 
�       HZ    ��    �p�               �L
�             �G
�H T   %              �     }        �GG %              
"   
 �
"   
 � 
"   
 �
"   
 �(�  L ( l       �        �Z    �� �   � P   �         [    �@    
� @  , 
�       [    �� �   �p�               �L
�    %              � 8      [    � $         � �   �     
�    � �   �
"   
 �p� @  , 
�       (\    �� �   �p�               �L%              �             I%               �             �%              % 	    END-ERROR ��            �%               *        "      "      � �  /    4               � �     "      � �     "      %      
            � �     "      "         "    �%               �            �%              %               %     Reasignar-Orden     �  �   	 ��   )   %               �            �%              �     }        � `     @     ,         � G  (   G %       
       � p  &   G %       
       � �  & � % 
    disable_UI 
�    %                0   � 
�        
�             � 
%   
           
�             � 
�    %     createObjects   %     initializeObject � %     destroyObject   "    �"    �"    �� �   � � �   � "     � "     � &    &    &    &    &    &    d    @            "       &        "       &        "       &        "       &    "    � %      SUPER   �            B           "      � �     "    ��            B           "      � �     "    ��   	   �   	   � �   � �     � "     � "     � "    � "    � &    &    &    &    &    &    �    p    L    0        %              %              %                  "  :    &        "  9    &        "  4    &    "      "     � "     � "    � "    � "    � "    � &    &    &    &    &    &    &    &    &    &    &    &    &    &    &    &    �    �    d    @            "      &        "      &        "     &        "      &        "      &        "      &    "      �                       �           �   l       ��                 Z  ~  �               ܴ�                    O   ����    e�          O   ����    R�          O   ����    ��        $  i  �   ���                       \L     
                    � ߱              j  (  �      �L      4   �����L                �                      ��                  k  }                  L�                       k  8  �  �  l   M            n  �  `      XM      4   ����XM                p                      ��                  o  |                  ��                       o  �  �  o   p      ,                                 �  �   q  xM      �  �   r  �M      $  $  s  �  ���                       �M     
                    � ߱        8  �   t  �M      L  �   u  N      `  �   x  0N          $   {  �  ���                       `N  @         LN              � ߱                     T          ,  @   T �            
             
             
             
                 $   4   D          $   4   D   ����        ��                            ����                                            �           �   l       ��                 �  �  �                �                    O   ����    e�          O   ����    R�          O   ����    ��                            �          �  $  �    ���                       �N     
                    � ߱                  �  �                      ��                   �  �                  ���                     �  4      4   �����N      $  �  �  ���                        O     
                    � ߱        �    �  4  D      4O      4   ����4O      /  �  p                               3   ����HO  �  �   �  TO          O   �  ��  ��  �O                               , �                          
                               �      ��                            ����                                                        �   l       ��                  _  f  �               $��                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                                            �           �   l       ��                  l  w  �               줂                    O   ����    e�          O   ����    R�          O   ����    ��             v  �� �                   ��                              ��        q                  ����                                            �           �   l       ��                  }  �  �               �J�                    O   ����    e�          O   ����    R�          O   ����    ��      (`  �           4`  �          @`  �              � ߱        `  Z   �  �    �                            �              �              �              � ߱        �  h   �      �                        �  
   �  �� �                    s   �  �       �                        X  8                               7   ����           ��                �`   ��          �                  6   �         �   ��               �`   ��          �                                                                $             |`  �`  �`           �`  �`  �`                      �            J   �        ���    ��                                                         4a                      �                 L`   X`   d`   p`        ��                              ��        q                  ����                            L        2                 �c                    �           �   l       ��                  �  �  �               ��                    O   ����    e�          O   ����    R�          O   ����    ��      �   /   �  �                                 3   ����@a  H  $   �    ���                       ha  @         Ta              � ߱            $   �  t  ���                       �a  @         �a              � ߱          ��                              ��        q                  ����                                                       �   l       ��                 �  �  �               ���                    O   ����    e�          O   ����    R�          O   ����    ��                    �              0      ��                �  �  H              ��                      �  �       O   �     ��  �a      O   �     ��  �a        �      �          |  d      ��      @          �  �  �              ���                p     �  `        X       ��                            7   ����        ��               tb    �            �                  6   �       �   ��         �  tb    �            �                                                        �a   b   b    b   ,b   8b                   P  D           Db  Tb  db           Lb  \b  lb                         ,        O   �    ��          O   ����  R�          O   ����  ��      ,c       3       3           � ߱            V   �  �  ���                              �      �          x  `      ��      @          �  �  �              x�                       �        �  �       ��                            7   ����
       ����                d    �            L                  6   �  
     �  ����         p   d    �            L                                                        8c   Dc   Pc   \c   hc   tc                   @  (           �c  �c  �c  �c           �c  �c  �c  �c           �c  �c  �c  �c           �c  �c  �c  �c         � �     �   	 
        �   �  	 �  
         O   �    ��          O   ����  R�          O   ����  ��      �d                         � ߱            V   �  �  ���                          8  �        8  �         O   �  ��  ��  �d    ��                             ��                             ��                            ����                                �"          L  �
   ��                              
 �                                                                 2  8     V  
     /                                     
 �                                                                2  G     a       K?                                     
 �                                                                2  U     f         N                                     
 �                                                                2  e     l         \                                       �                                                                                                                                       L    d d     �   ��"  �"  � �       y  �                                  q   �
                                                         
 $ d     D                                                                
 X  �� �d          d   x           
                              �     �      H  �b�"                                 L          �           P   �fd                                                           l   G   
 X  ��d         �   �           
                                   �     
 X  ��Td         �   �           
                   
           &     �      \  4��s                                 �                  r                 B      \  ���s                                 �                  y                 A      P ��k�s                      
                              �        D                                                                                                        TXS appSrvUtils pCodPHR pNroPHR pCodOrden pNroOrden ASSIGNFOCUSEDWIDGET ASSIGNWIDGETVALUE ASSIGNWIDGETVALUELIST BLANKWIDGET CLEARWIDGET DISABLERADIOBUTTON DISABLEWIDGET ENABLERADIOBUTTON ENABLEWIDGET FORMATTEDWIDGETVALUE FORMATTEDWIDGETVALUELIST HIDEWIDGET HIGHLIGHTWIDGET RESETWIDGETVALUE TOGGLEWIDGET VIEWWIDGET WIDGETHANDLE WIDGETLONGCHARVALUE WIDGETISBLANK WIDGETISFOCUSED WIDGETISMODIFIED WIDGETISTRUE WIDGETVALUE WIDGETVALUELIST s-codcia s-coddiv x-vtacdocu VtaCDocu x-msg Btn_Cancel Btn_OK FILL-IN-1 Seleccione la PHR destino!!! FILL-IN-orden FILL-IN-phr DI-RutaC Cabecera de ruta BROWSE-18 99/99/9999 x(3) X(15) x(6) gDialog Reasignar Ordenes a una nueva PHR X(256) X(20) Origen de la Orden DISABLEPAGESINFOLDER ENABLEPAGESINFOLDER GETCALLERPROCEDURE GETCALLERWINDOW GETCONTAINERMODE GETCONTAINERTARGET GETCONTAINERTARGETEVENTS GETCURRENTPAGE GETDISABLEDADDMODETABS GETDYNAMICSDOPROCEDURE GETFILTERSOURCE GETMULTIINSTANCEACTIVATED GETMULTIINSTANCESUPPORTED GETNAVIGATIONSOURCE GETNAVIGATIONSOURCEEVENTS GETNAVIGATIONTARGET GETOUTMESSAGETARGET GETPAGENTARGET GETPAGESOURCE GETPRIMARYSDOTARGET GETREENABLEDATALINKS GETRUNDOOPTIONS GETRUNMULTIPLE GETSAVEDCONTAINERMODE GETSDOFOREIGNFIELDS GETTOPONLY GETUPDATESOURCE GETUPDATETARGET GETWAITFOROBJECT GETWINDOWTITLEVIEWER GETSTATUSAREA PAGENTARGETS SETCALLEROBJECT SETCALLERPROCEDURE SETCALLERWINDOW SETCONTAINERMODE SETCONTAINERTARGET SETCURRENTPAGE SETDISABLEDADDMODETABS SETDYNAMICSDOPROCEDURE SETFILTERSOURCE SETINMESSAGETARGET SETMULTIINSTANCEACTIVATED SETMULTIINSTANCESUPPORTED SETNAVIGATIONSOURCE SETNAVIGATIONSOURCEEVENTS SETNAVIGATIONTARGET SETOUTMESSAGETARGET SETPAGENTARGET SETPAGESOURCE SETPRIMARYSDOTARGET SETREENABLEDATALINKS SETROUTERTARGET SETRUNDOOPTIONS SETRUNMULTIPLE SETSAVEDCONTAINERMODE SETSDOFOREIGNFIELDS SETTOPONLY SETUPDATESOURCE SETUPDATETARGET SETWAITFOROBJECT SETWINDOWTITLEVIEWER GETOBJECTTYPE SETSTATUSAREA GETALLFIELDHANDLES GETALLFIELDNAMES GETCOL GETDEFAULTLAYOUT GETDISABLEONINIT GETENABLEDOBJFLDS GETENABLEDOBJHDLS GETHEIGHT GETHIDEONINIT GETLAYOUTOPTIONS GETLAYOUTVARIABLE GETOBJECTENABLED GETOBJECTLAYOUT GETROW GETWIDTH GETRESIZEHORIZONTAL GETRESIZEVERTICAL SETALLFIELDHANDLES SETALLFIELDNAMES SETDEFAULTLAYOUT SETDISABLEONINIT SETHIDEONINIT SETLAYOUTOPTIONS SETOBJECTLAYOUT SETRESIZEHORIZONTAL SETRESIZEVERTICAL GETOBJECTTRANSLATED GETOBJECTSECURED CREATEUIEVENTS GETAPPSERVICE GETASBOUND GETASDIVISION GETASHANDLE GETASHASSTARTED GETASINFO GETASINITIALIZEONRUN GETASUSEPROMPT GETSERVERFILENAME GETSERVEROPERATINGMODE RUNSERVERPROCEDURE SETAPPSERVICE SETASDIVISION SETASHANDLE SETASINFO SETASINITIALIZEONRUN SETASUSEPROMPT SETSERVERFILENAME SETSERVEROPERATINGMODE gshAstraAppserver gshSessionManager gshRIManager gshSecurityManager gshProfileManager gshRepositoryManager gshTranslationManager gshWebManager gscSessionId gsdSessionObj gshFinManager gshGenManager gshAgnManager gsdTempUniqueID gsdUserObj gsdRenderTypeObj gsdSessionScopeObj ghProp ghADMProps ghADMPropsBuf glADMLoadFromRepos glADMOk ANYMESSAGE ASSIGNLINKPROPERTY FETCHMESSAGES GETCHILDDATAKEY GETCONTAINERHANDLE GETCONTAINERHIDDEN GETCONTAINERSOURCE GETCONTAINERSOURCEEVENTS GETCONTAINERTYPE GETDATALINKSENABLED GETDATASOURCE GETDATASOURCEEVENTS GETDATASOURCENAMES GETDATATARGET GETDATATARGETEVENTS GETDBAWARE GETDESIGNDATAOBJECT GETDYNAMICOBJECT GETINSTANCEPROPERTIES GETLOGICALOBJECTNAME GETLOGICALVERSION GETOBJECTHIDDEN GETOBJECTINITIALIZED GETOBJECTNAME GETOBJECTPAGE GETOBJECTPARENT GETOBJECTVERSION GETOBJECTVERSIONNUMBER GETPARENTDATAKEY GETPASSTHROUGHLINKS GETPHYSICALOBJECTNAME GETPHYSICALVERSION GETPROPERTYDIALOG GETQUERYOBJECT GETRUNATTRIBUTE GETSUPPORTEDLINKS GETTRANSLATABLEPROPERTIES GETUIBMODE GETUSERPROPERTY INSTANCEPROPERTYLIST LINKHANDLES LINKPROPERTY MAPPEDENTRY MESSAGENUMBER PROPERTYTYPE REVIEWMESSAGES SETCHILDDATAKEY SETCONTAINERHIDDEN SETCONTAINERSOURCE SETCONTAINERSOURCEEVENTS SETDATALINKSENABLED SETDATASOURCE SETDATASOURCEEVENTS SETDATASOURCENAMES SETDATATARGET SETDATATARGETEVENTS SETDBAWARE SETDESIGNDATAOBJECT SETDYNAMICOBJECT SETINSTANCEPROPERTIES SETLOGICALOBJECTNAME SETLOGICALVERSION SETOBJECTNAME SETOBJECTPARENT SETOBJECTVERSION SETPARENTDATAKEY SETPASSTHROUGHLINKS SETPHYSICALOBJECTNAME SETPHYSICALVERSION SETRUNATTRIBUTE SETSUPPORTEDLINKS SETTRANSLATABLEPROPERTIES SETUIBMODE SETUSERPROPERTY SHOWMESSAGE SIGNATURE , prepareInstance ObjectName CHAR  ObjectVersion ADM2.2 ObjectType SmartDialog ContainerType DIALOG-BOX PropertyDialog adm2/support/visuald.w QueryObject LOGICAL ContainerHandle HANDLE InstanceProperties LogicalObjectName,PhysicalObjectName,DynamicObject,RunAttribute,HideOnInit,DisableOnInit,ObjectLayout SupportedLinks Data-Target,Data-Source,Page-Target,Update-Source,Update-Target ContainerHidden ObjectInitialized ObjectHidden ObjectsCreated HideOnInit UIBMode ContainerSource ContainerSourceEvents initializeObject,hideObject,viewObject,destroyObject,enableObject,confirmExit,confirmCancel,confirmOk,isUpdateActive DataSource DataSourceEvents dataAvailable,queryPosition,updateState,deleteComplete,fetchDataSet,confirmContinue,confirmCommit,confirmUndo,assignMaxGuess,isUpdatePending TranslatableProperties ObjectPage INT DBAware DesignDataObject DataSourceNames DataTarget DataTargetEvents CHARACTER updateState,rowObjectState,fetchBatch,LinkState LogicalObjectName PhysicalObjectName LogicalVersion PhysicalVersion DynamicObject RunAttribute ChildDataKey ParentDataKey DataLinksEnabled InactiveLinks InstanceId DECIMAL SuperProcedure SuperProcedureMode SuperProcedureHandle LayoutPosition ClassName RenderingProcedure ThinRenderingProcedure Label cType ADMProps Target WHERE Target = WIDGET-H(" ") AppService ASDivision ASHandle ASHasConnected ASHasStarted ASInfo ASInitializeOnRun ASUsePrompt BindSignature BindScope ServerOperatingMode ServerFileName ServerFirstCall NeedContext ObjectLayout LayoutOptions ObjectEnabled LayoutVariable DefaultLayout DisableOnInit EnabledObjFlds EnabledObjHdls FieldSecurity SecuredTokens AllFieldHandles AllFieldNames MinHeight MinWidth ResizeHorizontal ResizeVertical ObjectSecured ObjectTranslated PopupButtonsInFields ColorInfoBG INTEGER ColorInfoFG ColorWarnBG ColorWarnFG ColorErrorBG ColorErrorFG BGColor FGColor FieldPopupMapping CurrentPage PendingPage ContainerTarget ContainerTargetEvents exitObject,okObject,cancelObject,updateActive ContainerToolbarSource ContainerToolbarSourceEvents toolbar,okObject,cancelObject OutMessageTarget PageNTarget PageSource FilterSource UpdateSource UpdateTarget CommitSource CommitSourceEvents commitTransaction,undoTransaction CommitTarget CommitTargetEvents rowObjectState StartPage RunMultiple WaitForObject DynamicSDOProcedure adm2/dyndata.w RunDOOptions InitialPageList WindowFrameHandle Page0LayoutManager MultiInstanceSupported MultiInstanceActivated ContainerMode SavedContainerMode SdoForeignFields NavigationSource NavigationTarget PrimarySdoTarget NavigationSourceEvents fetchFirst,fetchNext,fetchPrev,fetchLast,startFilter CallerWindow CallerProcedure CallerObject DisabledAddModeTabs ReEnableDataLinks WindowTitleViewer UpdateActive InstanceNames ClientNames ContainedDataObjects ContainedAppServices DataContainer HasDbAwareObjects HasDynamicProxy HideOnClose HideChildContainersOnClose HasObjectMenu RequiredPages RemoveMenuOnHide ProcessList PageLayoutInfo PageTokens DataContainerName WidgetIDFileName ghContainer adm2/smart.p cObjectName iStart / \ . hReposBuffer hPropTable hBuffer hTable deleteProperties ADM-CLONE-PROPS pcProcName hProc START-SUPER-PROC cAppService cASDivision cServerOperatingMode adm2/appserver.p getAppService Server NONE setASDivision setAppService cFields adm2/visual.p   BROWSE-18 Btn_Cancel Btn_OK CTRL-PAGE-UP CTRL-PAGE-DOWN adm2/containr.p Add initializeDataObjects getSupportedLinks data-target data-source buildDataRequest containertoolbar-target ContainerToolbar-Target END-ERROR x-rowid Las PHR deben ser difentes..Imposible reasignar rpta Seguro de reasignar la Orden  - hacia la nueva PHR  ADM-ERROR NO se complet� el proceso de reasignaci�n iStartPage A SmartDialog is not intended to be run  Persistent or to be placed in another  SmartObject at AppBuilder design time. ADM-CREATE-OBJECTS DISABLE_UI PHR P ENABLE_UI INITIALIZEOBJECT HPK A DI-RutaD Detalle de ruta OK REASIGNAR-ORDEN Generada FchDoc Cod.Doc CodDoc Numero NroDoc Division CodDiv Orden Cancel Reasignar llave01 Llave05 Llave02    H      �$      % �8   ��      0         pcProp      ��      P         pcProp      ��      p         plCancel    �   ��      �         pcProcName  �   ��      �        
 pcProcName  �   ��      �         pcProcName      ��              
 pcProcName      ��      $        piPageNum       ��      H        piPageNum       ��      l        pcPageList      ��      �        pcProc  �  ��      �        pcLinkName      ��      �        pcLinkName    ��      �       
 phTarget        ��              phTarget        ��      @        piPageNum       ��      d        pcValue     ��      �        piPageNum       ��      �        pcAction        ��      �       
 phAppService        ��      �        pcMode     ��             
 phSource    D  ��      8        phSource        ��      \       
 phSource    �  ��      �        pcText  �  ��      �        pcText      ��      �        pcText  �  ��      �       
 phObject      ��             
 phObject        ��      (        phObject        ��      L        pcField     ��      l        pcCursor    �  ��      �       
 phCaller    �  ��      �        phCaller    �  ��      �        phCaller        ��      �        phCaller    (  ��               pcMod   H  ��      @        pcMod       ��      `       
 pcMod   �  ��      �       
 phSource    �  ��      �        phSource        ��      �       
 phSource    �  ��      �        pdRow       ��              pdRow       ��      ,       
 hTarget X  ��      L        pcMessage       ��      p        pcMessage       ��      �        plEnabled             �     cType       �     6   �          �                  getObjectType   ^	  v	  x	  ,          
   hReposBuffer    L        @  
   hPropTable  h        `  
   hBuffer           |  
   hTable  �  �     7             �                  adm-clone-props i  j  k  l  n  o  p  q  r  s  t  u  x  {  |  }  ~              
   hProc             <        pcProcName  �  �  	   8     $      x                  start-super-proc    �  �  �  �  �  �  �  �  �  H  �     9                                   �  �  	     :                                   �  �  �  L	     ;                                   �  �  	  �	     <                                   �  �  �	        �	     x-rowid �	        �	     x-msg             �	     rpta    T	  
     =   �	                                  	  
                                  �	  �
     >               �
                  adm-create-objects  f  `
  �
     ?               �
                  disable_UI  v  w  �
  (     @                                 enable_UI   �  �  �  �  �  �
  �     A               l                  initializeObject    �  �  �  �  <  �     B               �                  Reasignar-Orden �  �  �  �  �  �  �  �  �  �  �  �  �    �      0  @  �                      P          D  
   appSrvUtils p        d     s-codcia    �        �     s-coddiv    �       �     x-msg   �       �     FILL-IN-1   �       �     FILL-IN-orden               FILL-IN-phr 8        $  
   gshAstraAppserver   `        L  
   gshSessionManager   �  	 	     t  
   gshRIManager    �  
 
     �  
   gshSecurityManager  �        �  
   gshProfileManager            �  
   gshRepositoryManager    ,          
   gshTranslationManager   P        @  
   gshWebManager   t        d     gscSessionId    �        �     gsdSessionObj   �        �  
   gshFinManager   �        �  
   gshGenManager           �  
   gshAgnManager   (             gsdTempUniqueID H        <     gsdUserObj  p        \     gsdRenderTypeObj    �        �     gsdSessionScopeObj  �       �  
   ghProp  �       �  
   ghADMProps  �       �  
   ghADMPropsBuf        	        glADMLoadFromRepos  <    
   4     glADMOk \       P  
   ghContainer |       p     cObjectName �       �     iStart  �       �     cAppService �       �     cASDivision        �     cServerOperatingMode                 cFields          4     iStartPage  `       X        pCodPHR �       x        pNroPHR �       �        pCodOrden            �        pNroOrden   �     C  �  x-vtacdocu          �  DI-RutaC               DI-RutaD             <   �  �  �  �  �  �  �  ,  -  .  /  F  R  S  T  V  X  Y  Z  ^  _  b  c  d  e  g  i  k  m  n  o  r  t  u  w  x  y  z  {  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  #	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  
  *
  +
  -
  .
  /
  0
  1
  2
  3
  4
  5
  6
  7
  8
  9
  :
  ;
  <
  =
  >
  ?
  @
  A
  B
  C
  D
  E
  F
  G
  H
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
  �
  �
                 w  �  �  �  �  �  �  �  �  �  �  �  �  
  /  K  M  b  �        -  .  /  2  3  4  ;  <  Y  m  �  "  #  '  1  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  i  �  �  �  �  ;  <  @  A  B  C  F  G  I  N  P  Q  R  U      �� $ C:\Progress\OpenEdge\src\adm2\dialogmn.i   f!  C:\Progress\OpenEdge\src\adm2\containr.i @  � # %C:\Progress\OpenEdge\src\adm2\custom\containrcustom.i    t  ��  C:\Progress\OpenEdge\src\adm2\visual.i   �  # " %C:\Progress\OpenEdge\src\adm2\custom\visualcustom.i  �  �<  C:\Progress\OpenEdge\src\adm2\appserver.i    ,  �� ! %C:\Progress\OpenEdge\src\adm2\custom\appservercustom.i   d  I�  C:\Progress\OpenEdge\src\adm2\smart.i    �  Ds   C:\Progress\OpenEdge\gui\fn  �  tw  %C:\Progress\OpenEdge\src\adm2\custom\smartcustom.i     Q.  C:\Progress\OpenEdge\gui\set D  ��  C:\Progress\OpenEdge\src\adm2\cntnprop.i l  ��  %C:\Progress\OpenEdge\src\adm2\custom\cntnpropcustom.i    �  P  %C:\Progress\OpenEdge\src\adm2\custom\cntnprtocustom.i    �  F>  C:\Progress\OpenEdge\src\adm2\visprop.i  (  �I  %C:\Progress\OpenEdge\src\adm2\custom\vispropcustom.i \  ��  %C:\Progress\OpenEdge\src\adm2\custom\visprtocustom.i �  �l 
 C:\Progress\OpenEdge\src\adm2\appsprop.i �  ɏ  %C:\Progress\OpenEdge\src\adm2\custom\appspropcustom.i      V  %C:\Progress\OpenEdge\src\adm2\custom\appsprtocustom.i    T  i$  C:\Progress\OpenEdge\src\adm2\smrtprop.i �  �j  C:\Progress\OpenEdge\gui\get �  �  %C:\Progress\OpenEdge\src\adm2\custom\smrtpropcustom.i    �  ��  %C:\Progress\OpenEdge\src\adm2\custom\smrtprtocustom.i    8  ��  C:\Progress\OpenEdge\src\adm2\smrtprto.i |  Su  C:\Progress\OpenEdge\src\adm2\globals.i  �  M�  %C:\Progress\OpenEdge\src\adm2\custom\globalscustom.i �  )a  %C:\Progress\OpenEdge\src\adm2\custom\smartdefscustom.i   $  �  C:\Progress\OpenEdge\src\adm2\appsprto.i h  ��  %C:\Progress\OpenEdge\src\adm2\custom\appserverdefscustom.i   �  �X 	 C:\Progress\OpenEdge\src\adm2\visprto.i  �  !�  %C:\Progress\OpenEdge\src\adm2\custom\visualdefscustom.i    n�  C:\Progress\OpenEdge\src\adm2\cntnprto.i \  ;  %C:\Progress\OpenEdge\src\adm2\custom\containrdefscustom.i    �  ~�  C:\Progress\OpenEdge\src\adm2\widgetprto.i   �  e�  !C:\Progress\OpenEdge\gui\adecomm\appserv.i     ��    D:\newsie\on_in_co\aplic\logis\d-reasignar-orden.w       C  W      �     -  $   �  �   �      �  �   �     �     �     �  �   �     �     a     �  �   Y     �        #     �   �          �      (  �   �     8     �      H  �   �     X     �      h  r   �     x  n   �     �     P  "   �  i   K     �     )     �  P        �  �        �     �  !   �  �   �     �     �        �   �           e     (   �   c     8      A     H   g   '     X           h   O   �     x   �   z     �      x      �   �   H     �      �     �   �   �     �      �     �   �   �     �      �     �   �   �     !     }     !  �   |     (!     Z     8!  �   I     H!     '     X!  �   $     h!          x!  }   �     �!     �     �!     X     �!     
     �!     �     �!  7   �     �!  �   w     �!  O   i     �!     X     "     
     "  �   �
     ("  �   �
     8"  O   �
     H"     �
     X"     L
     h"  �   '
     x"  x   
  
   �"  M   

     �"     �	     �"     �	     �"  a   �	  
   �"  �  u	     �"     V	     �"  �  #	     �"  O   	     #     	     #     �     (#  �   �     8#     �     H#          X#  x        h#     �     x#     q     �#     m     �#     Y     �#     @     �#  Q   0  
   �#     �     �#     �  
   �#     �     �#     p  
   $  f   E     $     �  	   ($  "   �     8$     �     H$     k     X$  Z        h$     "     x$     �     �$     �     �$     �     �$          �$  0   �       �$     I      �$  	   "       �$     	      
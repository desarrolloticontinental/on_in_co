	��V֫%N�4  � �                                              `� 34AC010Autf-8 MAIN O:\on_st_co\APLIC\VTA\w-corrfchfac.w,, PROCEDURE exitObject,, PROCEDURE enable_UI,, PROCEDURE disable_UI,, PROCEDURE adm-create-objects,, PROCEDURE start-super-proc,,INPUT pcProcName CHARACTER PROCEDURE adm-clone-props,, PROCEDURE toggleData,,INPUT plEnabled LOGICAL PROCEDURE showMessageProcedure,,INPUT pcMessage CHARACTER,OUTPUT plAnswer LOGICAL PROCEDURE returnFocus,,INPUT hTarget HANDLE PROCEDURE repositionObject,,INPUT pdRow DECIMAL,INPUT pdCol DECIMAL PROCEDURE removeLink,,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE PROCEDURE removeAllLinks,, PROCEDURE modifyUserLinks,,INPUT pcMod CHARACTER,INPUT pcLinkName CHARACTER,INPUT phObject HANDLE PROCEDURE modifyListProperty,,INPUT phCaller HANDLE,INPUT pcMode CHARACTER,INPUT pcListName CHARACTER,INPUT pcListValue CHARACTER PROCEDURE hideObject,, PROCEDURE editInstanceProperties,, PROCEDURE displayLinks,, PROCEDURE createControls,, PROCEDURE changeCursor,,INPUT pcCursor CHARACTER PROCEDURE applyEntry,,INPUT pcField CHARACTER PROCEDURE adjustTabOrder,,INPUT phObject HANDLE,INPUT phAnchor HANDLE,INPUT pcPosition CHARACTER PROCEDURE addMessage,,INPUT pcText CHARACTER,INPUT pcField CHARACTER,INPUT pcTable CHARACTER PROCEDURE addLink,,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE PROCEDURE unbindServer,,INPUT pcMode CHARACTER PROCEDURE startServerObject,, PROCEDURE runServerObject,,INPUT phAppService HANDLE PROCEDURE restartServerObject,, PROCEDURE initializeServerObject,, PROCEDURE disconnectObject,, PROCEDURE destroyServerObject,, PROCEDURE bindServer,, PROCEDURE processAction,,INPUT pcAction CHARACTER PROCEDURE enableObject,, PROCEDURE disableObject,, PROCEDURE applyLayout,, PROCEDURE viewPage,,INPUT piPageNum INTEGER PROCEDURE viewObject,, PROCEDURE toolbar,,INPUT pcValue CHARACTER PROCEDURE selectPage,,INPUT piPageNum INTEGER PROCEDURE removePageNTarget,,INPUT phTarget HANDLE,INPUT piPage INTEGER PROCEDURE passThrough,,INPUT pcLinkName CHARACTER,INPUT pcArgument CHARACTER PROCEDURE notifyPage,,INPUT pcProc CHARACTER PROCEDURE initPages,,INPUT pcPageList CHARACTER PROCEDURE initializeVisualContainer,, PROCEDURE initializeObject,, PROCEDURE hidePage,,INPUT piPageNum INTEGER PROCEDURE destroyObject,, PROCEDURE deletePage,,INPUT piPageNum INTEGER PROCEDURE createObjects,, PROCEDURE constructObject,,INPUT pcProcName CHARACTER,INPUT phParent HANDLE,INPUT pcPropList CHARACTER,OUTPUT phObject HANDLE PROCEDURE confirmExit,,INPUT-OUTPUT plCancel LOGICAL PROCEDURE changePage,, PROCEDURE assignPageProperty,,INPUT pcProp CHARACTER,INPUT pcValue CHARACTER FUNCTION Signature,CHARACTER,INPUT pcName CHARACTER FUNCTION showmessage,LOGICAL,INPUT pcMessage CHARACTER FUNCTION setUserProperty,LOGICAL,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER FUNCTION setUIBMode,LOGICAL,INPUT pcMode CHARACTER FUNCTION setTranslatableProperties,LOGICAL,INPUT pcPropList CHARACTER FUNCTION setSupportedLinks,LOGICAL,INPUT pcLinkList CHARACTER FUNCTION setRunAttribute,LOGICAL,INPUT cRunAttribute CHARACTER FUNCTION setPhysicalVersion,LOGICAL,INPUT cVersion CHARACTER FUNCTION setPhysicalObjectName,LOGICAL,INPUT cTemp CHARACTER FUNCTION setPassThroughLinks,LOGICAL,INPUT pcLinks CHARACTER FUNCTION setParentDataKey,LOGICAL,INPUT cParentDataKey CHARACTER FUNCTION setObjectVersion,LOGICAL,INPUT cObjectVersion CHARACTER FUNCTION setObjectParent,LOGICAL,INPUT phParent HANDLE FUNCTION setObjectName,LOGICAL,INPUT pcName CHARACTER FUNCTION setLogicalVersion,LOGICAL,INPUT cVersion CHARACTER FUNCTION setLogicalObjectName,LOGICAL,INPUT c CHARACTER FUNCTION setInstanceProperties,LOGICAL,INPUT pcPropList CHARACTER FUNCTION setDynamicObject,LOGICAL,INPUT lTemp LOGICAL FUNCTION setDesignDataObject,LOGICAL,INPUT pcDataObject CHARACTER FUNCTION setDBAware,LOGICAL,INPUT lAware LOGICAL FUNCTION setDataTargetEvents,LOGICAL,INPUT pcEvents CHARACTER FUNCTION setDataTarget,LOGICAL,INPUT pcTarget CHARACTER FUNCTION setDataSourceNames,LOGICAL,INPUT pcSourceNames CHARACTER FUNCTION setDataSourceEvents,LOGICAL,INPUT pcEventsList CHARACTER FUNCTION setDataSource,LOGICAL,INPUT phObject HANDLE FUNCTION setDataLinksEnabled,LOGICAL,INPUT lDataLinksEnabled LOGICAL FUNCTION setContainerSourceEvents,LOGICAL,INPUT pcEvents CHARACTER FUNCTION setContainerSource,LOGICAL,INPUT phObject HANDLE FUNCTION setContainerHidden,LOGICAL,INPUT plHidden LOGICAL FUNCTION setChildDataKey,LOGICAL,INPUT cChildDataKey CHARACTER FUNCTION reviewMessages,CHARACTER, FUNCTION propertyType,CHARACTER,INPUT pcPropName CHARACTER FUNCTION messageNumber,CHARACTER,INPUT piMessage INTEGER FUNCTION mappedEntry,CHARACTER,INPUT pcEntry CHARACTER,INPUT pcList CHARACTER,INPUT plFirst LOGICAL,INPUT pcDelimiter CHARACTER FUNCTION linkProperty,CHARACTER,INPUT pcLink CHARACTER,INPUT pcPropName CHARACTER FUNCTION linkHandles,CHARACTER,INPUT pcLink CHARACTER FUNCTION instancePropertyList,CHARACTER,INPUT pcPropList CHARACTER FUNCTION getUserProperty,CHARACTER,INPUT pcPropName CHARACTER FUNCTION getUIBMode,CHARACTER, FUNCTION getTranslatableProperties,CHARACTER, FUNCTION getSupportedLinks,CHARACTER, FUNCTION getRunAttribute,CHARACTER, FUNCTION getQueryObject,LOGICAL, FUNCTION getPropertyDialog,CHARACTER, FUNCTION getPhysicalVersion,CHARACTER, FUNCTION getPhysicalObjectName,CHARACTER, FUNCTION getPassThroughLinks,CHARACTER, FUNCTION getParentDataKey,CHARACTER, FUNCTION getObjectVersionNumber,CHARACTER, FUNCTION getObjectVersion,CHARACTER, FUNCTION getObjectParent,HANDLE, FUNCTION getObjectPage,INTEGER, FUNCTION getObjectName,CHARACTER, FUNCTION getObjectInitialized,LOGICAL, FUNCTION getObjectHidden,LOGICAL, FUNCTION getLogicalVersion,CHARACTER, FUNCTION getLogicalObjectName,CHARACTER, FUNCTION getInstanceProperties,CHARACTER, FUNCTION getDynamicObject,LOGICAL, FUNCTION getDesignDataObject,CHARACTER, FUNCTION getDBAware,LOGICAL, FUNCTION getDataTargetEvents,CHARACTER, FUNCTION getDataTarget,CHARACTER, FUNCTION getDataSourceNames,CHARACTER, FUNCTION getDataSourceEvents,CHARACTER, FUNCTION getDataSource,HANDLE, FUNCTION getDataLinksEnabled,LOGICAL, FUNCTION getContainerType,CHARACTER, FUNCTION getContainerSourceEvents,CHARACTER, FUNCTION getContainerSource,HANDLE, FUNCTION getContainerHidden,LOGICAL, FUNCTION getContainerHandle,HANDLE, FUNCTION getChildDataKey,CHARACTER, FUNCTION fetchMessages,CHARACTER, FUNCTION assignLinkProperty,LOGICAL,INPUT pcLink CHARACTER,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER FUNCTION anyMessage,LOGICAL, FUNCTION setServerOperatingMode,LOGICAL,INPUT pcServerOperatingMode CHARACTER FUNCTION setServerFileName,LOGICAL,INPUT pcFileName CHARACTER FUNCTION setASUsePrompt,LOGICAL,INPUT plFlag LOGICAL FUNCTION setASInitializeOnRun,LOGICAL,INPUT plInitialize LOGICAL FUNCTION setASInfo,LOGICAL,INPUT pcInfo CHARACTER FUNCTION setASHandle,LOGICAL,INPUT phASHandle HANDLE FUNCTION setASDivision,LOGICAL,INPUT pcDivision CHARACTER FUNCTION setAppService,LOGICAL,INPUT pcAppService CHARACTER FUNCTION runServerProcedure,HANDLE,INPUT pcServerFileName CHARACTER,INPUT phAppService HANDLE FUNCTION getServerOperatingMode,CHARACTER, FUNCTION getServerFileName,CHARACTER, FUNCTION getASUsePrompt,LOGICAL, FUNCTION getASInitializeOnRun,LOGICAL, FUNCTION getASInfo,CHARACTER, FUNCTION getASHasStarted,LOGICAL, FUNCTION getASHandle,HANDLE, FUNCTION getAsDivision,CHARACTER, FUNCTION getASBound,LOGICAL, FUNCTION getAppService,CHARACTER, FUNCTION createUiEvents,LOGICAL, FUNCTION getObjectSecured,LOGICAL, FUNCTION getObjectTranslated,LOGICAL, FUNCTION setResizeVertical,LOGICAL,INPUT plResizeVertical LOGICAL FUNCTION setResizeHorizontal,LOGICAL,INPUT plResizeHorizontal LOGICAL FUNCTION setObjectLayout,LOGICAL,INPUT pcLayout CHARACTER FUNCTION setLayoutOptions,LOGICAL,INPUT pcOptions CHARACTER FUNCTION setHideOnInit,LOGICAL,INPUT plHide LOGICAL FUNCTION setDisableOnInit,LOGICAL,INPUT plDisable LOGICAL FUNCTION setDefaultLayout,LOGICAL,INPUT pcDefault CHARACTER FUNCTION setAllFieldNames,LOGICAL,INPUT pcValue CHARACTER FUNCTION setAllFieldHandles,LOGICAL,INPUT pcValue CHARACTER FUNCTION getResizeVertical,LOGICAL, FUNCTION getResizeHorizontal,LOGICAL, FUNCTION getWidth,DECIMAL, FUNCTION getRow,DECIMAL, FUNCTION getObjectLayout,CHARACTER, FUNCTION getObjectEnabled,LOGICAL, FUNCTION getLayoutVariable,CHARACTER, FUNCTION getLayoutOptions,CHARACTER, FUNCTION getHideOnInit,LOGICAL, FUNCTION getHeight,DECIMAL, FUNCTION getEnabledObjHdls,CHARACTER, FUNCTION getEnabledObjFlds,CHARACTER, FUNCTION getDisableOnInit,LOGICAL, FUNCTION getDefaultLayout,CHARACTER, FUNCTION getCol,DECIMAL, FUNCTION getAllFieldNames,CHARACTER, FUNCTION getAllFieldHandles,CHARACTER, FUNCTION setStatusArea,LOGICAL,INPUT plStatusArea LOGICAL FUNCTION getObjectType,character, FUNCTION setWindowTitleViewer,LOGICAL,INPUT phViewer HANDLE FUNCTION setWaitForObject,LOGICAL,INPUT phObject HANDLE FUNCTION setUpdateTarget,LOGICAL,INPUT pcTarget CHARACTER FUNCTION setUpdateSource,LOGICAL,INPUT pcSource CHARACTER FUNCTION setTopOnly,LOGICAL,INPUT plTopOnly LOGICAL FUNCTION setSdoForeignFields,LOGICAL,INPUT cSdoForeignFields CHARACTER FUNCTION setSavedContainerMode,LOGICAL,INPUT cSavedContainerMode CHARACTER FUNCTION setRunMultiple,LOGICAL,INPUT plMultiple LOGICAL FUNCTION setRunDOOptions,LOGICAL,INPUT pcOptions CHARACTER FUNCTION setRouterTarget,LOGICAL,INPUT phObject HANDLE FUNCTION setReEnableDataLinks,LOGICAL,INPUT cReEnableDataLinks CHARACTER FUNCTION setPrimarySdoTarget,LOGICAL,INPUT hPrimarySdoTarget HANDLE FUNCTION setPageSource,LOGICAL,INPUT phObject HANDLE FUNCTION setPageNTarget,LOGICAL,INPUT pcObject CHARACTER FUNCTION setOutMessageTarget,LOGICAL,INPUT phObject HANDLE FUNCTION setNavigationTarget,LOGICAL,INPUT cTarget CHARACTER FUNCTION setNavigationSourceEvents,LOGICAL,INPUT pcEvents CHARACTER FUNCTION setNavigationSource,LOGICAL,INPUT pcSource CHARACTER FUNCTION setMultiInstanceSupported,LOGICAL,INPUT lMultiInstanceSupported LOGICAL FUNCTION setMultiInstanceActivated,LOGICAL,INPUT lMultiInstanceActivated LOGICAL FUNCTION setInMessageTarget,LOGICAL,INPUT phObject HANDLE FUNCTION setFilterSource,LOGICAL,INPUT phObject HANDLE FUNCTION setDynamicSDOProcedure,LOGICAL,INPUT pcProc CHARACTER FUNCTION setDisabledAddModeTabs,LOGICAL,INPUT cDisabledAddModeTabs CHARACTER FUNCTION setCurrentPage,LOGICAL,INPUT iPage INTEGER FUNCTION setContainerTarget,LOGICAL,INPUT pcObject CHARACTER FUNCTION setContainerMode,LOGICAL,INPUT cContainerMode CHARACTER FUNCTION setCallerWindow,LOGICAL,INPUT h HANDLE FUNCTION setCallerProcedure,LOGICAL,INPUT h HANDLE FUNCTION setCallerObject,LOGICAL,INPUT h HANDLE FUNCTION pageNTargets,CHARACTER,INPUT phTarget HANDLE,INPUT piPageNum INTEGER FUNCTION getStatusArea,LOGICAL, FUNCTION getWindowTitleViewer,HANDLE, FUNCTION getWaitForObject,HANDLE, FUNCTION getUpdateTarget,CHARACTER, FUNCTION getUpdateSource,CHARACTER, FUNCTION getTopOnly,LOGICAL, FUNCTION getSdoForeignFields,CHARACTER, FUNCTION getSavedContainerMode,CHARACTER, FUNCTION getRunMultiple,LOGICAL, FUNCTION getRunDOOptions,CHARACTER, FUNCTION getReEnableDataLinks,CHARACTER, FUNCTION getPrimarySdoTarget,HANDLE, FUNCTION getPageSource,HANDLE, FUNCTION getPageNTarget,CHARACTER, FUNCTION getOutMessageTarget,HANDLE, FUNCTION getNavigationTarget,HANDLE, FUNCTION getNavigationSourceEvents,CHARACTER, FUNCTION getNavigationSource,CHARACTER, FUNCTION getMultiInstanceSupported,LOGICAL, FUNCTION getMultiInstanceActivated,LOGICAL, FUNCTION getFilterSource,HANDLE, FUNCTION getDynamicSDOProcedure,CHARACTER, FUNCTION getDisabledAddModeTabs,CHARACTER, FUNCTION getCurrentPage,INTEGER, FUNCTION getContainerTargetEvents,CHARACTER, FUNCTION getContainerTarget,CHARACTER, FUNCTION getContainerMode,CHARACTER, FUNCTION getCallerWindow,HANDLE, FUNCTION getCallerProcedure,HANDLE, FUNCTION enablePagesInFolder,LOGICAL,INPUT pcPageInformation CHARACTER FUNCTION disablePagesInFolder,LOGICAL,INPUT pcPageInformation CHARACTER FUNCTION widgetValueList,CHARACTER,INPUT pcNameList CHARACTER,INPUT pcDelimiter CHARACTER FUNCTION widgetValue,CHARACTER,INPUT pcName CHARACTER FUNCTION widgetIsTrue,LOGICAL,INPUT pcName CHARACTER FUNCTION widgetIsModified,LOGICAL,INPUT pcNameList CHARACTER FUNCTION widgetIsFocused,LOGICAL,INPUT pcName CHARACTER FUNCTION widgetIsBlank,LOGICAL,INPUT pcNameList CHARACTER FUNCTION widgetLongcharValue,LONGCHAR,INPUT pcName CHARACTER FUNCTION widgetHandle,HANDLE,INPUT pcName CHARACTER FUNCTION viewWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION toggleWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION resetWidgetValue,LOGICAL,INPUT pcNameList CHARACTER FUNCTION highlightWidget,LOGICAL,INPUT pcNameList CHARACTER,INPUT pcHighlightType CHARACTER FUNCTION hideWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION formattedWidgetValueList,CHARACTER,INPUT pcNameList CHARACTER,INPUT pcDelimiter CHARACTER FUNCTION formattedWidgetValue,CHARACTER,INPUT pcName CHARACTER FUNCTION enableWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION enableRadioButton,LOGICAL,INPUT pcNameList CHARACTER,INPUT piButtonNum INTEGER FUNCTION disableWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION disableRadioButton,LOGICAL,INPUT pcNameList CHARACTER,INPUT piButtonNum INTEGER FUNCTION clearWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION blankWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION assignWidgetValueList,LOGICAL,INPUT pcNameList CHARACTER,INPUT pcValueList CHARACTER,INPUT pcDelimiter CHARACTER FUNCTION assignWidgetValue,LOGICAL,INPUT pcName CHARACTER,INPUT pcValue CHARACTER FUNCTION assignFocusedWidget,LOGICAL,INPUT pcName CHARACTER       �+              P             �� �+  ��              pn              �'    +   �� �  7   `� `  8   �� �   E   �� |  F   0� �  G   � $  H           4� T  ? �� <   iSO8859-1                                                                           �*    �                                       �                  ��                    �'     (   �    ��  4+         `�  �   d+      p+          �                                             PROGRESS                         L
           
    
                    �              �                                                                                                     
               INTEGRAL                         PROGRESS                         �	        �          C                      ��FM               �                              �  l                        |  �e     CODCIACODDOCNRODOCFCHDOCCODCLINOMCLIDIRCLIRUCCLICODANTCODPEDNROPEDNROORDIMPBRTIMPEXOPORIGVIMPIGVIMPDTOIMPTOTSDOACTFLGESTCODCOBCODCTAUSUARIOFLGCIEFCHCIEHORCIECODMONTPOCMBCODALMLUGENTTIPOCODMOVCODVENIMPISCIMPVTAFCHCANGLOSACODREFNROREFFCHVTOCODAGEFLGUBIFLGUBIAFCHUBIFCHUBIAFLGSITCNDCRECODDIVIMPINTFMAPGOFCHACTFLGSITATIPVTAPORDTOTPOFACUSRDSCTOFLGCBDFCHCBDNROSALCODOPENROMESNROASTFCHANUUSUANUCODDPTOCODPROVCODDISTFLGCONLUGENT2FLGATEFCHATEIMPFLEIMPTOT2IMPCTOACUBONNROCARDTIPBONCCOFLGENVPUNTOSMRGUTISEDELIBRE_C01LIBRE_C02LIBRE_C03LIBRE_C04LIBRE_C05LIBRE_D01LIBRE_D02LIBRE_F01LIBRE_F02FCHCRELIBRE_F03LIBRE_F04LIBRE_F05FCHCOBRANZAUSRCOBRANZADIVORIIMPPROIMPDTO2GLOSAIMPDTO2                                                                       	          
                                                                                                                                                                                                                                       !          "          #          $          %          &          '          (          )          *          +          ,          -          .          /          0          1          2          3          4          5          6          7          8          9          :          ;          <          =          >          ?          @          A          B          C          D          E          F          G          H          I          J          K          L 
        M          N 
        O          P          Q          R          S          T          U          V          W          X          Y          Z          [          \          ]          ^          _          `          a          b          c          d          e          f          |
             8
                                                                                          �             �
  �
      t
  
    
                  `
  (             �
                                                                                          �
          
  �  �
         
    
                    �             �                                                                                          �
          
  P  �
      �  
    
                  �  �             <                                                                                          �
          
  �  �
      x  
    
                  d  ,             �                                                                                          �
          
  �  �
      $  
    
                    �  	           �                                                                                          �
          
  T  �
      �  
    
                  �  �  
           @                                                                                          �
          
     �
      |  
    
                  h  0             �                                                                                          �
          
  �        (  
    
                    �             �                                                                                                    
  X        �                         �  �             D                                                                                                              �                        l  4             �                                                                                                      �  +      ,  
    
                    �             �                                                                                          +          
  \  9      �  
    
                  �  �             H                                                                                          9          
    G      �  
    
                  p  8             �                                                                                          G          
  �  U      0                          �             �                                                                                          U            `  e      �                        �  �             L                                                                                          e              p      �                        t  <             �                                                                                          p                �      4                           �             �                                                                                          �            �        �                                ��FM               �                              �  8                      �  H  �e     CODCIACODDOCNRODOCFCHDOCCODCLINOMCLIDIRCLIRUCCLICODANTCODPEDNROPEDNROORDIMPBRTIMPEXOPORIGVIMPIGVIMPDTOIMPTOTSDOACTFLGESTCODCOBCODCTAUSUARIOFLGCIEFCHCIEHORCIECODMONTPOCMBCODALMLUGENTTIPOCODMOVCODVENIMPISCIMPVTAFCHCANGLOSACODREFNROREFFCHVTOCODAGEFLGUBIFLGUBIAFCHUBIFCHUBIAFLGSITCNDCRECODDIVIMPINTFMAPGOFCHACTFLGSITATIPVTAPORDTOTPOFACUSRDSCTOFLGCBDFCHCBDNROSALCODOPENROMESNROASTFCHANUUSUANUCODDPTOCODPROVCODDISTFLGCONLUGENT2FLGATEFCHATEIMPFLEIMPTOT2IMPCTOACUBONNROCARDTIPBONCCOFLGENVPUNTOSMRGUTISEDELIBRE_C01LIBRE_C02LIBRE_C03LIBRE_C04LIBRE_C05LIBRE_D01LIBRE_D02LIBRE_F01LIBRE_F02FCHCRELIBRE_F03LIBRE_F04LIBRE_F05FCHCOBRANZAUSRCOBRANZADIVORIIMPPROIMPDTO2GLOSAIMPDTO2                                                                       	          
                                                                                                                                                                                                                                       !          "          #          $          %          &          '          (          )          *          +          ,          -          .          /          0          1          2          3          4          5          6          7          8          9          :          ;          <          =          >          ?          @          A          B          C          D          E          F          G          H          I          J          K          L 
        M          N 
        O          P          Q          R          S          T          U          V          W          X          Y          Z          [          \          ]          ^          _          `          a          b          c          d          e          f          x      &  �      &                         5cN            &  ��                              �                        �  ,  �       CODCIACODDOCNRODOCNROITMUNDVTACODMATPREUNIPORDTOIMPDTOIMPLINCANDESAFTIGVAFTISCPREBASPREVTAIMPIGVIMPISCFACTORCANDEVPORDTO2PESMATCODCLIALMDESPOR_DSCTOSFLG_FACTORFCHDOCCODDIVIMPCTOPUNTOSMRGUTIIMPPROIMPDTO2                                                                        	          
                                                                                                                                                                                                                                     !          �#     K  �      K                         �ȔM            K  *e                              �  �                       �!  !  � (     CODCIACODALMTIPMOVCODMOVNRODOCFCHDOCNRORF1NRORF2CODPROCODCLICODVENOBSERVTOTITMCODMONTPOCMBFLGESTALMDESUSUARIONROSERFLGSITHORSALHORRCPFCHANUCODDOCCODREFNROREFCODTRAFLGCBDFCHCBDFLGFACNROFACNOMREFIMPMN1IMPMN2CCOAREAIMPIGVMODADQNRORF3HRADOC                                                                      	          
                                                                                                                                                                                                                                       !          "          #          $          %          &          '          (          )                 U  �      U                         )�M            U  �r                              �  T$                      �%  d$  80     CODCIACODALMTIPMOVCODMOVNRODOCFCHDOCCODMONNROITMTPOCMBCODMATCANDESFACTORPREUNICODUNDIMPCTOAJUSTEALMORIIMPMN1IMPMN2VCTOMN1VCTOMN2STKSUBSTKACTCODAJTNROSERPORDTOIMPDTOIMPLINAFTIGVAFTISCPREBASIMPIGVIMPISCCANDEVDSCTOSIGVMATPRECOSPRELISPESMATCODANTNROANTPOR_DSCTOSFLG_FACTORHRADOCSTKACTCBDSTKSUBCBDVCTOMN1CBDVCTOMN2CBD                                                                      	          
                                                                                                                                                                                                                                       !          "          #          $         %          &          '          (          )          *          +         ,          -          .          /          0          1                        ��                                              ' ��          �)  d*  p  D(            
                FAC                                                    G/R                                                                                                                    
             
             
                                         
                                                                                                                p   �   �   �   �   �   �   �   �           0  @  P  `  p  �  �  �  �  �  �  �  �           p   �   �   �   �   �   �   �   �          0  @  P  `  p  �  �  �  �  �  �  �  �         ��                                               �          ����                                �"    &    ��    .    \�    5    ��    undefined                                                               �        �  �   l   0�    `�                  �����               (�`                    O   ����    e�          O   ����    R�          O   ����    ��      t       �   �              4   ����     /                                    3   ����       $     H  ���                       8      
                       � ߱        �  �      D       p
     A          assignFocusedWidget         �      �             LOGICAL,INPUT pcName CHARACTER  assignWidgetValue   �      �      $    4       LOGICAL,INPUT pcName CHARACTER,INPUT pcValue CHARACTER  assignWidgetValueList         \      �    F       LOGICAL,INPUT pcNameList CHARACTER,INPUT pcValueList CHARACTER,INPUT pcDelimiter CHARACTER  blankWidget t      �          \       LOGICAL,INPUT pcNameList CHARACTER  clearWidget �      @      l    h       LOGICAL,INPUT pcNameList CHARACTER  disableRadioButton  L      �      �    t       LOGICAL,INPUT pcNameList CHARACTER,INPUT piButtonNum INTEGER    disableWidget   �            4    �       LOGICAL,INPUT pcNameList CHARACTER  enableRadioButton         X      �    �       LOGICAL,INPUT pcNameList CHARACTER,INPUT piButtonNum INTEGER    enableWidget    l      �      �    �       LOGICAL,INPUT pcNameList CHARACTER  formattedWidgetValue    �             X  	  �       CHARACTER,INPUT pcName CHARACTER    formattedWidgetValueList    8      |      �  
  �       CHARACTER,INPUT pcNameList CHARACTER,INPUT pcDelimiter CHARACTER    hideWidget  �      �      (   
 �       LOGICAL,INPUT pcNameList CHARACTER  highlightWidget       L      |    �       LOGICAL,INPUT pcNameList CHARACTER,INPUT pcHighlightType CHARACTER  resetWidgetValue    \      �      �    �       LOGICAL,INPUT pcNameList CHARACTER  toggleWidget    �            H          LOGICAL,INPUT pcNameList CHARACTER  viewWidget  (      l      �   
       LOGICAL,INPUT pcNameList CHARACTER  widgetHandle    x      �      �    &      HANDLE,INPUT pcName CHARACTER   widgetLongcharValue �            @    3      LONGCHAR,INPUT pcName CHARACTER widgetIsBlank          `      �    G      LOGICAL,INPUT pcNameList CHARACTER  widgetIsFocused p      �      �    U      LOGICAL,INPUT pcName CHARACTER  widgetIsModified    �      	      8	    e      LOGICAL,INPUT pcNameList CHARACTER  widgetIsTrue    	      \	      �	    v      LOGICAL,INPUT pcName CHARACTER  widgetValue l	      �	      �	    �      CHARACTER,INPUT pcName CHARACTER    widgetValueList �	      �	      ,
    �      CHARACTER,INPUT pcNameList CHARACTER,INPUT pcDelimiter CHARACTER    �    ~  �
  �
  P  d       4   ����d       o          �
                              �  �   NA  �   �  �   �  �      �      �     �         $    8    L  `  `  
`  t  $  �    �     �      $  �  |  ���                       �     
                    � ߱        ؁    �  �  @      �      4   �����                P                      ��                  �  �                  4�                       �  �  �    �  l  |             4   ����       $  �  �  ���                       P  @         <              � ߱              �  �         �      4   �����      $  �  ,  ���                       �  @         �              � ߱        assignPageProperty                              �  �      ��                  G  J                8                    O   ����    e�          O   ����    R�          O   ����    ��            ��   T                             ��                  H           ��                            ����                            changePage                              @  (      ��                  L  M  X              D�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            confirmExit                             @  (      ��                  O  Q  X              |�                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  p           ��                            ����                            constructObject                             l  T      ��                  S  X  �              ��                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �             �               �� 
  �             �  
             ��                 �               �� 
                   
         ��                            ����                            createObjects                                 �      ��                  Z  [  (              ��                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            deletePage                                �      ��                  ]  _  (              ��                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  @           ��                            ����                            destroyObject                               <  $      ��                  a  b  T              L�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            hidePage                                <  $      ��                  d  f  T              ��                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  l           ��                            ����                            initializeObject                                l  T      ��                  h  i  �               �                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeVisualContainer                               |  d      ��                  k  l  �              8�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initPages                               |  d      ��                  n  p  �              ��                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �           ��                            ����                            notifyPage                              �  �      ��                  r  t  �              (�                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �           ��                            ����                            passThrough                             �  �      ��                  v  y  �              ��                    O   ����    e�          O   ����    R�          O   ����    ��            ��   0             �               ��                  $           ��                            ����                            removePageNTarget                               $        ��                  {  ~  <              ��                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  �             T  
             ��                  |           ��                            ����                            selectPage                              t  \      ��                  �  �  �              ��                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �           ��                            ����                            toolbar                             �  �      ��                  �  �  �              ��                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �           ��                            ����                            viewObject                              �   �       ��                  �  �  �               �                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            viewPage                                �!  �!      ��                  �  �  �!              ��                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �!           ��                            ����                            disablePagesInFolder    
      X"      �"    �      LOGICAL,INPUT pcPageInformation CHARACTER   enablePagesInFolder p"      �"      �"    �      LOGICAL,INPUT pcPageInformation CHARACTER   getCallerProcedure  �"      #      P#          HANDLE, getCallerWindow 0#      X#      �#          HANDLE, getContainerMode    h#      �#      �#    '      CHARACTER,  getContainerTarget  �#      �#      $    8      CHARACTER,  getContainerTargetEvents    �#      $      L$    K      CHARACTER,  getCurrentPage  ,$      X$      �$    d      INTEGER,    getDisabledAddModeTabs  h$      �$      �$     s      CHARACTER,  getDynamicSDOProcedure  �$      �$      %  !  �      CHARACTER,  getFilterSource �$      %      L%  "  �      HANDLE, getMultiInstanceActivated   ,%      T%      �%  #  �      LOGICAL,    getMultiInstanceSupported   p%      �%      �%  $  �      LOGICAL,    getNavigationSource �%      �%      &  %  �      CHARACTER,  getNavigationSourceEvents   �%      $&      `&  &  �      CHARACTER,  getNavigationTarget @&      l&      �&  '        HANDLE, getOutMessageTarget �&      �&      �&  (  '      HANDLE, getPageNTarget  �&      �&      '  )  ;      CHARACTER,  getPageSource   �&       '      P'  *  J      HANDLE, getPrimarySdoTarget 0'      X'      �'  +  X      HANDLE, getReEnableDataLinks    l'      �'      �'  ,  l      CHARACTER,  getRunDOOptions �'      �'      (  -  �      CHARACTER,  getRunMultiple  �'      (      D(  .  �      LOGICAL,    getSavedContainerMode   $(      P(      �(  /  �      CHARACTER,  getSdoForeignFields h(      �(      �(  0  �      CHARACTER,  getTopOnly  �(      �(       )  1 
 �      LOGICAL,    getUpdateSource �(      )      <)  2  �      CHARACTER,  getUpdateTarget )      H)      x)  3  �      CHARACTER,  getWaitForObject    X)      �)      �)  4  �      HANDLE, getWindowTitleViewer    �)      �)      �)  5        HANDLE, getStatusArea   �)       *      0*  6        LOGICAL,    pageNTargets    *      <*      l*  7  )      CHARACTER,INPUT phTarget HANDLE,INPUT piPageNum INTEGER setCallerObject L*      �*      �*  8  6      LOGICAL,INPUT h HANDLE  setCallerProcedure  �*      �*       +  9  F      LOGICAL,INPUT h HANDLE  setCallerWindow  +      8+      h+  :  Y      LOGICAL,INPUT h HANDLE  setContainerMode    H+      �+      �+  ;  i      LOGICAL,INPUT cContainerMode CHARACTER  setContainerTarget  �+      �+      ,  <  z      LOGICAL,INPUT pcObject CHARACTER    setCurrentPage  �+      4,      d,  =  �      LOGICAL,INPUT iPage INTEGER setDisabledAddModeTabs  D,      �,      �,  >  �      LOGICAL,INPUT cDisabledAddModeTabs CHARACTER    setDynamicSDOProcedure  �,      �,       -  ?  �      LOGICAL,INPUT pcProc CHARACTER  setFilterSource  -      @-      p-  @  �      LOGICAL,INPUT phObject HANDLE   setInMessageTarget  P-      �-      �-  A  �      LOGICAL,INPUT phObject HANDLE   setMultiInstanceActivated   �-      �-       .  B  �      LOGICAL,INPUT lMultiInstanceActivated LOGICAL   setMultiInstanceSupported    .      P.      �.  C        LOGICAL,INPUT lMultiInstanceSupported LOGICAL   setNavigationSource l.      �.      �.  D  !      LOGICAL,INPUT pcSource CHARACTER    setNavigationSourceEvents   �.      /      P/  E  5      LOGICAL,INPUT pcEvents CHARACTER    setNavigationTarget 0/      t/      �/  F  O      LOGICAL,INPUT cTarget CHARACTER setOutMessageTarget �/      �/      �/  G  c      LOGICAL,INPUT phObject HANDLE   setPageNTarget  �/      0      L0  H  w      LOGICAL,INPUT pcObject CHARACTER    setPageSource   ,0      p0      �0  I  �      LOGICAL,INPUT phObject HANDLE   setPrimarySdoTarget �0      �0      �0  J  �      LOGICAL,INPUT hPrimarySdoTarget HANDLE  setReEnableDataLinks    �0      1      T1  K  �      LOGICAL,INPUT cReEnableDataLinks CHARACTER  setRouterTarget 41      �1      �1  L  �      LOGICAL,INPUT phObject HANDLE   setRunDOOptions �1      �1       2  M  �      LOGICAL,INPUT pcOptions CHARACTER   setRunMultiple  �1      $2      T2  N  �      LOGICAL,INPUT plMultiple LOGICAL    setSavedContainerMode   42      x2      �2  O  �      LOGICAL,INPUT cSavedContainerMode CHARACTER setSdoForeignFields �2      �2      3  P        LOGICAL,INPUT cSdoForeignFields CHARACTER   setTopOnly  �2      <3      h3  Q 
       LOGICAL,INPUT plTopOnly LOGICAL setUpdateSource H3      �3      �3  R  !      LOGICAL,INPUT pcSource CHARACTER    setUpdateTarget �3      �3      4  S  1      LOGICAL,INPUT pcTarget CHARACTER    setWaitForObject    �3      04      d4  T  A      LOGICAL,INPUT phObject HANDLE   setWindowTitleViewer    D4      �4      �4  U  R      LOGICAL,INPUT phViewer HANDLE   getObjectType   �4      �4      5  V  g      CHARACTER,  setStatusArea   �4      5      H5  W  u      LOGICAL,INPUT plStatusArea LOGICAL  applyLayout                             �5  �5      ��                  	  
  6              �%                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            disableObject                                7  �6      ��                      7               (                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            enableObject                                8  �7      ��                      8              �(                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeObject                                9  �8      ��                      $9              @                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            processAction                               :  �9      ��                      (:              H                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  @:           ��                            ����                            getAllFieldHandles  (5      �:      �:  X  �      CHARACTER,  getAllFieldNames    �:      �:      ;  Y  �      CHARACTER,  getCol  �:      (;      P;  Z  �      DECIMAL,    getDefaultLayout    0;      \;      �;  [  �      CHARACTER,  getDisableOnInit    p;      �;      �;  \  �      LOGICAL,    getEnabledObjFlds   �;      �;      <  ]  �      CHARACTER,  getEnabledObjHdls   �;      <      P<  ^  �      CHARACTER,  getHeight   0<      \<      �<  _ 	 �      DECIMAL,    getHideOnInit   h<      �<      �<  `  �      LOGICAL,    getLayoutOptions    �<      �<      =  a        CHARACTER,  getLayoutVariable   �<      =      D=  b        CHARACTER,  getObjectEnabled    $=      P=      �=  c  /      LOGICAL,    getObjectLayout d=      �=      �=  d  @      CHARACTER,  getRow  �=      �=      �=  e  P      DECIMAL,    getWidth    �=       >      ,>  f  W      DECIMAL,    getResizeHorizontal >      8>      l>  g  `      LOGICAL,    getResizeVertical   L>      x>      �>  h  t      LOGICAL,    setAllFieldHandles  �>      �>      �>  i  �      LOGICAL,INPUT pcValue CHARACTER setAllFieldNames    �>      ?      @?  j  �      LOGICAL,INPUT pcValue CHARACTER setDefaultLayout     ?      `?      �?  k  �      LOGICAL,INPUT pcDefault CHARACTER   setDisableOnInit    t?      �?      �?  l  �      LOGICAL,INPUT plDisable LOGICAL setHideOnInit   �?      @      <@  m  �      LOGICAL,INPUT plHide LOGICAL    setLayoutOptions    @      \@      �@  n  �      LOGICAL,INPUT pcOptions CHARACTER   setObjectLayout p@      �@      �@  o  �      LOGICAL,INPUT pcLayout CHARACTER    setResizeHorizontal �@      A      <A  p  �      LOGICAL,INPUT plResizeHorizontal LOGICAL    setResizeVertical   A      hA      �A  q  	      LOGICAL,INPUT plResizeVertical LOGICAL  getObjectTranslated |A      �A      �A  r  !	      LOGICAL,    getObjectSecured    �A      B      8B  s  5	      LOGICAL,    createUiEvents  B      DB      tB  t  F	      LOGICAL,    bindServer                              C  �B      ��                  �  �  (C              �I                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            destroyObject                               D  �C      ��                  �  �  ,D              pL                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            destroyServerObject                             E  E      ��                  �     4E              �C                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            disconnectObject                                $F  F      ��                      <F              DD                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeServerObject                              0G  G      ��                      HG              �D                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            restartServerObject                             8H   H      ��                    	  PH              �;                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            runServerObject                             <I  $I      ��                      TI              t<                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
                 lI  
         ��                            ����                            startServerObject                               lJ  TJ      ��                      �J               I                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            unbindServer                                pK  XK      ��                      �K              4t                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �K           ��                            ����                            getAppService   TB      L      8L  u  U	      CHARACTER,  getASBound  L      DL      pL  v 
 c	      LOGICAL,    getAsDivision   PL      |L      �L  w  n	      CHARACTER,  getASHandle �L      �L      �L  x  |	      HANDLE, getASHasStarted �L      �L      M  y  �	      LOGICAL,    getASInfo   �L      (M      TM  z 	 �	      CHARACTER,  getASInitializeOnRun    4M      `M      �M  {  �	      LOGICAL,    getASUsePrompt  xM      �M      �M  |  �	      LOGICAL,    getServerFileName   �M      �M      N  }  �	      CHARACTER,  getServerOperatingMode  �M       N      XN  ~  �	      CHARACTER,  runServerProcedure  8N      dN      �N    �	      HANDLE,INPUT pcServerFileName CHARACTER,INPUT phAppService HANDLE   setAppService   xN      �N      O  �  
      LOGICAL,INPUT pcAppService CHARACTER    setASDivision   �N      4O      dO  �  
      LOGICAL,INPUT pcDivision CHARACTER  setASHandle DO      �O      �O  �  
      LOGICAL,INPUT phASHandle HANDLE setASInfo   �O      �O       P  � 	 *
      LOGICAL,INPUT pcInfo CHARACTER  setASInitializeOnRun    �O       P      XP  �  4
      LOGICAL,INPUT plInitialize LOGICAL  setASUsePrompt  8P      |P      �P  �  I
      LOGICAL,INPUT plFlag LOGICAL    setServerFileName   �P      �P       Q  �  X
      LOGICAL,INPUT pcFileName CHARACTER  setServerOperatingMode  �P      $Q      \Q  �  j
      LOGICAL,INPUT pcServerOperatingMode CHARACTER   addLink                             R   R      ��                  �  �  0R              \�                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  |R             HR  
             ��   �R             pR               �� 
                 �R  
         ��                            ����                            addMessage                              �S  xS      ��                  �  �  �S              �                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �S             �S               ��   T             �S               ��                  T           ��                            ����                            adjustTabOrder                              U  �T      ��                  �  �  $U              d�                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  pU             <U  
             �� 
  �U             dU  
             ��                  �U           ��                            ����                            applyEntry                              �V  lV      ��                  �  �  �V              ��                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �V           ��                            ����                            changeCursor                                �W  �W      ��                  �  �  �W              <f                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �W           ��                            ����                            createControls                              �X  �X      ��                  �  �  �X              �f                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            destroyObject                               �Y  �Y      ��                  �  �  �Y              L
f                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            displayLinks                                �Z  �Z      ��                  �  �  �Z              �f                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            editInstanceProperties                              �[  �[      ��                  �  �  \              Tf                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            exitObject                              �\  �\      ��                  �  �  ]              rf                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            hideObject                              �]  �]      ��                       ^              �rf                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeObject                                �^  �^      ��                      _              Hsf                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            modifyListProperty                               `  �_      ��                      `              �zf                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  d`             0`  
             ��   �`             X`               ��   �`             �`               ��                  �`           ��                            ����                            modifyUserLinks                             �a  �a      ��                      �a              H�f                    O   ����    e�          O   ����    R�          O   ����    ��            ��   b             �a               ��   0b             �a               �� 
                 $b  
         ��                            ����                            removeAllLinks                               c  c      ��                      8c              l{f                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            removeLink                               d  d      ��                      8d              \vf                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  �d             Pd  
             ��   �d             xd               �� 
                 �d  
         ��                            ����                            repositionObject                                �e  �e      ��                      �e              ��f                    O   ����    e�          O   ����    R�          O   ����    ��            ��   f             �e               ��                  �e           ��                            ����                            returnFocus                             �f  �f      ��                  !  #  g              l�f                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
                  g  
         ��                            ����                            showMessageProcedure                                $h  h      ��                  %  (  <h              ��f                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �h             Th               ��                  |h           ��                            ����                            toggleData                              ti  \i      ��                  *  ,  �i              (�f                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �i           ��                            ����                            viewObject                              �j  �j      ��                  .  /  �j              �g                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            anyMessage  <Q      k      8k  � 
 �      LOGICAL,    assignLinkProperty  k      Dk      xk  �  �      LOGICAL,INPUT pcLink CHARACTER,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER   fetchMessages   Xk      �k       l  �  �      CHARACTER,  getChildDataKey �k      l      <l  �  �      CHARACTER,  getContainerHandle  l      Hl      |l  �        HANDLE, getContainerHidden  \l      �l      �l  �        LOGICAL,    getContainerSource  �l      �l      �l  �  1      HANDLE, getContainerSourceEvents    �l       m      <m  �  D      CHARACTER,  getContainerType    m      Hm      |m  �  ]      CHARACTER,  getDataLinksEnabled \m      �m      �m  �  n      LOGICAL,    getDataSource   �m      �m      �m  �  �      HANDLE, getDataSourceEvents �m       n      4n  �  �      CHARACTER,  getDataSourceNames  n      @n      tn  �  �      CHARACTER,  getDataTarget   Tn      �n      �n  �  �      CHARACTER,  getDataTargetEvents �n      �n      �n  �  �      CHARACTER,  getDBAware  �n      �n      (o  � 
 �      LOGICAL,    getDesignDataObject o      4o      ho  �  �      CHARACTER,  getDynamicObject    Ho      to      �o  �  �      LOGICAL,    getInstanceProperties   �o      �o      �o  �  	      CHARACTER,  getLogicalObjectName    �o      �o      0p  �        CHARACTER,  getLogicalVersion   p      <p      pp  �  4      CHARACTER,  getObjectHidden Pp      |p      �p  �  F      LOGICAL,    getObjectInitialized    �p      �p      �p  �  V      LOGICAL,    getObjectName   �p      �p      ,q  �  k      CHARACTER,  getObjectPage   q      8q      hq  �  y      INTEGER,    getObjectParent Hq      tq      �q  �  �      HANDLE, getObjectVersion    �q      �q      �q  �  �      CHARACTER,  getObjectVersionNumber  �q      �q      $r  �  �      CHARACTER,  getParentDataKey    r      0r      dr  �  �      CHARACTER,  getPassThroughLinks Dr      pr      �r  �  �      CHARACTER,  getPhysicalObjectName   �r      �r      �r  �  �      CHARACTER,  getPhysicalVersion  �r      �r      (s  �  �      CHARACTER,  getPropertyDialog   s      4s      hs  �        CHARACTER,  getQueryObject  Hs      ts      �s  �        LOGICAL,    getRunAttribute �s      �s      �s  �  .      CHARACTER,  getSupportedLinks   �s      �s       t  �  >      CHARACTER,  getTranslatableProperties    t      ,t      ht  �  P      CHARACTER,  getUIBMode  Ht      tt      �t  � 
 j      CHARACTER,  getUserProperty �t      �t      �t  �  u      CHARACTER,INPUT pcPropName CHARACTER    instancePropertyList    �t      u      <u  �  �      CHARACTER,INPUT pcPropList CHARACTER    linkHandles u      du      �u  �  �      CHARACTER,INPUT pcLink CHARACTER    linkProperty    pu      �u      �u  �  �      CHARACTER,INPUT pcLink CHARACTER,INPUT pcPropName CHARACTER mappedEntry �u       v      Lv  �  �      CHARACTER,INPUT pcEntry CHARACTER,INPUT pcList CHARACTER,INPUT plFirst LOGICAL,INPUT pcDelimiter CHARACTER  messageNumber   ,v      �v      �v  �  �      CHARACTER,INPUT piMessage INTEGER   propertyType    �v      w      <w  �  �      CHARACTER,INPUT pcPropName CHARACTER    reviewMessages  w      dw      �w  �  �      CHARACTER,  setChildDataKey tw      �w      �w  �  �      LOGICAL,INPUT cChildDataKey CHARACTER   setContainerHidden  �w      �w      ,x  �  �      LOGICAL,INPUT plHidden LOGICAL  setContainerSource  x      Lx      �x  �        LOGICAL,INPUT phObject HANDLE   setContainerSourceEvents    `x      �x      �x  �        LOGICAL,INPUT pcEvents CHARACTER    setDataLinksEnabled �x       y      4y  �  8      LOGICAL,INPUT lDataLinksEnabled LOGICAL setDataSource   y      \y      �y  �  L      LOGICAL,INPUT phObject HANDLE   setDataSourceEvents ly      �y      �y  �  Z      LOGICAL,INPUT pcEventsList CHARACTER    setDataSourceNames  �y      z      <z  �  n      LOGICAL,INPUT pcSourceNames CHARACTER   setDataTarget   z      dz      �z  �  �      LOGICAL,INPUT pcTarget CHARACTER    setDataTargetEvents tz      �z      �z  �  �      LOGICAL,INPUT pcEvents CHARACTER    setDBAware  �z      {      <{  � 
 �      LOGICAL,INPUT lAware LOGICAL    setDesignDataObject {      \{      �{  �  �      LOGICAL,INPUT pcDataObject CHARACTER    setDynamicObject    p{      �{      �{  �  �      LOGICAL,INPUT lTemp LOGICAL setInstanceProperties   �{      |      @|  �  �      LOGICAL,INPUT pcPropList CHARACTER  setLogicalObjectName     |      d|      �|  �  �      LOGICAL,INPUT c CHARACTER   setLogicalVersion   ||      �|      �|  �  �      LOGICAL,INPUT cVersion CHARACTER    setObjectName   �|      }      @}  �        LOGICAL,INPUT pcName CHARACTER  setObjectParent  }      `}      �}  �        LOGICAL,INPUT phParent HANDLE   setObjectVersion    p}      �}      �}  �  .      LOGICAL,INPUT cObjectVersion CHARACTER  setParentDataKey    �}      ~      @~  �  ?      LOGICAL,INPUT cParentDataKey CHARACTER  setPassThroughLinks  ~      h~      �~  �  P      LOGICAL,INPUT pcLinks CHARACTER setPhysicalObjectName   |~      �~      �~  �  d      LOGICAL,INPUT cTemp CHARACTER   setPhysicalVersion  �~            H  �  z      LOGICAL,INPUT cVersion CHARACTER    setRunAttribute (      l      �  �  �      LOGICAL,INPUT cRunAttribute CHARACTER   setSupportedLinks   |      �      �  �  �      LOGICAL,INPUT pcLinkList CHARACTER  setTranslatableProperties   �      �      X�  �  �      LOGICAL,INPUT pcPropList CHARACTER  setUIBMode  8�      |�      ��  � 
 �      LOGICAL,INPUT pcMode CHARACTER  setUserProperty ��      Ȁ      ��  �  �      LOGICAL,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER  showmessage ؀      8�      d�  �  �      LOGICAL,INPUT pcMessage CHARACTER   Signature   D�      ��      ��  � 	 �      CHARACTER,INPUT pcName CHARACTER    ��    E  �  p�            4   ����                ��                      ��                  F  s                  �Ig                       F  �        G  ��  �      (      4   ����(                (�                      ��                  H  r                  XJg                       H  ��  (�    _  D�  ��      <      4   ����<                Ѓ                      ��                  k  m                  �Mg                       k  T�         l                                  �     
                    � ߱        T�  $  o  ��  ���                           $  q  ��  ���                       $                         � ߱        ��    w  Ȅ  D�      4      4   ����4                T�                      ��                  x  <	                  �Ng                       x  ؄  ��  o   {      ,                                 ��  $   |  ��  ���                       �  @         �              � ߱        �  �   }  �      �  �   ~  <      �  �   �  �      0�  �   �  $      D�  �   �  �      X�  �   �        l�  �   �  �      ��  �   �  �      ��  �   �  8      ��  �   �  �      ��  �   �  (	      І  �   �  �	      �  �   �   
      ��  �   �  \
      �  �   �  �
       �  �   �  L      4�  �   �  �      H�  �   �  �      \�  �   �  8      p�  �   �  �      ��  �   �         ��  �   �  �      ��  �   �        ��  �   �  �      ԇ  �   �        �  �   �  |      ��  �   �  �      �  �   �  ,      $�  �   �  �      8�  �   �  �      L�  �   �  P      `�  �   �  �      t�  �   �  �      ��  �   �        ��  �   �  @      ��  �   �  �      Ĉ  �   �  �      ؈  �   �  4      �  �   �  p       �  �   �  �      �  �   �  �      (�  �   �  $      <�  �   �  `      P�  �   �  �          �   �  �                      |�          �  Љ      ��                  c	  �	   �              ��f                    O   ����    e�          O   ����    R�          O   ����    ��      H     
                �                     �                         � ߱        ��  $ w	  �  ���                           O   �	  ��  ��                 �          �  �    �                                             ��                            ����                                �4      d�      ��     6     �                      V �  g                     x�    �	  ԋ  P�             4   ����                 `�                      ��                  �	  8
                  �:g                       �	  �  t�  �   �	  �      ��  �   �	  �      ��  �   �	  p      ��  �   �	  �      Č  �   �	  h      ،  �   �	  �      �  �   �	  X       �  �   �	  �      �  �   �	  P      (�  �   �	  �      <�  �   �	  @      P�  �   �	  �      d�  �   �	  8          �   �	  �      P�    C
  ��  �      $      4   ����$                 �                      ��                  D
  �
                  �<g                       D
  ��  4�  �   F
  �      H�  �   G
  �      \�  �   H
  l      p�  �   I
  �      ��  �   J
  \       ��  �   K
  �       ��  �   L
  L!      ��  �   M
  �!      Ԏ  �   N
  4"      �  �   O
  �"      ��  �   P
  $#      �  �   Q
  �#      $�  �   R
  $      8�  �   S
  �$      L�  �   T
  %      `�  �   U
  �%      t�  �   V
  �%      ��  �   W
  x&      ��  �   X
  �&      ��  �   Y
  p'      ď  �   Z
  �'      ؏  �   [
  h(      �  �   \
  �(       �  �   ]
  `)      �  �   ^
  �)      (�  �   _
  X*      <�  �   `
  �*          �   a
  P+      l�    �
  l�  �      �+      4   �����+                ��                      ��                  �
  �                  T#                       �
  |�  �  �   �
  ,       �  �   �
  �,      4�  �   �
  -      H�  �   �
  �-      \�  �   �
  �-      p�  �   �
  l.      ��  �   �
  �.      ��  �   �
  /      ��  �   �
  �/      ��  �   �
  �/      ԑ  �   �
  0      �  �   �
  |0      ��  �   �
  �0      �  �   �
  l1      $�  �   �
  �1      8�  �   �
  T2      L�  �   �
  �2      `�  �   �
  D3      t�  �   �
  �3      ��  �   �
  �3      ��  �   �
  p4      ��  �   �
  �4      Ē  �   �
  X5      ؒ  �   �
  �5      �  �   �
  �5       �  �   �
  L6      �  �      �6      (�  �     �6      <�  �      7      P�  �     <7      d�  �     x7      x�  �     �7      ��  �     �7      ��  �     d8      ��  �   	  �8      ȓ  �   
  �8      ܓ  �     9      �  �     T9      �  �     �9      �  �     �9      ,�  �     :      @�  �     |:      T�  �     �:      h�  �     d;      |�  �     �;      ��  �     T<      ��  �     �<      ��  �     L=      ̔  �     �=      ��  �     D>      ��  �     �>      �  �     �>      �  �     x?      0�  �     �?      D�  �     �?      X�  �     ,@          �     �@      ĕ  $  �  ��  ���                       A     
                    � ߱        \�    �  ��  �      A      4   ����A      /   �  �     ,�                          3   ����$A            L�                      3   ����DA  ��    �  x�  ��  ��  `A      4   ����`A  	              �                      ��             	     �  d                  �Wg                       �  ��  �  �   �  �A      p�  $  �  D�  ���                       �A     
                    � ߱        ��  �   �  B      ܗ  $   �  ��  ���                       4B  @          B              � ߱        ��  $  �  �  ���                       �B                         � ߱        �B     
                xC                     �D  @        
 �D              � ߱        (�  V   �  4�  ���                        �D                     E                     DE                         � ߱        ��  $    Ę  ���                       F     
                �F                     �G  @        
 �G              � ߱        H�  V   #  T�  ���                        �G     
                XH                     �I  @        
 hI              � ߱            V   H  �  ���                        
              ��                      ��             
     f                    ng                       f  t�  �I     
                0J                     �K  @        
 @K          �K  @        
 �K          DL  @        
 L          �L  @        
 dL              � ߱            V   {  �  ���                        adm-clone-props \�  ԛ              �     7     `                          \  :                     start-super-proc    �  @�  �           �     8                                  [                     H�      ̜  ܜ      0P      4   ����0P      /     �     �                          3   ����@P            8�                      3   ����`P  ��  $  6  t�  ���                       �P                         � ߱        \�    F  ��  8�  ؞  �P      4   �����P                ��                      ��                  G  K                  �`g                       G  ̝  �P                     �P                     �P                         � ߱            $  H  H�  ���                             L  ��  0�      �P      4   �����P  Q                         � ߱            $  M  �  ���                       X�    T  x�  ��  ��  $Q      4   ����$Q      $  U  ��  ���                       DQ                         � ߱            �   r  XQ      �Q     
                R                     dS  @        
 $S              � ߱        ��  V   �  ��  ���                        ��  �   �  pS      0�    ;  ��  Ġ      �S      4   �����S      /   <  �      �                          3   �����S             �                      3   �����S  �  $  @  \�  ���                       �S                         � ߱        (T     
                �T                     �U  @        
 �U              � ߱        �  V   J  ��  ���                        ��    �  4�  ��       V      4   ���� V                ��                      ��                  �  �                  f                       �  D�      g   �  آ         A���                           ��          p�  X�      ��                  �      ��              pf                    O   ����    e�          O   ����    R�          O   ����    ��          /  �  ̣     ܣ  (V                      3   ����V  �     
   ��                      3   ����4V         
   ,�                      3   ����<V    ��                              ��        �                  ����                                        �              9      <�                      g                                �  g   �  �          A�	��                           إ          ��  ��      ��                  �  �  ��              �h                    O   ����    e�          O   ����    R�          O   ����    ��          /  �  �     �  `V                      3   ����DV            4�                      3   ����hV    ��                              ��        �                  ����                                        $�              :      D�                      g                               �  g   �  �          A�	��                           �          ��  ��      ��                  �  �  ȧ              �f                    O   ����    e�          O   ����    R�          O   ����    ��          /  �  �     �  �V                      3   �����V            <�                      3   �����V    ��                              ��        �                  ����                                        ,�              ;      L�                      g                               h�    �  $�  ��      �V      4   �����V                ��                      ��                  �                    |�f                       �  4�  �  /   �  ܩ     �                          3   �����V            �                      3   �����V  �  /  �  H�     X�  0W                      3   ����W  ��     
   x�                      3   ����8W  ��        ��                      3   ����@W  �        ت                      3   ����TW            �                      3   ����xW  @�    �  4�  D�      �W      4   �����W      /  �  p�     ��  $X                      3   ����X  ��     
   ��                      3   ����,X  �        Ы                      3   ����4X  �         �                      3   ����HX            0�                      3   ����lX           \�  l�      �X      4   �����X      /    ��     ��  �X                      3   �����X  ج     
   Ȭ                      3   �����X  �        ��                      3   �����X  8�        (�                      3   ����Y            X�                      3   ���� Y  (�      ��   �      DY      4   ����DY                �                      ��                                      8�f                         ��      g     (�         A�̯        TY                  �          ��  ��      ��                        خ              ��f                    O   ����    e�          O   ����    R�          O   ����    ��          /    �     ,�  xY                      3   ����`Y  \�     
   L�                      3   �����Y         
   |�                      3   �����Y    ��                            ����                                        <�              <      ��                      g                               ��       �Y                                     �Y     
                $Z                     t[  @        
 4[              � ߱        P�  V   �  \�  ���                        �[     
                \                     T]  @        
 ]              � ߱        |�  V   �  �  ���                         �    �  ��  ��      h]      4   ����h]      $   �  Ա  ���                       �]  @         �]              � ߱        Գ  g   �  �         A�x�        �]  A�x�        �]                  ��          Ĳ  ��      ��                  �  �  ܲ                                  O   ����    e�          O   ����    R�          O   ����    ��            �  �   �      �]      4   �����]      O  �  ������  ^    ��                            ����                                        @�              =      8�                      g                               ��  g     �         A6$�         ^                  ��          ��  l�      ��                    	  ��              �                    O   ����    e�          O   ����    R�          O   ����    ��      ̴      (^  }          O    ������  <^    ��                            ����                                         �              >      �                      g                               ��  g     ��         A"H�                           `�          0�  �      ��0�                T  H�              X�                    O   ����    e�          O   ����    R�          O   ����    ��      ��  	    ��                         \^            3   ����P^  �  V     ж  ���                                                    ߱                    �      $�  4�      h^      4   ����h^      O    ������  �^                                                       	       	               
       
                                                       � ߱        0�  $     L�   �                       �  A         ��   ��         ��  �^                                         �^   �^   �^                 �  �           �^  �^  �^           �^  �^  �^         �            ��   ̸    ��    !   �  ��      D_      4   ����D_                ع                      ��                  !  B                  8g                       !  0�  L_                         � ߱        p�  V   "  ��  ���                              ��      L�          �  �      ��                 $  &  4�              �8g                ��     $  �      ��  ��       ��                            7   ����        ��               �_    �            L�                  6   $       ��   ��         p�  �_    �            L�                                                        X_   d_   p_   |_                   �  �           �_  �_  �_  �_           �_  �_  �_  �_                      ��   Ȼ        O   ����  e�          O   ����  R�          O   ����  ��          V   %  x�  ���                        0`                         � ߱        �  	  '  ؼ                                        3   ����<`  ��  A  (       P�   ��         8�  �`                                         H`   T`   ``                 ��  ��           l`  |`  �`           t`  �`  �`         �            l�   ��          ,  ؽ  T�      �`      4   �����`                ��                      ��                  ,  A                  �g                       ,  �  �`                         � ߱        (�  V   -  d�  ���                              8�      �          ��  ��      ��                 /  1  ��              |�g                \�     /  ��      d�  ��       ��                            7   ����        ��               la    �            �                  6   /       H�   ��         (�  la    �            �                                                        �`   a   a    a                   ��  ��           ,a  <a  La  \a           4a  Da  Ta  da                      d�   ��        O   ����  e�          O   ����  R�          O   ����  ��          V   0  0�  ���                        �a                         � ߱        ��  	  2  ��                                        3   �����a  ��  A  3       �   ��         ��  �b                                         �a   �a   b   b   $b                 |�  p�           4b  Db  Tb  db  tb           <b  Lb  \b  lb  |b         �            0�   P�          9  ��  (�      c      4   ����c                d�                      ��                  9  @                  t�g                       9  ��  c                         � ߱        ��  V   :  8�  ���                              �      ��          ��  ��      ��                 <  >  ��              �g                L�     <  ��      8�  ��       ��                            7   ����        ��               �c    �            ��                  6   <       (�   ��         ��  �c    �            ��                                                        c   (c   4c   @c   Lc   Xc                   ��  ��           dc  tc  �c  �c  �c  �c           lc  |c  �c  �c  �c  �c                      D�   h�        O   ����  e�          O   ����  R�          O   ����  ��          V   =   �  ���                        dd                         � ߱            	  ?  ��                                        3   ����pd  ��  8  C     ��  8  D     ��  8  E     ��  8  F     �  8  G     |d           h          �d           h          �d           h          �d           h          �d           h          �d  	         h          �d           h          �d           h          �d           h          �d           h              � ߱            Z   H  ��   �                                      h�                                           ��                              ��        �                   ��                             ��                             ��                            ����                            8�    @�          T�          ��  4�         ?     p�                      g   l�                          ��  g   \  ��         A <�                           ��          T�  <�      ��                  ]  _  l�              �g                    O   ����    e�          O   ����    R�          O   ����    ��      �d                            � ߱            Z   ^  ��   �                          ��                              ��        �                  ����                                        ��              @      ��                      g                               l�  g   g  ��         A �                           x�          H�  0�      ����               h  �  `�              ��g                    O   ����    e�          O   ����    R�          O   ����    ��      ��    i  ��  ��      e      4   ����e      O   i  ��  ��  8e  ��  A  j        $�   ��         �  �e                                         Le   Xe   le                 |�  p�           �e  �e  �e           �e  �e  �e         �            @�   X�          n  ��  (�  ,�  �e      4   �����e                ��                      ��                  n  �                  d�g                       n  ��  f                        f                        Df                            � ߱        ��  Z   o  8�   �                        ��  A  t        8�   ��         �  �f                                         Pf   df   pf   |f                   ��  ��           �f  �f  �f           �f  �f  �f         �            T�   l�          x  ��  <�      g      4   ����g                 �                      ��                  x  �                  0�g                       x  ��  g                        (g                        4g                        @g                        tg   	                     �g   
                     �g                            � ߱        L�  Z   y  L�   �                        <�  A  �        ��   ��        	 ��  0h                                         �g   �g   �g   �g   �g                 (�  �           �g  �g   h  h   h           �g  �g  h  h  (h         �            ��   ��          �  X�  ��      �h      4   �����h  �h                            � ߱            Z   �  h�   �                                      <�                      ��                  �  �                  (�g                       �  ��  ��  	  �  p�                                        3   �����h      O  �  ������  �h    ��                              ��        �                  ����                             �    �                      ��              A      ��                      g                               �  g   �  ��         A ��                           L�          �  �      ����               �  �  4�              ��g                    O   ����    e�          O   ����    R�          O   ����    ��      ��    �  h�  x�      �h      4   �����h      O   �  ��  ��  i  d�  A  �        ��   ��        
 ��  �i                                         $i   0i   Di                 P�  D�           Xi  hi  xi           `i  pi  �i         �            �   ,�          �  ��  ��  ��  �i      4   �����i                ��                      ��                  �  �                  x�g                       �  ��  �i                        �i  �           j                        $j                        Xj   	                     dj   
                     pj                            � ߱         �  Z   �  �   �                        ��  A  �        t�   ��         P�  k                                         |j   �j   �j   �j   �j                 ��  ��           �j  �j  �j  �j  k           �j  �j  �j  �j  k         �            ��   ��          �  �  H�      �k      4   �����k  �k                            � ߱            Z   �  �   �                                      ��                      ��                  �  �                  0�g                       �  t�  4�  	  �  $�                                        3   �����k      O  �  ������  �k    ��                              ��        �                  ����                            ��                      ��              B      L�                      g                                     �  4�  ��      �k      4   �����k                $�                      ��                  �  �                  ȫg                       �  D�  �k  @                     l  @         �k          0l  @         l              � ߱        P�  $   �  ��  ���                       L�  g   �  h�         An��      }                      0�           �  ��      ��                  �  �  �              <�g                    O   ����    e�          O   ����    R�          O   ����    ��      l�  /  �  \�                                 3   ����<l        �  ��  ��      Xl      4   ����Xl      O  �  ������  �l    ��                            ����                                        |�              C      ��                      g                                �  g   �  d�         A!��         �l                  X�          ��  ��      ��                  �  �  �              �g                    O   ����    e�          O   ����    R�          O   ����    ��      �l  @                         � ߱            $  �  ,�  ���                         ��                            ����                                        x�              D      ��                      g                               \�  /   �  L�                                 3   �����l        �  x�  ��      �l      4   �����l                p�                      ��                  �  �                  ��g                       �  ��                ��          ��  ��      ��                 �  �                  ��g                       �  �      O   �    ��          O   �    ��      ��  /   �  ��                                 3   �����l        �  �  �      m      4   ����m      k   �  4�              }       n        �   adm-create-objects  H�  L�                      E      �                               �                     disable_UI  `�  ��                      F      <                              �  
                   enable_UI   ��  $�                      G      �                              �  	                   exitObject  0�  ��                      H      �                               �  
                    �� �   �FAC   G/R���     ���  �        @�  8   ����   P�  8   ����   h�  8   ����   x�  8   ����   ��    ��  8   ����   ��  8   ����   ��    ��  8   ����   ��  8   ����       8   ����       8   ����             ��  ��      toggleData  ,INPUT plEnabled LOGICAL    ��   �  8�      showMessageProcedure    ,INPUT pcMessage CHARACTER,OUTPUT plAnswer LOGICAL  �  |�  ��      returnFocus ,INPUT hTarget HANDLE   l�  ��  ��      repositionObject    ,INPUT pdRow DECIMAL,INPUT pdCol DECIMAL    ��   �  �      removeLink  ,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE ��  `�  p�      removeAllLinks  ,   P�  ��  ��      modifyUserLinks ,INPUT pcMod CHARACTER,INPUT pcLinkName CHARACTER,INPUT phObject HANDLE t�  ��   �      modifyListProperty  ,INPUT phCaller HANDLE,INPUT pcMode CHARACTER,INPUT pcListName CHARACTER,INPUT pcListValue CHARACTER    ��  x�  ��      hideObject  ,   h�  ��  ��      editInstanceProperties  ,   ��  ��  ��      displayLinks    ,   ��  ��  ��      createControls  ,   ��  �  �      changeCursor    ,INPUT pcCursor CHARACTER   ��  H�  T�      applyEntry  ,INPUT pcField CHARACTER    8�  ��  ��      adjustTabOrder  ,INPUT phObject HANDLE,INPUT phAnchor HANDLE,INPUT pcPosition CHARACTER p�  ��  ��      addMessage  ,INPUT pcText CHARACTER,INPUT pcField CHARACTER,INPUT pcTable CHARACTER ��  L�  T�      addLink ,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE <�  ��  ��      unbindServer    ,INPUT pcMode CHARACTER ��  ��  ��      startServerObject   ,   ��  �  �      runServerObject ,INPUT phAppService HANDLE  ��  D�  X�      restartServerObject ,   4�  l�  ��      initializeServerObject  ,   \�  ��  ��      disconnectObject    ,   ��  ��  ��      destroyServerObject ,   ��  ��  ��      bindServer  ,   ��  �  �      processAction   ,INPUT pcAction CHARACTER   ��  D�  T�      enableObject    ,   4�  h�  x�      disableObject   ,   X�  ��  ��      applyLayout ,   |�  ��  ��      viewPage    ,INPUT piPageNum INTEGER    ��  ��  ��      viewObject  ,   ��  �  �      toolbar ,INPUT pcValue CHARACTER    ��  8�  D�      selectPage  ,INPUT piPageNum INTEGER    (�  p�  ��      removePageNTarget   ,INPUT phTarget HANDLE,INPUT piPage INTEGER `�  ��  ��      passThrough ,INPUT pcLinkName CHARACTER,INPUT pcArgument CHARACTER  ��  �   �      notifyPage  ,INPUT pcProc CHARACTER �  H�  T�      initPages   ,INPUT pcPageList CHARACTER 8�  ��  ��      initializeVisualContainer   ,   p�  ��  ��      initializeObject    ,   ��  ��  ��      hidePage    ,INPUT piPageNum INTEGER    ��  �   �      destroyObject   ,    �  4�  @�      deletePage  ,INPUT piPageNum INTEGER    $�  l�  |�      createObjects   ,   \�  ��  ��      constructObject ,INPUT pcProcName CHARACTER,INPUT phParent HANDLE,INPUT pcPropList CHARACTER,OUTPUT phObject HANDLE ��  �   �      confirmExit ,INPUT-OUTPUT plCancel LOGICAL  �  P�  \�      changePage  ,   @�  p�  ��      assignPageProperty  ,INPUT pcProp CHARACTER,INPUT pcValue CHARACTER      � 
"     
 `%     adecomm/as-utils.w 
"   
   �    }        �
"     
       �     }        �G� �   �G%              � �  .   %              %        %        %       %        %       %               %               %               %              %              %              %               %              
�        
"   
 `
�    
"   
 `
"   
 N    �        �     �        �    
"   
   �        0         �     }        �%              
"   
 `
"   
 N    �        �     �        �    
"   
   �        �         �     }        �%              � 
" 
   
 P%              � �  �         X      $              
�    � �   P     
"   
                       
�            � �   N
" 
   
 N
�H T   %              �     }        �GG %              � 
"   
   P �L 
�H T   %              �     }        �GG %              
"   
   �        �    7%               
"   
 �           �    1�   
 �    P%               o%   o           �     
"   
 �           0    1�    �    P%               o%   o           � +   
"   
 �           �    1� 2  
 �    P%               o%   o           � =   
"   
 �               1� I   �    P%               o%   o           � W   
"   
 �           �    1� ^   �    P%               o%   o           � m   
"   
 �                1� �   � �   P%               o%   o           %               
"   
 P�          |    1� �   P� �     
"   
 �           �    1� �   �    P%               o%   o           � �  e 
"   
 �           ,    1� (   �    P%               o%   o           � 7  [ 
"   
 �           �    1� �   � �   P%               o%   o           %               
"   
 �           	    1� �   � �   P%               o%   o           %               
"   
 �           �	    1� �   � �   P%               o%   o           %              
"   
 P�          
    1� �   P� �     
"   
 �           P
    1� �  
 � �   P%               o%   o           %               
"   
 �           �
    1� �   �    P%               o%   o           �     
"   
 P�          @    1� �   P� �     
"   
 �           |    1� �   �    P%               o%   o           � 
  t 
"   
 P�          �    1�   
 P� �     
"   
 �           ,    1� �   �    P%               o%   o           � �  � 
"   
 �           �    1� (   �    P%               o%   o           �     
"   
 �               1� ?  
 � J   P%               o%   o           %               
"   
 �           �    1� N   � �   P%               o%   o           %               
"   
 �               1� V   �    P%               o%   o           �     
"   
 �           �    1� g   �    P%               o%   o           o%   o           
"   
 �           �    1� w  
 �    P%               o%   o           �     
"   
 �           p    1� �   � �  	 P%               o%   o           � �  / 
"   
 P�          �    1� �   P� �  	   
"   
 �                1� �   � �  	 Po%   o           o%   o           �     
"   
 P�          �    1� �   P� �  	   
"   
 �           �    1�    � �  	 Po%   o           o%   o           �     
"   
 P�          D    1�    P� �     
"   
 P�          �    1�    P� �  	   
"   
 P�          �    1� ,   P� �  	   
"   
 P�          �    1� 9   P� �  	   
"   
 �           4    1� G   � �   Po%   o           o%   o           %              
"   
 P�          �    1� X   P� �  	   
"   
 P�          �    1� f  
 P� q     
"   
 P�          (    1� y   P� �  	   
"   
 P�          d    1� �   P� �  	   
"   
 P�          �    1� �   P� �  	   
"   
 P�          �    1� �   P� �  	   
"   
 P�              1� �  	 P� �  	   
"   
 P�          T    1� �   P� �  	   
"   
 P�          �    1� �   P� �  	   
"   
 �           �    1� �   �    P%               o%   o           o%   o           
�H T   %              �     }        �GG %              
"   
   
"   
 
"   
   
"   
 N(�  L ( l       �        �    �� �   � P   �        �    �@    
� @  , 
�       �    ��      p�               �L
�    %              � 8      �    � $         �           
�    � )     
"   
 �� @  , 
�       �    �� 2  
 �p�               �L"      P �L 
�H T   %              �     }        �GG %              
"   
 �           t    1� ,  
 �    P%               o%   o           �     
"   
 �           �    1� 7  
 �    P%               o%   o           o%   o           
"   
 �           d    1� B   � �   P%               o%   o           o%   o           
"   
 �           �    1� K   � �   P%               o%   o           %               
"   
 �           \    1� Z   � �   P%               o%   o           %               
"   
 �           �    1� g   �    P%               o%   o           �     
"   
 `�           L    1� n   `� �   P%               o%   o           %              
"   
 `�           �    1� �   `� �   P%               o%   o           o%   o           
"   
 �           D    1� �   �    P%               o%   o           o%   o           
"   
 �           �    1� �  	 �    P%               o%   o           �     
"   
 �           4    1� �   �    P%               o%   o           o%   o           
"   
 �           �    1� �   �    P%               o%   o           o%   o           
"   
 �           ,    1� �   � �   P%               o%   o           %               
"   
 �           �    1� �   � �   P%               o%   o           o%   o           P �L 
�H T   %              �     }        �GG %              
"   
 �           x    1� �   � �  	 P%               o%   o           �     
"   
 �           �    1� �   � �  	 P%               o%   o           �     
"   
 �           `    1� �   � �   P%               o%   o           %               
"   
 �           �    1�    � �  	 P%               o%   o           �     
"   
 �           P     1�    � �  	 P%               o%   o           �     
"   
 �           �     1� )   � �   P%               o%   o           %               
"   
 `�           @!    1� 7   `� �  	 P%               o%   o           �     
"   
 �           �!    1� F   � �  	 P%               o%   o           �     `
"   
 �           ("    1� U   � �  	 P%               o%   o           �     
"   
 �           �"    1� c   � �  	 P%               o%   o           o%   o           
"   
 �           #    1� q   � �  	 P%               o%   o           �     
"   
 �           �#    1� �   � �  	 P%               o%   o           �     
"   
 �            $    1� �  	 � q   P%               o%   o           %               
"   
 �           |$    1� �   � q   P%               o%   o           %               
"   
 �           �$    1� �   � �   P%               o%   o           o%   o           
"   
 �           t%    1� �   � �   P%               o%   o           o%   o           
"   
 �           �%    1� �   � �   P%               o%   o           %               
"   
 �           l&    1� �   � �   P%               o%   o           %               
"   
 �           �&    1� �   � �   P%               o%   o           %               
"   
 �           d'    1� �   �    P%               o%   o           %       
       
"   
 �           �'    1� 
   �    P%               o%   o           o%   o           
"   
 �           \(    1�    �    P%               o%   o           %              
"   
 �           �(    1� "   �    P%               o%   o           o%   o           
"   
 �           T)    1� .   �    P%               o%   o           %              
"   
 �           �)    1� ;   �    P%               o%   o           o%   o           
"   
 �           L*    1� H   �    P%               o%   o           %              
"   
 �           �*    1� P   �    P%               o%   o           o%   o           
"   
 �           D+    1� X   � �  	 P%               o%   o           �     P �L 
�H T   %              �     }        �GG %              
"   
 �           ,    1� j   � J   P%               o%   o           %               
"   
 �           �,    1� v   � J   P%               o%   o           o%   o           
"   
 �           -    1� �   �    P%               o%   o           �     
"   
 �           x-    1� �   �    P%               o%   o           � �  - 
"   
 �           �-    1� �   �    P%               o%   o           �     
"   
 �           `.    1� �   �    P%               o%   o           � 
   
"   
 P�          �.    1� (   P� �     
"   
 `�           /    1� 9   `�    P%               o%   o           �     
"   
 P�          �/    1� E  
 P� �     
"   
 P�          �/    1� P   P� �     
"   
 �           �/    1� ]   � �  	 P%               o%   o           �     
"   
 �           p0    1� j   �    P%               o%   o           �     
"   
 �           �0    1� w   � �   P%               o%   o           o%   o           
"   
 �           `1    1� �   �    P%               o%   o           � �  ! 
"   
 �           �1    1� �   �    P%               o%   o           �     
"   
 �           H2    1� �   �    P%               o%   o           � �   
"   
 �           �2    1� �  	 � J   P%               o%   o           o%   o           
"   
 �           83    1� �   � �   P%               o%   o           %               
"   
 P�          �3    1� �   P� �     
"   
 �           �3    1�    �    P%               o%   o           �     
"   
 �           d4    1� /   � �  	 P%               o%   o           �     
"   
 �           �4    1� <   � �  	 P%               o%   o           �     
"   
 P�          L5    1� L   P� �     
"   
 P�          �5    1� ^   P� �  	   
"   
 �           �5    1� q   � �   Po%   o           o%   o           %               
"   
 P�          @6    1� �   P� �     
"   
 P�          |6    1� �   P� �  	   
"   
 P�          �6    1� �   P� �  	   
"   
 P�          �6    1� �   P� �  	   
"   
 P�          07    1� �   P� �  	   
"   
 P�          l7    1� �   P� �  	   
"   
 P�          �7    1� �   P� �     
"   
 �           �7    1�    �    P%               o%   o           �   4 
"   
 P�          X8    1� P   P� �     
"   
 P�          �8    1� ]   P� �     
"   
 P�          �8    1� m   P� �     
"   
 P�          9    1� z   P� �  	   
"   
 P�          H9    1� �   P� �  	   
"   
 P�          �9    1� �   P� �  	   
"   
 P�          �9    1� �   P� �     
"   
 �           �9    1� �   � �  	 P%               o%   o           �     
"   
 �           p:    1� �   � �  	 P%               o%   o           �     
"   
 �           �:    1� �   � �  	 P%               o%   o           �     
"   
 �           X;    1� �   � �  	 P%               o%   o           �     
"   
 �           �;    1�    � �   P%               o%   o           %               
"   
 �           H<    1�    � �   P%               o%   o           o%   o           
"   
 �           �<    1� #   � �   P%               o%   o           %               
"   
 �           @=    1� 3   � �   P%               o%   o           %               
"   
 �           �=    1� ?   � �   P%               o%   o           o%   o           
"   
 �           8>    1� Z   � �   P%               o%   o           %               
"   
 P�          �>    1� h   P� �  	   
"   
 �           �>    1� v   � �   P%               o%   o           %              
"   
 P�          l?    1� �   P� �  	   
"   
 P�          �?    1� �   P� �  	   
"   
 P�          �?    1� �  
 P� �  	   
"   
 �            @    1� �   � �  	 P%               o%   o           �    
"   
 �           �@    1� �   � �  	 P%               o%   o           �     
"   
    "    P%     start-super-proc �P%     adm2/smart.p BNP �L 
�H T   %              �     }        �GG %              
"   
   �       �A    6� �     
"   
   
�        �A    8
"   
   �         B    ��     }        �G 4              
"   
 ߱G %              G %              %p e `   LogicalObjectName,PhysicalObjectName,DynamicObject,RunAttribute,HideOnInit,DisableOnInit,ObjectLayout N
�H T   %              �     }        �GG %              
"   
 N
"   
 P
"   
 N
"   
   (�  L ( l       �        HC    �� �   � P   �        TC    �@    
� @  , 
�       `C    ��    Np�               �L
�    %              � 8      lC    � $         �           
�    � )   N
"   
 �p� @  , 
�       |D    �� �   �p�               �L"    , �   � �   � �   P�     }        �A      |    "      � �   %              (<   \ (    |    �     }        �A�     �A"        "    N"      < "    N"    (    |    �     }        �A�     �A"    
�H T   %              �     }        �GG %              
"   
 N
"   
 P
"   
 N
"   
   (�  L ( l       �        PF    �� �   � P   �        \F    �@    
� @  , 
�       hF    ��    Np�               �L
�    %              � 8      tF    � $         �           
�    � )   N
"   
 �p� @  , 
�       �G    ��   
 �p�               �L"    , 
�H T   %              �     }        �GG %              
"   
 N
"   
 P
"   
 N
"   
   (�  L ( l       �        (H    �� �   � P   �        4H    �@    
� @  , 
�       @H    ��    Np�               �L
�    %              � 8      LH    � $         �           
�    � )   N
"   
 �p� @  , 
�       \I    �� �   �p�               �L
"   
 , 
�H T   %              �     }        �GG %              
"   
   
"   
 g
"   
   
"   
   (�  L ( l       �         J    �� �   � P   �        J    �@    
� @  , 
�       J    ��      p�               �L
�    %              � 8      $J    � $         �           
�    � )     
"   
 �p� @  , 
�       4K    �� 2  
 �p�               �L%     SmartWindow 
"   
   p� @  , 
�       �K    �� I     p�               �L%      WINDOW  
"   
  p� @  , 
�       �K    ��     p�               �L%               
"   
  p� @  , 
�       XL    �� �    p�               �L(        �       �       �       �     }        �A
�H T   %              �     }        �GG %              
"   
  (   � 
"   
 N    �        8M    �� �   �
"   
   � 8      �M    � $         �           
�    � )   N
"   
   �        �M    �
"   
   �       �M    /
"   
   
"   
   �       (N    6� �     
"   
   
�        TN    8
"   
   �        tN    �
"   
   �       �N    �
"   
   p�    � )   
�    �     }        �G 4              
"   
 ߱G %              G %              
�     }        �
"   
    (   � 
"   
 N    �        XO    �A"    �A
"   
   
�        �O    �@ � 
"   
 "      �       }        �
"   
 P%              %                "    P%     start-super-proc �P%     adm2/appserver.p ��    � �     
�    �     }        �%               %      Server  - �     }        �    "    �     P%                   "    �     P%      NONE    p�,  8         $     "            � �   N
�    
�H T   %              �     }        �GG %              
"   
 N
"   
 P
"   
 N
"   
   (�  L ( l       �        �Q    �� �   � P   �        �Q    �@    
� @  , 
�       �Q    ��    Np�               �L
�    %              � 8      R    � $         �           
�    � )   N
"   
 �p� @  , 
�       S    �� �   �p�               �L"    , p�,  8         $     "            � �   N
�     "    P%     start-super-proc �P%     adm2/visual.p N�   � �     � �     � �  U   
�H T   %              �     }        �GG %              
"   
 N
"   
 P
"   
 N
"   
   (�  L ( l       �        tT    �� �   � P   �        �T    �@    
� @  , 
�       �T    ��    Np�               �L
�    %              � 8      �T    � $         �           
�    � )   N
"   
 �p� @  , 
�       �U    �� 7   �p�               �L"    , � 
"    
 P%     contextHelp 
"    
   
�    
�    %     processAction   
�    %     CTRL-PAGE-UP AN%     processAction   
�    %     CTRL-PAGE-DOWN  "    P%     start-super-proc �P%     adm2/containr.p %     modifyListProperty 
�    
�    %      Add     %      ContainerSourceEvents %      initializeDataObjects 0 0   A    �    � �   
�    � �   PA    �    � �     
�    � �   P%     modifyListProperty 
�    
�    %      Add     %      ContainerSourceEvents g%     buildDataRequest ent0 A    �    � �   P
�    � �   %     modifyListProperty 
�    
�    %      Add     %     SupportedLinks %      ContainerToolbar-Target � 
"    
 P
"   
 P%     contextHelp 
"    
   
�    
�    %               
�H T   %              �     }        �GG %              
"   
 N
"   
 P
"   
 N
"   
 (�  L ( l       �        �Y    �� �   � P   �         Z    �@    
� @  , 
�       Z    ��    Np�               �L
�    %              � 8      Z    � $         �    N     
�    � )   P
"   
 �p� @  , 
�       ([    �� L   �p�               �L
�             �G
�H T   %              �     }        �GG %              
"   
 N
"   
 P
"   
 N
"   
 N(�  L ( l       �        �[    �� �   � P   �        �[    �@    
� @  , 
�       �[    ��    Np�               �L
�    %              � 8      �[    � $         �    N     
�    � )   N
"   
 �p� @  , 
�       ]    ��    �p�               �L%              (        �     }        �G� �   �G� 
"   
 N
"   
   �        �]    �%              
"   
 P
"   
 P�     }        �%               
"   
 P%      CLOSE   %               � 
     "          "    %               %               "     T"    T"    T&    &    &    &    &    &    0        %              %              %              *    "  	    "      "  1    "      "      &    &    &    &    &    &    &    &    L    0        %              %              %              %              "      � /  
   "     T"    T"    T&    &    &    &    &    &    0        %              %              %              *    "  
    "      "  1    "      "      &    &    &    &    &    &    &    &    L    0        %              %              %              %              "      � :     � S   T%              "       "       "  <    &    &    &    &    &    &    &    &    &    &    h    L    0        %              %              %              %              %              *    "  
    "      "      "      "      "      "      &    &    &    &    &    &    &    &    &    &    &    &    �    h    L    0        %              %              %              %              %              %              "      � ]  	   � g    P� g      � g      � g    P� g      � g    N� g      � g    P� g      � g    N      �     }        B    �     }        B� g    B%               "     T�             B�     }        B&    &    &    &    &    &    0        %              %              %              *    "                 "  '    � m     "  (    "      %              "       "  '    T    &    "  (    &    &    &    &    &    &    0        %              %              %              *    "      "      "                 "  '    � m     "  (    "      "      "  <    � S   T%              "       "       "  <    &    &    &    &    &    &    &    &    &    &    h    L    0        %              %              %              %              %              *    "      � o     %                   �     }        B� g    B%               "     T�            B�     }        B&    &    &    &    &    &    0        %              %              %              *    "          #     Fecha    %              "                "  '    � m     "  (    "      "      "  <    � S   T%              "       "       "  <    &    &    &    &    &    &    &    &    &    &    h    L    0        %              %              %              %              %              *    "      � �     %               � 
"   
 P
"   
 g
"   
 N�        �k    %%              
�     }        �
"   
   %     destroyObject       �     }        �    �  � �  	   %               
"   
 P
�    %     createObjects    �     }        �%     initializeObject �P �     }        �(        �     }        �G� �   �G� 
"   
 N
"   
   �     }        �
�    
"   
 N"    "    N"  	    "      "      "      "      "  
    "      "      "    P"    P"    P
"   
 `
"   
   %      CLOSE   %                               �           �   l       ��                 s  �  �                pg                    O   ����    e�          O   ����    R�          O   ����    ��        $  �  �   ���                       �L     
                    � ߱              �  (  �      DM      4   ����DM                �                      ��                  �  �                  tg                       �  8  �  �  �  �M            �  �  `      �M      4   �����M                p                      ��                  �  �                   g                       �  �  �  o   �      ,                                 �  �   �  N      �  �   �  4N      $  $  �  �  ���                       `N     
                    � ߱        8  �   �  �N      L  �   �  �N      `  �   �  �N          $   �  �  ���                       �N  @         �N              � ߱                     T          ,  @   T �            
             
             
             
                 $   4   D          $   4   D   ����        ��                            ����                                            �           �   l       ��                 �  �  �               �!g                    O   ����    e�          O   ����    R�          O   ����    ��      J                      �          �  $  �    ���                       DO     
                    � ߱                  �  �                      ��                   �  �                  �g                     �  4      4   ����dO      $  �  �  ���                       �O     
                    � ߱        �    �  4  D      �O      4   �����O      /  �  p                               3   �����O  �  �   �  �O          O   �  ��  ��  P                               , �                          
                               �      ��                            ����                                                        �   l       ��                      �               �g                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                                            �           �   l       ��                    !  �               �g                    O   ����    e�          O   ����    R�          O   ����    ��             �   �        m      4   ���� m      n        �          `m             ,      lm      4   ����lm      �      �m    ��                            ����                                            �          �   l       ��                  '  ;  �               �g                    O   ����    e�          O   ����    R�          O   ����    ��      �m  �           �m  �          �m  �          �m  �          �m  �          �m  �          �m  �          �m  �          �m  �           n  � 	         n  � 
         n  �          $n  �              � ߱        0  Z   1  �    �        �m                  �               �              �              �              �              �              � ߱        \  h   6  �   �        0n                  
   :  �� x             <n    ��                              ��        �                  ����                                            �           �   l       ��                  A  K  �               ��g                    O   ����    e�          O   ����    R�          O   ����    ��      �     H  Hn  }          O   I  ��  ��  \n    ��                            ����                               :   d d     P   �� (�!(  � �                                               �                                                                         d     D                                                                  x  L�  l                                                         �     �                      �  �  �  
 X  �� xd                                                        @     �  	    P   �� ud                                                           �  G   
 X  �� xd                                                        "     �  
    P   �;d                                                           �  G   
 X  �;4d                                                        m     �      P   ���d                                                           �  G   
 X  ���d                                                        �     �     
 X  L~ d                                                              �     
 X  �~xd                                             
           O     �  	    P   �~ud                                                           �  G   
 X  �~xd                                                        1     �  
    P   ��d                                                           �  G   
 X  ��4d                                                        |     �      P   �V�d                                                           �  G   
 X  �V�d 	                                                       �     �      P   ���d                                                           �  G   
 X  ���d 
                                                       �     �     
 X  �xd                                                        ^     �     
 X  ��xd                                                             �  
    \  L-p                                 �                                  @       D                                                                    TXS appSrvUtils B-DOCU CcbCDocu ASSIGNFOCUSEDWIDGET ASSIGNWIDGETVALUE ASSIGNWIDGETVALUELIST BLANKWIDGET CLEARWIDGET DISABLERADIOBUTTON DISABLEWIDGET ENABLERADIOBUTTON ENABLEWIDGET FORMATTEDWIDGETVALUE FORMATTEDWIDGETVALUELIST HIDEWIDGET HIGHLIGHTWIDGET RESETWIDGETVALUE TOGGLEWIDGET VIEWWIDGET WIDGETHANDLE WIDGETLONGCHARVALUE WIDGETISBLANK WIDGETISFOCUSED WIDGETISMODIFIED WIDGETISTRUE WIDGETVALUE WIDGETVALUELIST s-codcia wWin BUTTON-1 COMBO-BOX-CodFac FAC BOL TCK FILL-IN-CliFac FILL-IN-CliGui FILL-IN-CodAlm FILL-IN-CodGui G/R FILL-IN-FchAlm FILL-IN-FchFac FILL-IN-FchGui FILL-IN-NroFac FILL-IN-NroGui FILL-IN-NroSal FILL-IN-RefFac FILL-IN-RefGui fMain X(256) X(9) 99/99/9999 GUI CORREGIR FECHA DE FACTURA (SOLO PARA SISTEMAS) DISABLEPAGESINFOLDER ENABLEPAGESINFOLDER GETCALLERPROCEDURE GETCALLERWINDOW GETCONTAINERMODE GETCONTAINERTARGET GETCONTAINERTARGETEVENTS GETCURRENTPAGE GETDISABLEDADDMODETABS GETDYNAMICSDOPROCEDURE GETFILTERSOURCE GETMULTIINSTANCEACTIVATED GETMULTIINSTANCESUPPORTED GETNAVIGATIONSOURCE GETNAVIGATIONSOURCEEVENTS GETNAVIGATIONTARGET GETOUTMESSAGETARGET GETPAGENTARGET GETPAGESOURCE GETPRIMARYSDOTARGET GETREENABLEDATALINKS GETRUNDOOPTIONS GETRUNMULTIPLE GETSAVEDCONTAINERMODE GETSDOFOREIGNFIELDS GETTOPONLY GETUPDATESOURCE GETUPDATETARGET GETWAITFOROBJECT GETWINDOWTITLEVIEWER GETSTATUSAREA PAGENTARGETS SETCALLEROBJECT SETCALLERPROCEDURE SETCALLERWINDOW SETCONTAINERMODE SETCONTAINERTARGET SETCURRENTPAGE SETDISABLEDADDMODETABS SETDYNAMICSDOPROCEDURE SETFILTERSOURCE SETINMESSAGETARGET SETMULTIINSTANCEACTIVATED SETMULTIINSTANCESUPPORTED SETNAVIGATIONSOURCE SETNAVIGATIONSOURCEEVENTS SETNAVIGATIONTARGET SETOUTMESSAGETARGET SETPAGENTARGET SETPAGESOURCE SETPRIMARYSDOTARGET SETREENABLEDATALINKS SETROUTERTARGET SETRUNDOOPTIONS SETRUNMULTIPLE SETSAVEDCONTAINERMODE SETSDOFOREIGNFIELDS SETTOPONLY SETUPDATESOURCE SETUPDATETARGET SETWAITFOROBJECT SETWINDOWTITLEVIEWER GETOBJECTTYPE SETSTATUSAREA GETALLFIELDHANDLES GETALLFIELDNAMES GETCOL GETDEFAULTLAYOUT GETDISABLEONINIT GETENABLEDOBJFLDS GETENABLEDOBJHDLS GETHEIGHT GETHIDEONINIT GETLAYOUTOPTIONS GETLAYOUTVARIABLE GETOBJECTENABLED GETOBJECTLAYOUT GETROW GETWIDTH GETRESIZEHORIZONTAL GETRESIZEVERTICAL SETALLFIELDHANDLES SETALLFIELDNAMES SETDEFAULTLAYOUT SETDISABLEONINIT SETHIDEONINIT SETLAYOUTOPTIONS SETOBJECTLAYOUT SETRESIZEHORIZONTAL SETRESIZEVERTICAL GETOBJECTTRANSLATED GETOBJECTSECURED CREATEUIEVENTS GETAPPSERVICE GETASBOUND GETASDIVISION GETASHANDLE GETASHASSTARTED GETASINFO GETASINITIALIZEONRUN GETASUSEPROMPT GETSERVERFILENAME GETSERVEROPERATINGMODE RUNSERVERPROCEDURE SETAPPSERVICE SETASDIVISION SETASHANDLE SETASINFO SETASINITIALIZEONRUN SETASUSEPROMPT SETSERVERFILENAME SETSERVEROPERATINGMODE gshAstraAppserver gshSessionManager gshRIManager gshSecurityManager gshProfileManager gshRepositoryManager gshTranslationManager gshWebManager gscSessionId gsdSessionObj gshFinManager gshGenManager gshAgnManager gsdTempUniqueID gsdUserObj gsdRenderTypeObj gsdSessionScopeObj ghProp ghADMProps ghADMPropsBuf glADMLoadFromRepos glADMOk ANYMESSAGE ASSIGNLINKPROPERTY FETCHMESSAGES GETCHILDDATAKEY GETCONTAINERHANDLE GETCONTAINERHIDDEN GETCONTAINERSOURCE GETCONTAINERSOURCEEVENTS GETCONTAINERTYPE GETDATALINKSENABLED GETDATASOURCE GETDATASOURCEEVENTS GETDATASOURCENAMES GETDATATARGET GETDATATARGETEVENTS GETDBAWARE GETDESIGNDATAOBJECT GETDYNAMICOBJECT GETINSTANCEPROPERTIES GETLOGICALOBJECTNAME GETLOGICALVERSION GETOBJECTHIDDEN GETOBJECTINITIALIZED GETOBJECTNAME GETOBJECTPAGE GETOBJECTPARENT GETOBJECTVERSION GETOBJECTVERSIONNUMBER GETPARENTDATAKEY GETPASSTHROUGHLINKS GETPHYSICALOBJECTNAME GETPHYSICALVERSION GETPROPERTYDIALOG GETQUERYOBJECT GETRUNATTRIBUTE GETSUPPORTEDLINKS GETTRANSLATABLEPROPERTIES GETUIBMODE GETUSERPROPERTY INSTANCEPROPERTYLIST LINKHANDLES LINKPROPERTY MAPPEDENTRY MESSAGENUMBER PROPERTYTYPE REVIEWMESSAGES SETCHILDDATAKEY SETCONTAINERHIDDEN SETCONTAINERSOURCE SETCONTAINERSOURCEEVENTS SETDATALINKSENABLED SETDATASOURCE SETDATASOURCEEVENTS SETDATASOURCENAMES SETDATATARGET SETDATATARGETEVENTS SETDBAWARE SETDESIGNDATAOBJECT SETDYNAMICOBJECT SETINSTANCEPROPERTIES SETLOGICALOBJECTNAME SETLOGICALVERSION SETOBJECTNAME SETOBJECTPARENT SETOBJECTVERSION SETPARENTDATAKEY SETPASSTHROUGHLINKS SETPHYSICALOBJECTNAME SETPHYSICALVERSION SETRUNATTRIBUTE SETSUPPORTEDLINKS SETTRANSLATABLEPROPERTIES SETUIBMODE SETUSERPROPERTY SHOWMESSAGE SIGNATURE , prepareInstance ObjectName CHAR  ObjectVersion ADM2.2 ObjectType SmartWindow ContainerType WINDOW PropertyDialog adm2/support/visuald.w QueryObject LOGICAL ContainerHandle HANDLE InstanceProperties LogicalObjectName,PhysicalObjectName,DynamicObject,RunAttribute,HideOnInit,DisableOnInit,ObjectLayout SupportedLinks Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source ContainerHidden ObjectInitialized ObjectHidden ObjectsCreated HideOnInit UIBMode ContainerSource ContainerSourceEvents initializeObject,hideObject,viewObject,destroyObject,enableObject,confirmExit,confirmCancel,confirmOk,isUpdateActive DataSource DataSourceEvents dataAvailable,queryPosition,updateState,deleteComplete,fetchDataSet,confirmContinue,confirmCommit,confirmUndo,assignMaxGuess,isUpdatePending TranslatableProperties ObjectPage INT DBAware DesignDataObject DataSourceNames DataTarget DataTargetEvents CHARACTER updateState,rowObjectState,fetchBatch,LinkState LogicalObjectName PhysicalObjectName LogicalVersion PhysicalVersion DynamicObject RunAttribute ChildDataKey ParentDataKey DataLinksEnabled InactiveLinks InstanceId DECIMAL SuperProcedure SuperProcedureMode SuperProcedureHandle LayoutPosition ClassName RenderingProcedure ThinRenderingProcedure Label cType ADMProps Target WHERE Target = WIDGET-H(" ") AppService ASDivision ASHandle ASHasConnected ASHasStarted ASInfo ASInitializeOnRun ASUsePrompt BindSignature BindScope ServerOperatingMode ServerFileName ServerFirstCall NeedContext ObjectLayout LayoutOptions ObjectEnabled LayoutVariable DefaultLayout DisableOnInit EnabledObjFlds EnabledObjHdls FieldSecurity SecuredTokens AllFieldHandles AllFieldNames MinHeight MinWidth ResizeHorizontal ResizeVertical ObjectSecured ObjectTranslated PopupButtonsInFields ColorInfoBG INTEGER ColorInfoFG ColorWarnBG ColorWarnFG ColorErrorBG ColorErrorFG BGColor FGColor FieldPopupMapping CurrentPage PendingPage ContainerTarget ContainerTargetEvents exitObject,okObject,cancelObject,updateActive ContainerToolbarSource ContainerToolbarSourceEvents toolbar,okObject,cancelObject OutMessageTarget PageNTarget PageSource FilterSource UpdateSource UpdateTarget CommitSource CommitSourceEvents commitTransaction,undoTransaction CommitTarget CommitTargetEvents rowObjectState StartPage RunMultiple WaitForObject DynamicSDOProcedure adm2/dyndata.w RunDOOptions InitialPageList WindowFrameHandle Page0LayoutManager MultiInstanceSupported MultiInstanceActivated ContainerMode SavedContainerMode SdoForeignFields NavigationSource NavigationTarget PrimarySdoTarget NavigationSourceEvents fetchFirst,fetchNext,fetchPrev,fetchLast,startFilter CallerWindow CallerProcedure CallerObject DisabledAddModeTabs ReEnableDataLinks WindowTitleViewer UpdateActive InstanceNames ClientNames ContainedDataObjects ContainedAppServices DataContainer HasDbAwareObjects HasDynamicProxy HideOnClose HideChildContainersOnClose HasObjectMenu RequiredPages RemoveMenuOnHide ProcessList PageLayoutInfo PageTokens DataContainerName WidgetIDFileName ghContainer adm2/smart.p cObjectName iStart / \ . hReposBuffer hPropTable hBuffer hTable deleteProperties ADM-CLONE-PROPS pcProcName hProc START-SUPER-PROC cAppService cASDivision cServerOperatingMode adm2/appserver.p getAppService Server NONE setASDivision setAppService cFields adm2/visual.p   COMBO-BOX-CodFac FILL-IN-NroFac FILL-IN-FchFac FILL-IN-NroGui FILL-IN-FchGui BUTTON-1 CTRL-PAGE-UP CTRL-PAGE-DOWN adm2/containr.p Add initializeDataObjects getSupportedLinks data-target data-source buildDataRequest containertoolbar-target ContainerToolbar-Target CLOSE rpta Continuamos con el proceso? CcbDDocu FACTURA OK GUIA REMISION OK Almcmov S Almdmov KARDEX OK  x(1)   Doc. NO registrado Documento NO registrado iStartPage ADM-ERROR ADM-CREATE-OBJECTS DISABLE_UI ENABLE_UI EXITOBJECT Fecha Referencia Cliente Salida de Almac�n CORREGIR FACTURA llave01 llave04 almc01 almd06 �  �       �'      % �8   ��      0         pcProp      ��      P         pcProp      ��      p         plCancel    �   ��      �         pcProcName  �   ��      �        
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
 hTarget X  ��      L        pcMessage       ��      p        pcMessage       ��      �        plEnabled             �     cType       �     6   �          �                  getObjectType   w	  �	  �	  ,          
   hReposBuffer    L        @  
   hPropTable  h        `  
   hBuffer           |  
   hTable  �  �     7             �                  adm-clone-props �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �              
   hProc             <        pcProcName  �  �  	   8     $      x                  start-super-proc    �  �  �  �  �  �  �  �  �  H  �     9                                   �  �  	     :                                   �  �  �  L	     ;                                   �  �  	  �	     <                                     T	  �	     =                                   �  �  �	  �	     >                                       	            
     rpta    �	  H
  "   ?   �	                                      !  "  $  %  &  '  (  ,  -  /  0  1  2  3  9  :  <  =  >  ?  @  A  B  C  D  E  F  G  H  T  
        @                                   ^  _  �
  8     A                                   i  j  n  o  t  x  y  �  �  �  �  �  �  �  �  �  �    �     B                                   �  �  �  �  �  �  �  �  �  �  �  �  �  |       C                                   �  �  �  �  �  P     D                                   �  �     �     E               �                  adm-create-objects    X  �     F               �                  disable_UI         !  �  (     G                                 enable_UI   1  6  :  ;  �  t     H               h                  exitObject  H  I  K  8  �       �                            �          �  
   appSrvUtils �        �     s-codcia             
   wWin    4             COMBO-BOX-CodFac    X       H     FILL-IN-CliFac  |       l     FILL-IN-CliGui  �       �     FILL-IN-CodAlm  �       �     FILL-IN-CodGui  �       �     FILL-IN-FchAlm      	   �     FILL-IN-FchFac  0    
         FILL-IN-FchGui  T       D     FILL-IN-NroFac  x       h     FILL-IN-NroGui  �       �     FILL-IN-NroSal  �       �     FILL-IN-RefFac  �       �     FILL-IN-RefGui          �  
   gshAstraAppserver   4           
   gshSessionManager   X        H  
   gshRIManager    �        l  
   gshSecurityManager  �  	 	     �  
   gshProfileManager   �  
 
     �  
   gshRepositoryManager             �  
   gshTranslationManager   $          
   gshWebManager   H        8     gscSessionId    l        \     gsdSessionObj   �        �  
   gshFinManager   �        �  
   gshGenManager   �        �  
   gshAgnManager   �        �     gsdTempUniqueID              gsdUserObj  D        0     gsdRenderTypeObj    l        X     gsdSessionScopeObj  �       �  
   ghProp  �       �  
   ghADMProps  �       �  
   ghADMPropsBuf   �       �     glADMLoadFromRepos              glADMOk 0       $  
   ghContainer P       D     cObjectName l       d     iStart  �       �     cAppService �       �     cASDivision �       �     cServerOperatingMode    �       �     cFields               iStartPage  ,     C  $  B-DOCU  H       <  CcbCDocu    d       X  CcbDDocu    |       t  Almcmov          �  Almdmov          A   ~    �  �  �  �  �  �  �  �  E  F  G  H  _  k  l  m  o  q  r  s  w  x  {  |  }  ~  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  <	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  8
  C
  D
  F
  G
  H
  I
  J
  K
  L
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
                   	  
                                            �  �  �  �  �  �  �  �  �  �  �  �    #  H  d  f  {        6  F  G  H  K  L  M  T  U  r  �  �  ;  <  @  J  �  �  �  �  �  �  �  �  �  �  �  �                   �  �  �  �  �      \  g  �  �  �  �  �  �  �  �  �  �  �  �  �  �      H� $ C:\Progress\OpenEdge\src\adm2\windowmn.i �  f!  C:\Progress\OpenEdge\src\adm2\containr.i �  � # %C:\Progress\OpenEdge\src\adm2\custom\containrcustom.i      ��  C:\Progress\OpenEdge\src\adm2\visual.i   \  # " %C:\Progress\OpenEdge\src\adm2\custom\visualcustom.i  �  �<  C:\Progress\OpenEdge\src\adm2\appserver.i    �  �� ! %C:\Progress\OpenEdge\src\adm2\custom\appservercustom.i     I�  C:\Progress\OpenEdge\src\adm2\smart.i    L  Ds   C:\Progress\OpenEdge\gui\fn  �  tw  %C:\Progress\OpenEdge\src\adm2\custom\smartcustom.i   �  Q.  C:\Progress\OpenEdge\gui\set �  ��  C:\Progress\OpenEdge\src\adm2\cntnprop.i   ��  %C:\Progress\OpenEdge\src\adm2\custom\cntnpropcustom.i    D  P  %C:\Progress\OpenEdge\src\adm2\custom\cntnprtocustom.i    �  F>  C:\Progress\OpenEdge\src\adm2\visprop.i  �  �I  %C:\Progress\OpenEdge\src\adm2\custom\vispropcustom.i    ��  %C:\Progress\OpenEdge\src\adm2\custom\visprtocustom.i @  �l 
 C:\Progress\OpenEdge\src\adm2\appsprop.i �  ɏ  %C:\Progress\OpenEdge\src\adm2\custom\appspropcustom.i    �  V  %C:\Progress\OpenEdge\src\adm2\custom\appsprtocustom.i    �  i$  C:\Progress\OpenEdge\src\adm2\smrtprop.i <  �j  C:\Progress\OpenEdge\gui\get p  �  %C:\Progress\OpenEdge\src\adm2\custom\smrtpropcustom.i    �  ��  %C:\Progress\OpenEdge\src\adm2\custom\smrtprtocustom.i    �  ��  C:\Progress\OpenEdge\src\adm2\smrtprto.i    Su  C:\Progress\OpenEdge\src\adm2\globals.i  T  M�  %C:\Progress\OpenEdge\src\adm2\custom\globalscustom.i �  )a  %C:\Progress\OpenEdge\src\adm2\custom\smartdefscustom.i   �  �  C:\Progress\OpenEdge\src\adm2\appsprto.i   ��  %C:\Progress\OpenEdge\src\adm2\custom\appserverdefscustom.i   @  �X 	 C:\Progress\OpenEdge\src\adm2\visprto.i  �  !�  %C:\Progress\OpenEdge\src\adm2\custom\visualdefscustom.i  �  n�  C:\Progress\OpenEdge\src\adm2\cntnprto.i     ;  %C:\Progress\OpenEdge\src\adm2\custom\containrdefscustom.i    4   ~�  C:\Progress\OpenEdge\src\adm2\widgetprto.i   |   e�  !C:\Progress\OpenEdge\gui\adecomm\appserv.i   �   �:    O:\on_st_co\APLIC\VTA\w-corrfchfac.w     �  �      !     �  $   ,!  �   �      <!  �   �     L!     �     \!  �   �     l!     z     |!  �   r     �!       #   �!  �        �!           �!  �   �     �!     �      �!  �   �     �!     �      �!  r   �     "  n   �     "     i  "   ,"  i   d     <"     B     L"  P   )     \"  �         l"     �  !   |"  �   �     �"     �     �"  �   �     �"     ~     �"  �   |     �"     Z     �"  g   @     �"     !     �"  O   	     #  �   �     #     �      ,#  �   a     <#     	     L#  �   �     \#     �     l#  �   �     |#     �     �#  �   �     �#     �     �#  �   �     �#     s     �#  �   b     �#     @     �#  �   =     �#          $  }        $     �     ,$     q     <$     #     L$     �     \$  7   �     l$  �   �     |$  O   �     �$     q     �$     #     �$  �   �
     �$  �   �
     �$  O   �
     �$     �
     �$     e
     �$  �   @
     %  x   8
  
   %  M   #
     ,%     
     <%     �	     L%  a   �	  
   \%  �  �	     l%     o	     |%  �  <	     �%  O   .	     �%     	     �%     �     �%  �   �     �%     �     �%           �%  x        �%          &     �     &     �     ,&     r     <&     Y     L&  Q   I  
   \&     �     l&     �  
   |&     �     �&     �  
   �&  f   ^     �&     �  	   �&  "   �     �&     �     �&     �     �&  Z   3     �&     ;     '     �     '     �     ,'     �     <'     �     L'  1   �       \'     J      l'     !       |'           
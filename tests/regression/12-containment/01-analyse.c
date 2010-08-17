// PARAM: --analysis containment --class UES --gccwarn --allfuns CXX.json
/* I2 Goblint Version with debug info*/
#line 0 "LLVM INTERNAL"
/* Provide Declarations */
#include <stdarg.h>
#include <setjmp.h>
/* get a declaration for alloca */
#if defined(__CYGWIN__) || defined(__MINGW32__)
#define  alloca(x) __builtin_alloca((x))
#define _alloca(x) __builtin_alloca((x))
#elif defined(__APPLE__)
extern void *__builtin_alloca(unsigned long);
#define alloca(x) __builtin_alloca(x)
#define longjmp _longjmp
#define setjmp _setjmp
#elif defined(__sun__)
#if defined(__sparcv9)
extern void *__builtin_alloca(unsigned long);
#else
extern void *__builtin_alloca(unsigned int);
#endif
#define alloca(x) __builtin_alloca(x)
#elif defined(__FreeBSD__) || defined(__NetBSD__) || defined(__OpenBSD__) || defined(__DragonFly__) || defined(__arm__)
#define alloca(x) __builtin_alloca(x)
#elif defined(_MSC_VER)
#define inline _inline
#define alloca(x) _alloca(x)
#else
#include <alloca.h>
#endif

#ifndef __GNUC__  /* Can only support "linkonce" vars with GCC */
#define __attribute__(X)
#endif

#if defined(__GNUC__) && defined(__APPLE_CC__)
#define __EXTERNAL_WEAK__ __attribute__((weak_import))
#elif defined(__GNUC__)
#define __EXTERNAL_WEAK__ __attribute__((weak))
#else
#define __EXTERNAL_WEAK__
#endif

#if defined(__GNUC__) && defined(__APPLE_CC__)
#define __ATTRIBUTE_WEAK__
#elif defined(__GNUC__)
#define __ATTRIBUTE_WEAK__ __attribute__((weak))
#else
#define __ATTRIBUTE_WEAK__
#endif

#if defined(__GNUC__)
#define __HIDDEN__ __attribute__((visibility("hidden")))
#endif

#ifdef __GNUC__
#define LLVM_NAN(NanStr)   __builtin_nan(NanStr)   /* Double */
#define LLVM_NANF(NanStr)  __builtin_nanf(NanStr)  /* Float */
#define LLVM_NANS(NanStr)  __builtin_nans(NanStr)  /* Double */
#define LLVM_NANSF(NanStr) __builtin_nansf(NanStr) /* Float */
#define LLVM_INF           __builtin_inf()         /* Double */
#define LLVM_INFF          __builtin_inff()        /* Float */
#define LLVM_PREFETCH(addr,rw,locality) __builtin_prefetch(addr,rw,locality)
#define __ATTRIBUTE_CTOR__ __attribute__((constructor))
#define __ATTRIBUTE_DTOR__ __attribute__((destructor))
#define LLVM_ASM           __asm__
#else
#define LLVM_NAN(NanStr)   ((double)0.0)           /* Double */
#define LLVM_NANF(NanStr)  0.0F                    /* Float */
#define LLVM_NANS(NanStr)  ((double)0.0)           /* Double */
#define LLVM_NANSF(NanStr) 0.0F                    /* Float */
#define LLVM_INF           ((double)0.0)           /* Double */
#define LLVM_INFF          0.0F                    /* Float */
#define LLVM_PREFETCH(addr,rw,locality)            /* PREFETCH */
#define __ATTRIBUTE_CTOR__
#define __ATTRIBUTE_DTOR__
#define LLVM_ASM(X)
#endif

#if __GNUC__ < 4 /* Old GCC's, or compilers not GCC */ 
#define __builtin_stack_save() 0   /* not implemented */
#define __builtin_stack_restore(X) /* noop */
#endif

#if __GNUC__ && __LP64__ /* 128-bit integer types */
typedef int llvmInt128;
typedef unsigned llvmUInt128;
#endif

#define CODE_FOR_MAIN() /* Any target-specific code for main()*/

#ifndef __cplusplus
typedef unsigned char bool;
#endif


/* Support for floating point constants */
typedef unsigned long long ConstantDoubleTy;
typedef unsigned int        ConstantFloatTy;
typedef struct { unsigned long long f1; unsigned short f2; unsigned short pad[3]; } ConstantFP80Ty;
typedef struct { unsigned long long f1; unsigned long long f2; } ConstantFP128Ty;


/* Global Declarations */
/* Helper union for bitcasts */
typedef union {
  unsigned int Int32;
  unsigned long long Int64;
  float Float;
  double Double;
} llvmBitCastUnion;
/* Structure forward decls */
struct l_class_OC_BaseEvent;
struct l_class_OC_CEvent;
struct l_class_OC_CStateBase;
struct l_class_OC_CStateT;
struct l_class_OC_Common_KD__KD_CFsmBase;
struct l_class_OC_ICcsLog;
struct l_class_OC_IFsmExitHandler;
struct l_class_OC_Uec_KD__KD_UecBaseData;
struct l_class_OC_Uec_KD__KD_UecCellData;
struct l_class_OC_Uec_KD__KD_UecDebugCallstack;
struct l_class_OC_Uec_KD__KD_UecDirectTransfer;
struct l_class_OC_Uec_KD__KD_UecEvent;
struct l_class_OC_Uec_KD__KD_UecEventBuffer;
struct l_class_OC_Uec_KD__KD_UecFsmBase;
struct l_class_OC_Uec_KD__KD_UecManagerBase;
struct l_class_OC_Uec_KD__KD_UecServiceBase;
struct l_class_OC_Uec_KD__KD_UecTimePort;
struct l_class_OC_Uec_KD__KD_UecTimerWheel;
struct l_class_OC_Uec_KD__KD_UecUeContextData;
struct l_class_OC_Uec_KD__KD_UecUeSecurityInformation;
struct l_class_OC_std_KD__KD__Rb_tree;
struct l_class_OC_std_KD__KD_basic_string;
struct l_class_OC_std_KD__KD_deque;
struct l_class_OC_std_KD__KD_ios_base;
struct l_class_OC_std_KD__KD_list;
struct l_class_OC_std_KD__KD_locale;
struct l_class_OC_std_KD__KD_locale_KD__KD__Impl;
struct l_class_OC_std_KD__KD_locale_KD__KD_facet;
struct l_class_OC_std_KD__KD_map;
struct l_class_OC_std_KD__KD_queue;
struct l_class_OC_std_KD__KD_vector;
struct l_struct_OC_SAdjacentCellInfo;
struct l_struct_OC_SAmRlcPbTab;
struct l_struct_OC_SAntennaInfo;
struct l_struct_OC_SArrayOfOaMAdjWInfList;
struct l_struct_OC_SArrayOfOaMBcPlmnIdList;
struct l_struct_OC_SArrayOfOaMBlacklistHoL;
struct l_struct_OC_SArrayOfOaMUTargetPlmnIdL;
struct l_struct_OC_SAsnDynstr;
struct l_struct_OC_SCellcCqiConfigurationParameter;
struct l_struct_OC_SCellcDrbConfiguration;
struct l_struct_OC_SCellcSchedulingRequestConfiguration;
struct l_struct_OC_SCellcSchedulingWeight;
struct l_struct_OC_SCellcSrbConfiguration;
struct l_struct_OC_SCqiInfo;
struct l_struct_OC_SErrcACBarringConfig;
struct l_struct_OC_SErrcAcBarringForSpecialAC;
struct l_struct_OC_SErrcAcBarringInfo;
struct l_struct_OC_SErrcBandInfoEUTRA;
struct l_struct_OC_SErrcBandListEUTRA;
struct l_struct_OC_SErrcCellAccessRelatedInfo;
struct l_struct_OC_SErrcCellSelectionInfo;
struct l_struct_OC_SErrcCsgIdentity;
struct l_struct_OC_SErrcDLInformationTransfer;
struct l_struct_OC_SErrcDLInformationTransferR8IEs;
struct l_struct_OC_SErrcDeltaFListPUCCH;
struct l_struct_OC_SErrcFourFrames;
struct l_struct_OC_SErrcFreqInfo;
struct l_struct_OC_SErrcIRATParametersCDMA20001XRTT;
struct l_struct_OC_SErrcIRATParametersGERAN;
struct l_struct_OC_SErrcIRATParametersUTRAFDD;
struct l_struct_OC_SErrcInterFreqBandList;
struct l_struct_OC_SErrcInterRATParameters;
struct l_struct_OC_SErrcMBSFNSubframeConfig;
struct l_struct_OC_SErrcMBSFNSubframeConfigList;
struct l_struct_OC_SErrcMasterInformationBlock;
struct l_struct_OC_SErrcMeasIdToAddMod;
struct l_struct_OC_SErrcMeasIdToAddModList;
struct l_struct_OC_SErrcMeasParameters;
struct l_struct_OC_SErrcN4TxAntennaTm4;
struct l_struct_OC_SErrcN4TxAntennaTm5;
struct l_struct_OC_SErrcPDCPParameters;
struct l_struct_OC_SErrcPDSCHConfigCommon;
struct l_struct_OC_SErrcPLMNIdentityInfo;
struct l_struct_OC_SErrcPLMNIdentityList;
struct l_struct_OC_SErrcPRACHConfigInfo;
struct l_struct_OC_SErrcPRACHConfigSIB;
struct l_struct_OC_SErrcPUCCHConfigCommon;
struct l_struct_OC_SErrcPUSCHConfigCommon;
struct l_struct_OC_SErrcPreambleInfo;
struct l_struct_OC_SErrcPuschConfigBasic;
struct l_struct_OC_SErrcRACHConfigCommon;
struct l_struct_OC_SErrcRFParameters;
struct l_struct_OC_SErrcRadioResourceConfigCommonSIB;
struct l_struct_OC_SErrcSIBMappingInfo;
struct l_struct_OC_SErrcSchedulingInfo;
struct l_struct_OC_SErrcSchedulingInfoList;
struct l_struct_OC_SErrcSetupSoundingRSULConfigCommon;
struct l_struct_OC_SErrcSetupSoundingRSULConfigDedicated;
struct l_struct_OC_SErrcSupportedBandList1XRTT;
struct l_struct_OC_SErrcSupportedBandListEUTRA;
struct l_struct_OC_SErrcSupportedBandListGERAN;
struct l_struct_OC_SErrcSupportedROHCProfiles;
struct l_struct_OC_SErrcSystemInformationBlockType1;
struct l_struct_OC_SErrcSystemInformationBlockType2;
struct l_struct_OC_SErrcUEEUTRACapability;
struct l_struct_OC_SErrcULInformationTransfer;
struct l_struct_OC_SErrcUplinkPowerControlCommon;
struct l_struct_OC_SGummei;
struct l_struct_OC_SHoData;
struct l_struct_OC_SHoIRatWcdma;
struct l_struct_OC_SHoMeasData;
struct l_struct_OC_SLnAdjW;
struct l_struct_OC_SLnHoW;
struct l_struct_OC_SOaMBlacklistHoL;
struct l_struct_OC_SOaMRedirGeranArfcnValueL;
struct l_struct_OC_SPdcpProf;
struct l_struct_OC_SQciTab;
struct l_struct_OC_SRedirGeranExplicitListOfArfcns;
struct l_struct_OC_SRedirRatGeran;
struct l_struct_OC_SRedirection;
struct l_struct_OC_SRedirectionTarget;
struct l_struct_OC_SS1apEPLMNs;
struct l_struct_OC_SS1apForbiddenLACs;
struct l_struct_OC_SS1apForbiddenLAs;
struct l_struct_OC_SS1apForbiddenLAsItem;
struct l_struct_OC_SS1apGUMMEI;
struct l_struct_OC_SS1apHandoverRestrictionList;
struct l_struct_OC_SS1apSecurityKey;
struct l_struct_OC_SS1apTAI;
struct l_struct_OC_SS1apUEAggregateMaximumBitrate;
struct l_struct_OC_SS1apUFDownlinkNASTransport;
struct l_struct_OC_SS1apUFInitialUEMessage;
struct l_struct_OC_SS1apUFNASNonDeliveryIndication;
struct l_struct_OC_SS1apUFUplinkNASTransport;
struct l_struct_OC_SSTMsi;
struct l_struct_OC_SSecurity;
struct l_struct_OC_STransportLayerAddress;
struct l_struct_OC_SX2apAllocationAndRetentionPriority;
struct l_struct_OC_TUP_L3MessageInd;
struct l_struct_OC_TUP_L3MessageReq;
struct l_struct_OC_TUP_SrbReceiveInd;
struct l_struct_OC_TUP_SrbSendReq;
struct l_struct_OC_TUP_SrbSendResp;
struct l_struct_OC_UErrcC1CriticalExtensionsDLInformationTransfer;
struct l_struct_OC_UErrcCodebookSubsetRestriction;
struct l_struct_OC_UErrcCriticalExtensionsDLInformationTransfer;
struct l_struct_OC_UErrcDedicatedInfoType;
struct l_struct_OC_UErrcInitialUEIdentity;
struct l_struct_OC_UErrcSoundingRSULConfigCommon;
struct l_struct_OC_UErrcSubframeAllocation;
struct l_struct_OC_URedirGeranFollowingArfcns;
struct l_struct_OC_URedirRt;
struct l_struct_OC_US1apCause;
struct l_struct_OC_Uec_KD__KD_SUecCellData;
struct l_struct_OC_Uec_KD__KD_SUecCellSetup;
struct l_struct_OC_Uec_KD__KD_SUecForbiddenLACs;
struct l_struct_OC_Uec_KD__KD_SUecForbiddenLAs;
struct l_struct_OC_Uec_KD__KD_SUecForbiddenLAsItem;
struct l_struct_OC_Uec_KD__KD_SUecHandoverRestrictionList;
struct l_struct_OC_Uec_KD__KD_SUecSysInfoData;
struct l_struct_OC_Uec_KD__KD_SUecUeCapabilityInfo;
struct l_struct_OC_Uec_KD__KD_SUecUeContextData;
struct l_struct_OC_Uec_KD__KD_SUecUeContextReleaseInfo;
struct l_struct_OC_Uec_KD__KD_SUecUeData;
struct l_struct_OC_Uec_KD__KD_SUecUeDb;
struct l_struct_OC_Uec_KD__KD_SUecUeDbData;
struct l_struct_OC_Uec_KD__KD_SUecUeInactivity;
struct l_struct_OC_Uec_KD__KD_SUecUeSb;
struct l_struct_OC_Uec_KD__KD_SUecUeSbData;
struct l_struct_OC_Uec_KD__KD_UEC_DTF_DirectTransfer;
struct l_struct_OC_Uec_KD__KD_UecTimerWheel_KD__KD_TimeSlot;
struct l_struct_OC_Uec_KD__KD_UecTimerWheel_KD__KD_TimerBlock;
struct l_struct_OC_std_KD__KD__Rb_tree_MD_EPmCounterId_MC__AC_std_KD__KD_pair_MD_EPmCounterId_AC_const_MC__AC_Common_KD__KD_LomBH_KD__KD_IfCommonLomBrowserHandler_KD__KD_CounterHandle_OD__MC__AC_std_KD__KD__Select1st_MD_std_KD__KD_pair_MD_EPmCounterId_AC_const_MC__AC_Common_KD__KD_LomBH_KD__KD_IfCommonLomBrowserHandler_KD__KD_CounterHandle_OD__AC__OD__MC__AC_std_KD__KD_less_MD_EPmCounterId_OD__MC__AC_std_KD__KD_allocator_MD_std_KD__KD_pair_MD_EPmCounterId_AC_const_MC__AC_Common_KD__KD_LomBH_KD__KD_IfCommonLomBrowserHandler_KD__KD_CounterHandle_OD__AC__OD__AC__OD__KD__KD__Rb_tree_impl;
struct l_struct_OC_std_KD__KD__Rb_tree_iterator;
struct l_struct_OC_std_KD__KD__Rb_tree_node;
struct l_struct_OC_std_KD__KD__Rb_tree_node_base;
struct l_struct_OC_std_KD__KD_basic_string_MD_char_MC__AC_std_KD__KD_char_traits_MD_char_OD__MC__AC_std_KD__KD_allocator_MD_char_OD__AC__OD__KD__KD__Alloc_hider;
struct l_struct_OC_std_KD__KD_basic_string_MD_char_MC__AC_std_KD__KD_char_traits_MD_char_OD__MC__AC_std_KD__KD_allocator_MD_char_OD__AC__OD__KD__KD__Rep_base;
struct l_struct_OC_std_KD__KD_ios_base_KD__KD__Callback_list;
struct l_struct_OC_std_KD__KD_ios_base_KD__KD__Words;
struct l_struct_OC_std_KD__KD_pair;
struct l_union_OC_UInnerUErrcC1CriticalExtensionsDLInformationTransfer;
struct l_union_OC_UInnerUErrcCodebookSubsetRestriction;
struct l_union_OC_UInnerUErrcCriticalExtensionsDLInformationTransfer;
struct l_union_OC_UInnerUErrcDedicatedInfoType;
struct l_union_OC_UInnerUErrcInitialUEIdentity;
struct l_union_OC_UInnerUErrcSoundingRSULConfigCommon;
struct l_union_OC_UInnerUErrcSubframeAllocation;
struct l_union_OC_UInnerURedirGeranFollowingArfcns;
struct l_union_OC_UInnerURedirRt;
struct l_unnamed0;
struct l_unnamed1;
struct l_unnamed10;
struct l_unnamed100;
struct l_unnamed101;
struct l_unnamed102;
struct l_unnamed103;
struct l_unnamed104;
struct l_unnamed105;
struct l_unnamed106;
struct l_unnamed107;
struct l_unnamed108;
struct l_unnamed109;
struct l_unnamed11;
struct l_unnamed12;
struct l_unnamed13;
struct l_unnamed14;
struct l_unnamed15;
struct l_unnamed16;
struct l_unnamed17;
struct l_unnamed18;
struct l_unnamed19;
struct l_unnamed2;
struct l_unnamed20;
struct l_unnamed21;
struct l_unnamed22;
struct l_unnamed23;
struct l_unnamed24;
struct l_unnamed25;
struct l_unnamed26;
struct l_unnamed27;
struct l_unnamed28;
struct l_unnamed29;
struct l_unnamed3;
struct l_unnamed30;
struct l_unnamed31;
struct l_unnamed32;
struct l_unnamed33;
struct l_unnamed34;
struct l_unnamed35;
struct l_unnamed36;
struct l_unnamed37;
struct l_unnamed38;
struct l_unnamed39;
struct l_unnamed4;
struct l_unnamed40;
struct l_unnamed41;
struct l_unnamed42;
struct l_unnamed43;
struct l_unnamed44;
struct l_unnamed45;
struct l_unnamed46;
struct l_unnamed47;
struct l_unnamed48;
struct l_unnamed49;
struct l_unnamed5;
struct l_unnamed50;
struct l_unnamed51;
struct l_unnamed52;
struct l_unnamed53;
struct l_unnamed54;
struct l_unnamed55;
struct l_unnamed56;
struct l_unnamed57;
struct l_unnamed58;
struct l_unnamed59;
struct l_unnamed6;
struct l_unnamed60;
struct l_unnamed61;
struct l_unnamed62;
struct l_unnamed63;
struct l_unnamed64;
struct l_unnamed65;
struct l_unnamed66;
struct l_unnamed67;
struct l_unnamed68;
struct l_unnamed69;
struct l_unnamed7;
struct l_unnamed70;
struct l_unnamed71;
struct l_unnamed72;
struct l_unnamed73;
struct l_unnamed74;
struct l_unnamed75;
struct l_unnamed76;
struct l_unnamed77;
struct l_unnamed78;
struct l_unnamed79;
struct l_unnamed8;
struct l_unnamed80;
struct l_unnamed81;
struct l_unnamed82;
struct l_unnamed83;
struct l_unnamed84;
struct l_unnamed85;
struct l_unnamed86;
struct l_unnamed87;
struct l_unnamed88;
struct l_unnamed89;
struct l_unnamed9;
struct l_unnamed90;
struct l_unnamed91;
struct l_unnamed92;
struct l_unnamed93;
struct l_unnamed94;
struct l_unnamed95;
struct l_unnamed96;
struct l_unnamed97;
struct l_unnamed98;
struct l_unnamed99;

/* Typedefs */
typedef struct l_class_OC_BaseEvent l_class_OC_BaseEvent;
typedef struct l_class_OC_CEvent l_class_OC_CEvent;
typedef struct l_class_OC_CStateBase l_class_OC_CStateBase;
typedef struct l_class_OC_CStateT l_class_OC_CStateT;
typedef struct l_class_OC_Common_KD__KD_CFsmBase l_class_OC_Common_KD__KD_CFsmBase;
typedef struct l_class_OC_ICcsLog l_class_OC_ICcsLog;
typedef struct l_class_OC_IFsmExitHandler l_class_OC_IFsmExitHandler;
typedef struct l_class_OC_Uec_KD__KD_UecBaseData l_class_OC_Uec_KD__KD_UecBaseData;
typedef struct l_class_OC_Uec_KD__KD_UecCellData l_class_OC_Uec_KD__KD_UecCellData;
typedef struct l_class_OC_Uec_KD__KD_UecDebugCallstack l_class_OC_Uec_KD__KD_UecDebugCallstack;
typedef struct l_class_OC_Uec_KD__KD_UecDirectTransfer l_class_OC_Uec_KD__KD_UecDirectTransfer;
typedef struct l_class_OC_Uec_KD__KD_UecEvent l_class_OC_Uec_KD__KD_UecEvent;
typedef struct l_class_OC_Uec_KD__KD_UecEventBuffer l_class_OC_Uec_KD__KD_UecEventBuffer;
typedef struct l_class_OC_Uec_KD__KD_UecFsmBase l_class_OC_Uec_KD__KD_UecFsmBase;
typedef struct l_class_OC_Uec_KD__KD_UecManagerBase l_class_OC_Uec_KD__KD_UecManagerBase;
typedef struct l_class_OC_Uec_KD__KD_UecServiceBase l_class_OC_Uec_KD__KD_UecServiceBase;
typedef struct l_class_OC_Uec_KD__KD_UecTimePort l_class_OC_Uec_KD__KD_UecTimePort;
typedef struct l_class_OC_Uec_KD__KD_UecTimerWheel l_class_OC_Uec_KD__KD_UecTimerWheel;
typedef struct l_class_OC_Uec_KD__KD_UecUeContextData l_class_OC_Uec_KD__KD_UecUeContextData;
typedef struct l_class_OC_Uec_KD__KD_UecUeSecurityInformation l_class_OC_Uec_KD__KD_UecUeSecurityInformation;
typedef struct l_class_OC_std_KD__KD__Rb_tree l_class_OC_std_KD__KD__Rb_tree;
typedef struct l_class_OC_std_KD__KD_basic_string l_class_OC_std_KD__KD_basic_string;
typedef struct l_class_OC_std_KD__KD_deque l_class_OC_std_KD__KD_deque;
typedef struct l_class_OC_std_KD__KD_ios_base l_class_OC_std_KD__KD_ios_base;
typedef struct l_class_OC_std_KD__KD_list l_class_OC_std_KD__KD_list;
typedef struct l_class_OC_std_KD__KD_locale l_class_OC_std_KD__KD_locale;
typedef struct l_class_OC_std_KD__KD_locale_KD__KD__Impl l_class_OC_std_KD__KD_locale_KD__KD__Impl;
typedef struct l_class_OC_std_KD__KD_locale_KD__KD_facet l_class_OC_std_KD__KD_locale_KD__KD_facet;
typedef struct l_class_OC_std_KD__KD_map l_class_OC_std_KD__KD_map;
typedef struct l_class_OC_std_KD__KD_queue l_class_OC_std_KD__KD_queue;
typedef struct l_class_OC_std_KD__KD_vector l_class_OC_std_KD__KD_vector;
typedef struct l_struct_OC_SAdjacentCellInfo l_struct_OC_SAdjacentCellInfo;
typedef struct l_struct_OC_SAmRlcPbTab l_struct_OC_SAmRlcPbTab;
typedef struct l_struct_OC_SAntennaInfo l_struct_OC_SAntennaInfo;
typedef struct l_struct_OC_SArrayOfOaMAdjWInfList l_struct_OC_SArrayOfOaMAdjWInfList;
typedef struct l_struct_OC_SArrayOfOaMBcPlmnIdList l_struct_OC_SArrayOfOaMBcPlmnIdList;
typedef struct l_struct_OC_SArrayOfOaMBlacklistHoL l_struct_OC_SArrayOfOaMBlacklistHoL;
typedef struct l_struct_OC_SArrayOfOaMUTargetPlmnIdL l_struct_OC_SArrayOfOaMUTargetPlmnIdL;
typedef struct l_struct_OC_SAsnDynstr l_struct_OC_SAsnDynstr;
typedef struct l_struct_OC_SCellcCqiConfigurationParameter l_struct_OC_SCellcCqiConfigurationParameter;
typedef struct l_struct_OC_SCellcDrbConfiguration l_struct_OC_SCellcDrbConfiguration;
typedef struct l_struct_OC_SCellcSchedulingRequestConfiguration l_struct_OC_SCellcSchedulingRequestConfiguration;
typedef struct l_struct_OC_SCellcSchedulingWeight l_struct_OC_SCellcSchedulingWeight;
typedef struct l_struct_OC_SCellcSrbConfiguration l_struct_OC_SCellcSrbConfiguration;
typedef struct l_struct_OC_SCqiInfo l_struct_OC_SCqiInfo;
typedef struct l_struct_OC_SErrcACBarringConfig l_struct_OC_SErrcACBarringConfig;
typedef struct l_struct_OC_SErrcAcBarringForSpecialAC l_struct_OC_SErrcAcBarringForSpecialAC;
typedef struct l_struct_OC_SErrcAcBarringInfo l_struct_OC_SErrcAcBarringInfo;
typedef struct l_struct_OC_SErrcBandInfoEUTRA l_struct_OC_SErrcBandInfoEUTRA;
typedef struct l_struct_OC_SErrcBandListEUTRA l_struct_OC_SErrcBandListEUTRA;
typedef struct l_struct_OC_SErrcCellAccessRelatedInfo l_struct_OC_SErrcCellAccessRelatedInfo;
typedef struct l_struct_OC_SErrcCellSelectionInfo l_struct_OC_SErrcCellSelectionInfo;
typedef struct l_struct_OC_SErrcCsgIdentity l_struct_OC_SErrcCsgIdentity;
typedef struct l_struct_OC_SErrcDLInformationTransfer l_struct_OC_SErrcDLInformationTransfer;
typedef struct l_struct_OC_SErrcDLInformationTransferR8IEs l_struct_OC_SErrcDLInformationTransferR8IEs;
typedef struct l_struct_OC_SErrcDeltaFListPUCCH l_struct_OC_SErrcDeltaFListPUCCH;
typedef struct l_struct_OC_SErrcFourFrames l_struct_OC_SErrcFourFrames;
typedef struct l_struct_OC_SErrcFreqInfo l_struct_OC_SErrcFreqInfo;
typedef struct l_struct_OC_SErrcIRATParametersCDMA20001XRTT l_struct_OC_SErrcIRATParametersCDMA20001XRTT;
typedef struct l_struct_OC_SErrcIRATParametersGERAN l_struct_OC_SErrcIRATParametersGERAN;
typedef struct l_struct_OC_SErrcIRATParametersUTRAFDD l_struct_OC_SErrcIRATParametersUTRAFDD;
typedef struct l_struct_OC_SErrcInterFreqBandList l_struct_OC_SErrcInterFreqBandList;
typedef struct l_struct_OC_SErrcInterRATParameters l_struct_OC_SErrcInterRATParameters;
typedef struct l_struct_OC_SErrcMBSFNSubframeConfig l_struct_OC_SErrcMBSFNSubframeConfig;
typedef struct l_struct_OC_SErrcMBSFNSubframeConfigList l_struct_OC_SErrcMBSFNSubframeConfigList;
typedef struct l_struct_OC_SErrcMasterInformationBlock l_struct_OC_SErrcMasterInformationBlock;
typedef struct l_struct_OC_SErrcMeasIdToAddMod l_struct_OC_SErrcMeasIdToAddMod;
typedef struct l_struct_OC_SErrcMeasIdToAddModList l_struct_OC_SErrcMeasIdToAddModList;
typedef struct l_struct_OC_SErrcMeasParameters l_struct_OC_SErrcMeasParameters;
typedef struct l_struct_OC_SErrcN4TxAntennaTm4 l_struct_OC_SErrcN4TxAntennaTm4;
typedef struct l_struct_OC_SErrcN4TxAntennaTm5 l_struct_OC_SErrcN4TxAntennaTm5;
typedef struct l_struct_OC_SErrcPDCPParameters l_struct_OC_SErrcPDCPParameters;
typedef struct l_struct_OC_SErrcPDSCHConfigCommon l_struct_OC_SErrcPDSCHConfigCommon;
typedef struct l_struct_OC_SErrcPLMNIdentityInfo l_struct_OC_SErrcPLMNIdentityInfo;
typedef struct l_struct_OC_SErrcPLMNIdentityList l_struct_OC_SErrcPLMNIdentityList;
typedef struct l_struct_OC_SErrcPRACHConfigInfo l_struct_OC_SErrcPRACHConfigInfo;
typedef struct l_struct_OC_SErrcPRACHConfigSIB l_struct_OC_SErrcPRACHConfigSIB;
typedef struct l_struct_OC_SErrcPUCCHConfigCommon l_struct_OC_SErrcPUCCHConfigCommon;
typedef struct l_struct_OC_SErrcPUSCHConfigCommon l_struct_OC_SErrcPUSCHConfigCommon;
typedef struct l_struct_OC_SErrcPreambleInfo l_struct_OC_SErrcPreambleInfo;
typedef struct l_struct_OC_SErrcPuschConfigBasic l_struct_OC_SErrcPuschConfigBasic;
typedef struct l_struct_OC_SErrcRACHConfigCommon l_struct_OC_SErrcRACHConfigCommon;
typedef struct l_struct_OC_SErrcRFParameters l_struct_OC_SErrcRFParameters;
typedef struct l_struct_OC_SErrcRadioResourceConfigCommonSIB l_struct_OC_SErrcRadioResourceConfigCommonSIB;
typedef struct l_struct_OC_SErrcSIBMappingInfo l_struct_OC_SErrcSIBMappingInfo;
typedef struct l_struct_OC_SErrcSchedulingInfo l_struct_OC_SErrcSchedulingInfo;
typedef struct l_struct_OC_SErrcSchedulingInfoList l_struct_OC_SErrcSchedulingInfoList;
typedef struct l_struct_OC_SErrcSetupSoundingRSULConfigCommon l_struct_OC_SErrcSetupSoundingRSULConfigCommon;
typedef struct l_struct_OC_SErrcSetupSoundingRSULConfigDedicated l_struct_OC_SErrcSetupSoundingRSULConfigDedicated;
typedef struct l_struct_OC_SErrcSupportedBandList1XRTT l_struct_OC_SErrcSupportedBandList1XRTT;
typedef struct l_struct_OC_SErrcSupportedBandListEUTRA l_struct_OC_SErrcSupportedBandListEUTRA;
typedef struct l_struct_OC_SErrcSupportedBandListGERAN l_struct_OC_SErrcSupportedBandListGERAN;
typedef struct l_struct_OC_SErrcSupportedROHCProfiles l_struct_OC_SErrcSupportedROHCProfiles;
typedef struct l_struct_OC_SErrcSystemInformationBlockType1 l_struct_OC_SErrcSystemInformationBlockType1;
typedef struct l_struct_OC_SErrcSystemInformationBlockType2 l_struct_OC_SErrcSystemInformationBlockType2;
typedef struct l_struct_OC_SErrcUEEUTRACapability l_struct_OC_SErrcUEEUTRACapability;
typedef struct l_struct_OC_SErrcULInformationTransfer l_struct_OC_SErrcULInformationTransfer;
typedef struct l_struct_OC_SErrcUplinkPowerControlCommon l_struct_OC_SErrcUplinkPowerControlCommon;
typedef struct l_struct_OC_SGummei l_struct_OC_SGummei;
typedef struct l_struct_OC_SHoData l_struct_OC_SHoData;
typedef struct l_struct_OC_SHoIRatWcdma l_struct_OC_SHoIRatWcdma;
typedef struct l_struct_OC_SHoMeasData l_struct_OC_SHoMeasData;
typedef struct l_struct_OC_SLnAdjW l_struct_OC_SLnAdjW;
typedef struct l_struct_OC_SLnHoW l_struct_OC_SLnHoW;
typedef struct l_struct_OC_SOaMBlacklistHoL l_struct_OC_SOaMBlacklistHoL;
typedef struct l_struct_OC_SOaMRedirGeranArfcnValueL l_struct_OC_SOaMRedirGeranArfcnValueL;
typedef struct l_struct_OC_SPdcpProf l_struct_OC_SPdcpProf;
typedef struct l_struct_OC_SQciTab l_struct_OC_SQciTab;
typedef struct l_struct_OC_SRedirGeranExplicitListOfArfcns l_struct_OC_SRedirGeranExplicitListOfArfcns;
typedef struct l_struct_OC_SRedirRatGeran l_struct_OC_SRedirRatGeran;
typedef struct l_struct_OC_SRedirection l_struct_OC_SRedirection;
typedef struct l_struct_OC_SRedirectionTarget l_struct_OC_SRedirectionTarget;
typedef struct l_struct_OC_SS1apEPLMNs l_struct_OC_SS1apEPLMNs;
typedef struct l_struct_OC_SS1apForbiddenLACs l_struct_OC_SS1apForbiddenLACs;
typedef struct l_struct_OC_SS1apForbiddenLAs l_struct_OC_SS1apForbiddenLAs;
typedef struct l_struct_OC_SS1apForbiddenLAsItem l_struct_OC_SS1apForbiddenLAsItem;
typedef struct l_struct_OC_SS1apGUMMEI l_struct_OC_SS1apGUMMEI;
typedef struct l_struct_OC_SS1apHandoverRestrictionList l_struct_OC_SS1apHandoverRestrictionList;
typedef struct l_struct_OC_SS1apSecurityKey l_struct_OC_SS1apSecurityKey;
typedef struct l_struct_OC_SS1apTAI l_struct_OC_SS1apTAI;
typedef struct l_struct_OC_SS1apUEAggregateMaximumBitrate l_struct_OC_SS1apUEAggregateMaximumBitrate;
typedef struct l_struct_OC_SS1apUFDownlinkNASTransport l_struct_OC_SS1apUFDownlinkNASTransport;
typedef struct l_struct_OC_SS1apUFInitialUEMessage l_struct_OC_SS1apUFInitialUEMessage;
typedef struct l_struct_OC_SS1apUFNASNonDeliveryIndication l_struct_OC_SS1apUFNASNonDeliveryIndication;
typedef struct l_struct_OC_SS1apUFUplinkNASTransport l_struct_OC_SS1apUFUplinkNASTransport;
typedef struct l_struct_OC_SSTMsi l_struct_OC_SSTMsi;
typedef struct l_struct_OC_SSecurity l_struct_OC_SSecurity;
typedef struct l_struct_OC_STransportLayerAddress l_struct_OC_STransportLayerAddress;
typedef struct l_struct_OC_SX2apAllocationAndRetentionPriority l_struct_OC_SX2apAllocationAndRetentionPriority;
typedef struct l_struct_OC_TUP_L3MessageInd l_struct_OC_TUP_L3MessageInd;
typedef struct l_struct_OC_TUP_L3MessageReq l_struct_OC_TUP_L3MessageReq;
typedef struct l_struct_OC_TUP_SrbReceiveInd l_struct_OC_TUP_SrbReceiveInd;
typedef struct l_struct_OC_TUP_SrbSendReq l_struct_OC_TUP_SrbSendReq;
typedef struct l_struct_OC_TUP_SrbSendResp l_struct_OC_TUP_SrbSendResp;
typedef struct l_struct_OC_UErrcC1CriticalExtensionsDLInformationTransfer l_struct_OC_UErrcC1CriticalExtensionsDLInformationTransfer;
typedef struct l_struct_OC_UErrcCodebookSubsetRestriction l_struct_OC_UErrcCodebookSubsetRestriction;
typedef struct l_struct_OC_UErrcCriticalExtensionsDLInformationTransfer l_struct_OC_UErrcCriticalExtensionsDLInformationTransfer;
typedef struct l_struct_OC_UErrcDedicatedInfoType l_struct_OC_UErrcDedicatedInfoType;
typedef struct l_struct_OC_UErrcInitialUEIdentity l_struct_OC_UErrcInitialUEIdentity;
typedef struct l_struct_OC_UErrcSoundingRSULConfigCommon l_struct_OC_UErrcSoundingRSULConfigCommon;
typedef struct l_struct_OC_UErrcSubframeAllocation l_struct_OC_UErrcSubframeAllocation;
typedef struct l_struct_OC_URedirGeranFollowingArfcns l_struct_OC_URedirGeranFollowingArfcns;
typedef struct l_struct_OC_URedirRt l_struct_OC_URedirRt;
typedef struct l_struct_OC_US1apCause l_struct_OC_US1apCause;
typedef struct l_struct_OC_Uec_KD__KD_SUecCellData l_struct_OC_Uec_KD__KD_SUecCellData;
typedef struct l_struct_OC_Uec_KD__KD_SUecCellSetup l_struct_OC_Uec_KD__KD_SUecCellSetup;
typedef struct l_struct_OC_Uec_KD__KD_SUecForbiddenLACs l_struct_OC_Uec_KD__KD_SUecForbiddenLACs;
typedef struct l_struct_OC_Uec_KD__KD_SUecForbiddenLAs l_struct_OC_Uec_KD__KD_SUecForbiddenLAs;
typedef struct l_struct_OC_Uec_KD__KD_SUecForbiddenLAsItem l_struct_OC_Uec_KD__KD_SUecForbiddenLAsItem;
typedef struct l_struct_OC_Uec_KD__KD_SUecHandoverRestrictionList l_struct_OC_Uec_KD__KD_SUecHandoverRestrictionList;
typedef struct l_struct_OC_Uec_KD__KD_SUecSysInfoData l_struct_OC_Uec_KD__KD_SUecSysInfoData;
typedef struct l_struct_OC_Uec_KD__KD_SUecUeCapabilityInfo l_struct_OC_Uec_KD__KD_SUecUeCapabilityInfo;
typedef struct l_struct_OC_Uec_KD__KD_SUecUeContextData l_struct_OC_Uec_KD__KD_SUecUeContextData;
typedef struct l_struct_OC_Uec_KD__KD_SUecUeContextReleaseInfo l_struct_OC_Uec_KD__KD_SUecUeContextReleaseInfo;
typedef struct l_struct_OC_Uec_KD__KD_SUecUeData l_struct_OC_Uec_KD__KD_SUecUeData;
typedef struct l_struct_OC_Uec_KD__KD_SUecUeDb l_struct_OC_Uec_KD__KD_SUecUeDb;
typedef struct l_struct_OC_Uec_KD__KD_SUecUeDbData l_struct_OC_Uec_KD__KD_SUecUeDbData;
typedef struct l_struct_OC_Uec_KD__KD_SUecUeInactivity l_struct_OC_Uec_KD__KD_SUecUeInactivity;
typedef struct l_struct_OC_Uec_KD__KD_SUecUeSb l_struct_OC_Uec_KD__KD_SUecUeSb;
typedef struct l_struct_OC_Uec_KD__KD_SUecUeSbData l_struct_OC_Uec_KD__KD_SUecUeSbData;
typedef struct l_struct_OC_Uec_KD__KD_UEC_DTF_DirectTransfer l_struct_OC_Uec_KD__KD_UEC_DTF_DirectTransfer;
typedef struct l_struct_OC_Uec_KD__KD_UecTimerWheel_KD__KD_TimeSlot l_struct_OC_Uec_KD__KD_UecTimerWheel_KD__KD_TimeSlot;
typedef struct l_struct_OC_Uec_KD__KD_UecTimerWheel_KD__KD_TimerBlock l_struct_OC_Uec_KD__KD_UecTimerWheel_KD__KD_TimerBlock;
typedef struct l_struct_OC_std_KD__KD__Rb_tree_MD_EPmCounterId_MC__AC_std_KD__KD_pair_MD_EPmCounterId_AC_const_MC__AC_Common_KD__KD_LomBH_KD__KD_IfCommonLomBrowserHandler_KD__KD_CounterHandle_OD__MC__AC_std_KD__KD__Select1st_MD_std_KD__KD_pair_MD_EPmCounterId_AC_const_MC__AC_Common_KD__KD_LomBH_KD__KD_IfCommonLomBrowserHandler_KD__KD_CounterHandle_OD__AC__OD__MC__AC_std_KD__KD_less_MD_EPmCounterId_OD__MC__AC_std_KD__KD_allocator_MD_std_KD__KD_pair_MD_EPmCounterId_AC_const_MC__AC_Common_KD__KD_LomBH_KD__KD_IfCommonLomBrowserHandler_KD__KD_CounterHandle_OD__AC__OD__AC__OD__KD__KD__Rb_tree_impl l_struct_OC_std_KD__KD__Rb_tree_MD_EPmCounterId_MC__AC_std_KD__KD_pair_MD_EPmCounterId_AC_const_MC__AC_Common_KD__KD_LomBH_KD__KD_IfCommonLomBrowserHandler_KD__KD_CounterHandle_OD__MC__AC_std_KD__KD__Select1st_MD_std_KD__KD_pair_MD_EPmCounterId_AC_const_MC__AC_Common_KD__KD_LomBH_KD__KD_IfCommonLomBrowserHandler_KD__KD_CounterHandle_OD__AC__OD__MC__AC_std_KD__KD_less_MD_EPmCounterId_OD__MC__AC_std_KD__KD_allocator_MD_std_KD__KD_pair_MD_EPmCounterId_AC_const_MC__AC_Common_KD__KD_LomBH_KD__KD_IfCommonLomBrowserHandler_KD__KD_CounterHandle_OD__AC__OD__AC__OD__KD__KD__Rb_tree_impl;
typedef struct l_struct_OC_std_KD__KD__Rb_tree_iterator l_struct_OC_std_KD__KD__Rb_tree_iterator;
typedef struct l_struct_OC_std_KD__KD__Rb_tree_node l_struct_OC_std_KD__KD__Rb_tree_node;
typedef struct l_struct_OC_std_KD__KD__Rb_tree_node_base l_struct_OC_std_KD__KD__Rb_tree_node_base;
typedef struct l_struct_OC_std_KD__KD_basic_string_MD_char_MC__AC_std_KD__KD_char_traits_MD_char_OD__MC__AC_std_KD__KD_allocator_MD_char_OD__AC__OD__KD__KD__Alloc_hider l_struct_OC_std_KD__KD_basic_string_MD_char_MC__AC_std_KD__KD_char_traits_MD_char_OD__MC__AC_std_KD__KD_allocator_MD_char_OD__AC__OD__KD__KD__Alloc_hider;
typedef struct l_struct_OC_std_KD__KD_basic_string_MD_char_MC__AC_std_KD__KD_char_traits_MD_char_OD__MC__AC_std_KD__KD_allocator_MD_char_OD__AC__OD__KD__KD__Rep_base l_struct_OC_std_KD__KD_basic_string_MD_char_MC__AC_std_KD__KD_char_traits_MD_char_OD__MC__AC_std_KD__KD_allocator_MD_char_OD__AC__OD__KD__KD__Rep_base;
typedef struct l_struct_OC_std_KD__KD_ios_base_KD__KD__Callback_list l_struct_OC_std_KD__KD_ios_base_KD__KD__Callback_list;
typedef struct l_struct_OC_std_KD__KD_ios_base_KD__KD__Words l_struct_OC_std_KD__KD_ios_base_KD__KD__Words;
typedef struct l_struct_OC_std_KD__KD_pair l_struct_OC_std_KD__KD_pair;
typedef struct l_union_OC_UInnerUErrcC1CriticalExtensionsDLInformationTransfer l_union_OC_UInnerUErrcC1CriticalExtensionsDLInformationTransfer;
typedef struct l_union_OC_UInnerUErrcCodebookSubsetRestriction l_union_OC_UInnerUErrcCodebookSubsetRestriction;
typedef struct l_union_OC_UInnerUErrcCriticalExtensionsDLInformationTransfer l_union_OC_UInnerUErrcCriticalExtensionsDLInformationTransfer;
typedef struct l_union_OC_UInnerUErrcDedicatedInfoType l_union_OC_UInnerUErrcDedicatedInfoType;
typedef struct l_union_OC_UInnerUErrcInitialUEIdentity l_union_OC_UInnerUErrcInitialUEIdentity;
typedef struct l_union_OC_UInnerUErrcSoundingRSULConfigCommon l_union_OC_UInnerUErrcSoundingRSULConfigCommon;
typedef struct l_union_OC_UInnerUErrcSubframeAllocation l_union_OC_UInnerUErrcSubframeAllocation;
typedef struct l_union_OC_UInnerURedirGeranFollowingArfcns l_union_OC_UInnerURedirGeranFollowingArfcns;
typedef struct l_union_OC_UInnerURedirRt l_union_OC_UInnerURedirRt;
typedef struct l_unnamed0 l_unnamed0;
typedef struct l_unnamed1 l_unnamed1;
typedef struct l_unnamed10 l_unnamed10;
typedef struct l_unnamed100 l_unnamed100;
typedef struct l_unnamed101 l_unnamed101;
typedef struct l_unnamed102 l_unnamed102;
typedef struct l_unnamed103 l_unnamed103;
typedef struct l_unnamed104 l_unnamed104;
typedef struct l_unnamed105 l_unnamed105;
typedef struct l_unnamed106 l_unnamed106;
typedef struct l_unnamed107 l_unnamed107;
typedef struct l_unnamed108 l_unnamed108;
typedef struct l_unnamed109 l_unnamed109;
typedef struct l_unnamed11 l_unnamed11;
typedef struct l_unnamed12 l_unnamed12;
typedef struct l_unnamed13 l_unnamed13;
typedef struct l_unnamed14 l_unnamed14;
typedef struct l_unnamed15 l_unnamed15;
typedef struct l_unnamed16 l_unnamed16;
typedef struct l_unnamed17 l_unnamed17;
typedef struct l_unnamed18 l_unnamed18;
typedef struct l_unnamed19 l_unnamed19;
typedef struct l_unnamed2 l_unnamed2;
typedef struct l_unnamed20 l_unnamed20;
typedef struct l_unnamed21 l_unnamed21;
typedef struct l_unnamed22 l_unnamed22;
typedef struct l_unnamed23 l_unnamed23;
typedef struct l_unnamed24 l_unnamed24;
typedef struct l_unnamed25 l_unnamed25;
typedef struct l_unnamed26 l_unnamed26;
typedef struct l_unnamed27 l_unnamed27;
typedef struct l_unnamed28 l_unnamed28;
typedef struct l_unnamed29 l_unnamed29;
typedef struct l_unnamed3 l_unnamed3;
typedef struct l_unnamed30 l_unnamed30;
typedef struct l_unnamed31 l_unnamed31;
typedef struct l_unnamed32 l_unnamed32;
typedef struct l_unnamed33 l_unnamed33;
typedef struct l_unnamed34 l_unnamed34;
typedef struct l_unnamed35 l_unnamed35;
typedef struct l_unnamed36 l_unnamed36;
typedef struct l_unnamed37 l_unnamed37;
typedef struct l_unnamed38 l_unnamed38;
typedef struct l_unnamed39 l_unnamed39;
typedef struct l_unnamed4 l_unnamed4;
typedef struct l_unnamed40 l_unnamed40;
typedef struct l_unnamed41 l_unnamed41;
typedef struct l_unnamed42 l_unnamed42;
typedef struct l_unnamed43 l_unnamed43;
typedef struct l_unnamed44 l_unnamed44;
typedef struct l_unnamed45 l_unnamed45;
typedef struct l_unnamed46 l_unnamed46;
typedef struct l_unnamed47 l_unnamed47;
typedef struct l_unnamed48 l_unnamed48;
typedef struct l_unnamed49 l_unnamed49;
typedef struct l_unnamed5 l_unnamed5;
typedef struct l_unnamed50 l_unnamed50;
typedef struct l_unnamed51 l_unnamed51;
typedef struct l_unnamed52 l_unnamed52;
typedef struct l_unnamed53 l_unnamed53;
typedef struct l_unnamed54 l_unnamed54;
typedef struct l_unnamed55 l_unnamed55;
typedef struct l_unnamed56 l_unnamed56;
typedef struct l_unnamed57 l_unnamed57;
typedef struct l_unnamed58 l_unnamed58;
typedef struct l_unnamed59 l_unnamed59;
typedef struct l_unnamed6 l_unnamed6;
typedef struct l_unnamed60 l_unnamed60;
typedef struct l_unnamed61 l_unnamed61;
typedef struct l_unnamed62 l_unnamed62;
typedef struct l_unnamed63 l_unnamed63;
typedef struct l_unnamed64 l_unnamed64;
typedef struct l_unnamed65 l_unnamed65;
typedef struct l_unnamed66 l_unnamed66;
typedef struct l_unnamed67 l_unnamed67;
typedef struct l_unnamed68 l_unnamed68;
typedef struct l_unnamed69 l_unnamed69;
typedef struct l_unnamed7 l_unnamed7;
typedef struct l_unnamed70 l_unnamed70;
typedef struct l_unnamed71 l_unnamed71;
typedef struct l_unnamed72 l_unnamed72;
typedef struct l_unnamed73 l_unnamed73;
typedef struct l_unnamed74 l_unnamed74;
typedef struct l_unnamed75 l_unnamed75;
typedef struct l_unnamed76 l_unnamed76;
typedef struct l_unnamed77 l_unnamed77;
typedef struct l_unnamed78 l_unnamed78;
typedef struct l_unnamed79 l_unnamed79;
typedef struct l_unnamed8 l_unnamed8;
typedef struct l_unnamed80 l_unnamed80;
typedef struct l_unnamed81 l_unnamed81;
typedef struct l_unnamed82 l_unnamed82;
typedef struct l_unnamed83 l_unnamed83;
typedef struct l_unnamed84 l_unnamed84;
typedef struct l_unnamed85 l_unnamed85;
typedef struct l_unnamed86 l_unnamed86;
typedef struct l_unnamed87 l_unnamed87;
typedef struct l_unnamed88 l_unnamed88;
typedef struct l_unnamed89 l_unnamed89;
typedef struct l_unnamed9 l_unnamed9;
typedef struct l_unnamed90 l_unnamed90;
typedef struct l_unnamed91 l_unnamed91;
typedef struct l_unnamed92 l_unnamed92;
typedef struct l_unnamed93 l_unnamed93;
typedef struct l_unnamed94 l_unnamed94;
typedef struct l_unnamed95 l_unnamed95;
typedef struct l_unnamed96 l_unnamed96;
typedef struct l_unnamed97 l_unnamed97;
typedef struct l_unnamed98 l_unnamed98;
typedef struct l_unnamed99 l_unnamed99;

/* Structure contents */
struct l_class_OC_BaseEvent {  unsigned char field0;};

struct l_class_OC_CEvent {  unsigned int  (**field0) ( int, ...);  unsigned int field1;  unsigned int field2;};

struct l_struct_OC_std_KD__KD_basic_string_MD_char_MC__AC_std_KD__KD_char_traits_MD_char_OD__MC__AC_std_KD__KD_allocator_MD_char_OD__AC__OD__KD__KD__Alloc_hider {  unsigned char *field0;};

struct l_class_OC_std_KD__KD_basic_string {  struct l_struct_OC_std_KD__KD_basic_string_MD_char_MC__AC_std_KD__KD_char_traits_MD_char_OD__MC__AC_std_KD__KD_allocator_MD_char_OD__AC__OD__KD__KD__Alloc_hider field0;};

struct l_class_OC_CStateBase {  unsigned int  (**field0) ( int, ...);  struct l_class_OC_std_KD__KD_basic_string field1;  struct l_class_OC_CStateBase *field2;  struct l_class_OC_CStateBase *field3;  struct l_class_OC_CStateBase *field4;};

struct l_unnamed82 { unsigned char array[40]; };

struct l_struct_OC_SS1apUEAggregateMaximumBitrate {  unsigned long long field0;  unsigned long long field1;};

struct l_class_OC_CStateT {  struct l_unnamed82 field0;  struct l_class_OC_Uec_KD__KD_UecDirectTransfer *field1;  struct l_struct_OC_SS1apUEAggregateMaximumBitrate field2;  struct l_class_OC_CStateBase *field3;  struct l_struct_OC_SS1apUEAggregateMaximumBitrate field4;  struct l_struct_OC_SS1apUEAggregateMaximumBitrate field5;};

struct l_unnamed18 { unsigned char array[24]; };

struct l_class_OC_std_KD__KD_vector {  struct l_unnamed18 field0;};

struct l_class_OC_Common_KD__KD_CFsmBase {  unsigned int  (**field0) ( int, ...);  struct l_class_OC_ICcsLog *field1;  struct l_class_OC_IFsmExitHandler *field2;  unsigned int field3;  unsigned char *field4;  struct l_class_OC_std_KD__KD_basic_string field5;  struct l_class_OC_CStateBase *field6;  struct l_class_OC_CStateBase *field7;  unsigned int field8;  unsigned int field9;  struct l_class_OC_std_KD__KD_vector field10;  struct l_class_OC_std_KD__KD_vector field11;};

struct l_class_OC_Uec_KD__KD_UecBaseData {  unsigned int  (**field0) ( int, ...);};

struct l_unnamed94 { unsigned char array[8]; };

struct l_struct_OC_SAmRlcPbTab {  unsigned int field0;  unsigned int field1;  unsigned int field2;};

struct l_unnamed12 { struct l_struct_OC_SAmRlcPbTab array[6]; };

struct l_struct_OC_SCellcSchedulingWeight {  unsigned int field0;  unsigned int field1;};

struct l_struct_OC_SErrcDeltaFListPUCCH {  unsigned int field0;  unsigned int field1;  unsigned int field2;  unsigned int field3;  unsigned int field4;};

struct l_unnamed104 { struct l_struct_OC_SErrcDeltaFListPUCCH array[2]; };

struct l_struct_OC_SErrcPDSCHConfigCommon {  unsigned char field0;  unsigned char field1;};

struct l_struct_OC_SPdcpProf {  unsigned int field0;  struct l_struct_OC_SErrcPDSCHConfigCommon field1;  unsigned short field2;  unsigned int field3;};

struct l_unnamed24 { struct l_struct_OC_SPdcpProf array[2]; };

struct l_struct_OC_SQciTab {  unsigned int field0;  unsigned int field1;  unsigned int field2;  unsigned int field3;  unsigned int field4;  unsigned int field5;  unsigned int field6;  unsigned int field7;  unsigned int field8;  unsigned int field9;  unsigned int field10;  unsigned int field11;};

struct l_unnamed25 { struct l_struct_OC_SQciTab array[9]; };

struct l_unnamed105 { struct l_struct_OC_SAmRlcPbTab array[5]; };

struct l_struct_OC_SAntennaInfo {  unsigned int field0;};

struct l_struct_OC_SCellcCqiConfigurationParameter {  unsigned int field0;  unsigned int field1;  unsigned int field2;  unsigned int field3;  unsigned int field4;  unsigned int field5;};

struct l_struct_OC_SArrayOfOaMBcPlmnIdList {  unsigned int field0;  struct l_unnamed12 field1;};

struct l_struct_OC_SAdjacentCellInfo {  unsigned int field0;  unsigned int field1;  unsigned int field2;  unsigned int field3;  unsigned int field4;  struct l_struct_OC_SArrayOfOaMBcPlmnIdList field5;  unsigned int field6;};

struct l_unnamed26 { struct l_struct_OC_SAdjacentCellInfo array[6]; };

struct l_struct_OC_SOaMBlacklistHoL {  struct l_struct_OC_SCellcSchedulingWeight field0;  unsigned int field1;};

struct l_unnamed106 { struct l_struct_OC_SOaMBlacklistHoL array[502]; };

struct l_struct_OC_SArrayOfOaMBlacklistHoL {  unsigned int field0;  unsigned int field1;  struct l_unnamed106 field2;};

struct l_struct_OC_SHoData {  unsigned int field0;  unsigned int field1;  unsigned int field2;  unsigned int field3;  struct l_unnamed26 field4;  unsigned int field5;  unsigned int field6;  unsigned int field7;  unsigned int field8;  unsigned int field9;  unsigned int field10;  unsigned int field11;  unsigned int field12;  unsigned int field13;  unsigned int field14;  unsigned int field15;  unsigned int field16;  unsigned int field17;  unsigned int field18;  struct l_struct_OC_SArrayOfOaMBlacklistHoL field19;};

struct l_struct_OC_SHoMeasData {  unsigned int field0;  unsigned int field1;  unsigned int field2;  unsigned int field3;  unsigned int field4;  unsigned int field5;  unsigned int field6;  unsigned int field7;  unsigned int field8;  unsigned int field9;  unsigned int field10;  unsigned int field11;  unsigned int field12;  unsigned int field13;  unsigned int field14;  unsigned int field15;  unsigned int field16;  unsigned int field17;  unsigned int field18;  unsigned int field19;  unsigned int field20;};

struct l_unnamed72 { struct l_struct_OC_SCellcSchedulingWeight array[32]; };

struct l_struct_OC_SArrayOfOaMAdjWInfList {  unsigned int field0;  struct l_unnamed72 field1;};

struct l_unnamed28 { struct l_struct_OC_SCellcSchedulingWeight array[6]; };

struct l_struct_OC_SArrayOfOaMUTargetPlmnIdL {  unsigned int field0;  struct l_unnamed28 field1;};

struct l_struct_OC_SLnAdjW {  unsigned int field0;  struct l_struct_OC_SArrayOfOaMUTargetPlmnIdL field1;  unsigned int field2;  unsigned int field3;  unsigned int field4;  unsigned int field5;  unsigned int field6;  unsigned int field7;};

struct l_unnamed27 { struct l_struct_OC_SLnAdjW array[32]; };

struct l_struct_OC_SLnHoW {  unsigned int field0;  unsigned int field1;  unsigned int field2;  unsigned int field3;  unsigned int field4;  unsigned int field5;  unsigned int field6;  unsigned int field7;  unsigned int field8;};

struct l_unnamed107 { struct l_struct_OC_SLnHoW array[16]; };

struct l_struct_OC_SHoIRatWcdma {  unsigned int field0;  unsigned int field1;  unsigned int field2;  unsigned int field3;  unsigned int field4;  struct l_struct_OC_SArrayOfOaMAdjWInfList field5;  unsigned int field6;  struct l_unnamed27 field7;  unsigned int field8;  struct l_unnamed107 field9;};

struct l_struct_OC_SCqiInfo {  unsigned int field0;  unsigned int field1;  unsigned int field2;  unsigned int field3;  unsigned int field4;  unsigned int field5;  unsigned int field6;  unsigned int field7;};

struct l_struct_OC_SSecurity {  unsigned int field0;  unsigned int field1;  unsigned int field2;  struct l_struct_OC_SAmRlcPbTab field3;  struct l_struct_OC_SAmRlcPbTab field4;};

struct l_unnamed73 { unsigned int array[31]; };

struct l_struct_OC_SOaMRedirGeranArfcnValueL {  unsigned int field0;  unsigned int field1;  struct l_unnamed73 field2;};

struct l_struct_OC_SRedirGeranExplicitListOfArfcns {  struct l_struct_OC_SOaMRedirGeranArfcnValueL field0;};

struct l_union_OC_UInnerURedirGeranFollowingArfcns {  struct l_struct_OC_SRedirGeranExplicitListOfArfcns field0;};

struct l_struct_OC_URedirGeranFollowingArfcns {  unsigned int field0;  struct l_union_OC_UInnerURedirGeranFollowingArfcns field1;};

struct l_struct_OC_SRedirRatGeran {  struct l_struct_OC_SCellcSchedulingWeight field0;  struct l_struct_OC_SCellcSchedulingWeight field1;  unsigned int field2;  struct l_struct_OC_URedirGeranFollowingArfcns field3;};

struct l_union_OC_UInnerURedirRt {  struct l_struct_OC_SRedirRatGeran field0;};

struct l_struct_OC_URedirRt {  unsigned int field0;  struct l_union_OC_UInnerURedirRt field1;};

struct l_struct_OC_SRedirectionTarget {  unsigned int field0;  unsigned int field1;  struct l_struct_OC_URedirRt field2;};

struct l_unnamed29 { struct l_struct_OC_SRedirectionTarget array[2]; };

struct l_struct_OC_SRedirection {  unsigned int field0;  unsigned int field1;  struct l_unnamed29 field2;};

struct l_unnamed85 { unsigned char array[1]; };

struct l_struct_OC_SErrcAcBarringForSpecialAC {  struct l_unnamed85 field0;};

struct l_unnamed83 { unsigned char array[2]; };

struct l_struct_OC_SErrcN4TxAntennaTm5 {  struct l_unnamed83 field0;};

struct l_struct_OC_SErrcMasterInformationBlock {  unsigned int field0;  struct l_struct_OC_SCellcSchedulingWeight field1;  struct l_struct_OC_SErrcAcBarringForSpecialAC field2;  struct l_struct_OC_SErrcN4TxAntennaTm5 field3;  unsigned char field4;};

struct l_struct_OC_SErrcPLMNIdentityInfo {  struct l_struct_OC_SAmRlcPbTab field0;  unsigned int field1;};

struct l_unnamed108 { struct l_struct_OC_SErrcPLMNIdentityInfo array[6]; };

struct l_struct_OC_SErrcPLMNIdentityList {  unsigned char field0;  unsigned char field1;  unsigned char field2;  unsigned char field3;  struct l_unnamed108 field4;};

struct l_unnamed54 { unsigned char array[4]; };

struct l_struct_OC_SErrcCsgIdentity {  struct l_unnamed54 field0;};

struct l_struct_OC_SErrcCellAccessRelatedInfo {  struct l_struct_OC_SErrcPLMNIdentityList field0;  unsigned int field1;  unsigned int field2;  unsigned int field3;  unsigned int field4;  unsigned char field5;  unsigned char field6;  unsigned char field7;  unsigned char field8;  unsigned int field9;  struct l_struct_OC_SErrcCsgIdentity field10;};

struct l_struct_OC_SErrcCellSelectionInfo {  unsigned char field0;  unsigned char field1;  unsigned char field2;  unsigned char field3;  unsigned int field4;  unsigned char field5;  unsigned char field6;  unsigned char field7;  unsigned char field8;};

struct l_struct_OC_SErrcSIBMappingInfo {  unsigned char field0;  unsigned char field1;  unsigned char field2;  unsigned char field3;  struct l_unnamed73 field4;};

struct l_struct_OC_SErrcSchedulingInfo {  unsigned int field0;  struct l_struct_OC_SErrcSIBMappingInfo field1;};

struct l_unnamed30 { struct l_struct_OC_SErrcSchedulingInfo array[32]; };

struct l_struct_OC_SErrcSchedulingInfoList {  unsigned char field0;  unsigned char field1;  unsigned char field2;  unsigned char field3;  struct l_unnamed30 field4;};

struct l_struct_OC_SErrcSystemInformationBlockType1 {  struct l_struct_OC_SErrcCellAccessRelatedInfo field0;  struct l_struct_OC_SErrcCellSelectionInfo field1;  unsigned int field2;  unsigned char field3;  unsigned char field4;  unsigned short field5;  struct l_struct_OC_SErrcSchedulingInfoList field6;  unsigned int field7;  struct l_struct_OC_SCellcSchedulingWeight field8;  unsigned int field9;  unsigned char field10;  unsigned char field11;  unsigned char field12;  unsigned char field13;  unsigned int field14;  struct l_class_OC_BaseEvent field15;  unsigned char field16;  unsigned char field17;  unsigned char field18;};

struct l_struct_OC_SErrcACBarringConfig {  unsigned int field0;  unsigned int field1;  struct l_struct_OC_SErrcAcBarringForSpecialAC field2;  unsigned char field3;  unsigned char field4;  unsigned char field5;};

struct l_struct_OC_SErrcAcBarringInfo {  unsigned char field0;  unsigned char field1;  unsigned char field2;  unsigned char field3;  unsigned int field4;  struct l_struct_OC_SErrcACBarringConfig field5;  unsigned int field6;  struct l_struct_OC_SErrcACBarringConfig field7;};

struct l_struct_OC_SErrcPreambleInfo {  unsigned int field0;  unsigned int field1;  struct l_struct_OC_SAmRlcPbTab field2;};

struct l_struct_OC_SErrcRACHConfigCommon {  struct l_struct_OC_SErrcPreambleInfo field0;  struct l_struct_OC_SCellcSchedulingWeight field1;  struct l_struct_OC_SAmRlcPbTab field2;  unsigned char field3;  unsigned char field4;  unsigned char field5;  unsigned char field6;};

struct l_struct_OC_SErrcPRACHConfigInfo {  unsigned char field0;  unsigned char field1;  unsigned char field2;  unsigned char field3;};

struct l_struct_OC_SErrcPRACHConfigSIB {  unsigned short field0;  struct l_struct_OC_SErrcPRACHConfigInfo field1;};

struct l_struct_OC_SErrcPuschConfigBasic {  unsigned char field0;  unsigned char field1;  unsigned char field2;  unsigned char field3;  unsigned int field4;  unsigned char field5;  unsigned char field6;  unsigned short field7;};

struct l_struct_OC_SErrcPUSCHConfigCommon {  struct l_struct_OC_SErrcPuschConfigBasic field0;  struct l_struct_OC_SErrcPRACHConfigInfo field1;};

struct l_struct_OC_SErrcPUCCHConfigCommon {  unsigned int field0;  unsigned char field1;  unsigned char field2;  unsigned short field3;};

struct l_struct_OC_SErrcSetupSoundingRSULConfigCommon {  unsigned int field0;  unsigned int field1;  unsigned char field2;  unsigned char field3;  unsigned char field4;  unsigned char field5;  unsigned int field6;  unsigned int field7;};

struct l_union_OC_UInnerUErrcSoundingRSULConfigCommon {  struct l_struct_OC_SErrcSetupSoundingRSULConfigCommon field0;};

struct l_struct_OC_UErrcSoundingRSULConfigCommon {  unsigned int field0;  struct l_union_OC_UInnerUErrcSoundingRSULConfigCommon field1;};

struct l_struct_OC_SErrcUplinkPowerControlCommon {  unsigned char field0;  unsigned char field1;  unsigned char field2;  unsigned char field3;  unsigned int field4;  unsigned char field5;  unsigned char field6;  unsigned char field7;  unsigned char field8;  struct l_struct_OC_SErrcDeltaFListPUCCH field9;  unsigned char field10;  unsigned char field11;  unsigned char field12;  unsigned char field13;};

struct l_struct_OC_SErrcRadioResourceConfigCommonSIB {  struct l_struct_OC_SErrcRACHConfigCommon field0;  struct l_struct_OC_SAntennaInfo field1;  struct l_struct_OC_SCellcSchedulingWeight field2;  struct l_struct_OC_SErrcPRACHConfigSIB field3;  struct l_struct_OC_SErrcPDSCHConfigCommon field4;  struct l_struct_OC_SErrcPUSCHConfigCommon field5;  struct l_struct_OC_SErrcPUCCHConfigCommon field6;  struct l_struct_OC_UErrcSoundingRSULConfigCommon field7;  struct l_struct_OC_SErrcUplinkPowerControlCommon field8;  unsigned int field9;};

struct l_struct_OC_SErrcFreqInfo {  unsigned int field0;  unsigned int field1;  unsigned int field2;  unsigned int field3;  unsigned char field4;  unsigned char field5;  unsigned char field6;  unsigned char field7;};

struct l_unnamed50 { unsigned char array[3]; };

struct l_struct_OC_SErrcFourFrames {  struct l_unnamed50 field0;};

struct l_union_OC_UInnerUErrcSubframeAllocation {  struct l_struct_OC_SErrcFourFrames field0;};

struct l_struct_OC_UErrcSubframeAllocation {  unsigned int field0;  struct l_union_OC_UInnerUErrcSubframeAllocation field1;};

struct l_struct_OC_SErrcMBSFNSubframeConfig {  unsigned int field0;  unsigned char field1;  unsigned char field2;  unsigned char field3;  unsigned char field4;  struct l_struct_OC_UErrcSubframeAllocation field5;};

struct l_unnamed109 { struct l_struct_OC_SErrcMBSFNSubframeConfig array[8]; };

struct l_struct_OC_SErrcMBSFNSubframeConfigList {  unsigned char field0;  unsigned char field1;  unsigned char field2;  unsigned char field3;  struct l_unnamed109 field4;};

struct l_struct_OC_SErrcSystemInformationBlockType2 {  unsigned int field0;  struct l_struct_OC_SErrcAcBarringInfo field1;  struct l_struct_OC_SErrcRadioResourceConfigCommonSIB field2;  struct l_struct_OC_SCellcCqiConfigurationParameter field3;  struct l_struct_OC_SErrcFreqInfo field4;  unsigned int field5;  struct l_struct_OC_SErrcMBSFNSubframeConfigList field6;  unsigned int field7;};

struct l_struct_OC_Uec_KD__KD_SUecSysInfoData {  struct l_struct_OC_SErrcMasterInformationBlock field0;  struct l_struct_OC_SErrcSystemInformationBlockType1 field1;  struct l_struct_OC_SErrcSystemInformationBlockType2 field2;};

struct l_struct_OC_Uec_KD__KD_SUecCellSetup {  unsigned int field0;  unsigned char field1;  struct l_unnamed12 field2;  unsigned int field3;  struct l_struct_OC_SCellcSchedulingWeight field4;  struct l_struct_OC_SAmRlcPbTab field5;  unsigned int field6;  struct l_unnamed104 field7;  unsigned int field8;  struct l_unnamed24 field9;  unsigned int field10;  struct l_unnamed25 field11;  struct l_unnamed105 field12;  struct l_struct_OC_SAntennaInfo field13;  struct l_struct_OC_SAmRlcPbTab field14;  struct l_struct_OC_SCellcCqiConfigurationParameter field15;  struct l_struct_OC_SAntennaInfo field16;  struct l_struct_OC_SCellcCqiConfigurationParameter field17;  struct l_struct_OC_SHoData field18;  struct l_struct_OC_SHoMeasData field19;  struct l_struct_OC_SHoIRatWcdma field20;  struct l_struct_OC_SCqiInfo field21;  struct l_struct_OC_SSecurity field22;  struct l_struct_OC_SRedirection field23;  struct l_struct_OC_SAmRlcPbTab field24;  struct l_struct_OC_SAntennaInfo field25;  struct l_struct_OC_Uec_KD__KD_SUecSysInfoData field26;};

struct l_struct_OC_Uec_KD__KD_SUecCellData {  unsigned int field0;  unsigned int field1;  unsigned int field2;  unsigned int field3;  struct l_struct_OC_Uec_KD__KD_SUecCellSetup field4;};

struct l_struct_OC_std_KD__KD__Rb_tree_node_base {  unsigned int field0;  struct l_struct_OC_std_KD__KD__Rb_tree_node_base *field1;  struct l_struct_OC_std_KD__KD__Rb_tree_node_base *field2;  struct l_struct_OC_std_KD__KD__Rb_tree_node_base *field3;};

struct l_struct_OC_std_KD__KD__Rb_tree_MD_EPmCounterId_MC__AC_std_KD__KD_pair_MD_EPmCounterId_AC_const_MC__AC_Common_KD__KD_LomBH_KD__KD_IfCommonLomBrowserHandler_KD__KD_CounterHandle_OD__MC__AC_std_KD__KD__Select1st_MD_std_KD__KD_pair_MD_EPmCounterId_AC_const_MC__AC_Common_KD__KD_LomBH_KD__KD_IfCommonLomBrowserHandler_KD__KD_CounterHandle_OD__AC__OD__MC__AC_std_KD__KD_less_MD_EPmCounterId_OD__MC__AC_std_KD__KD_allocator_MD_std_KD__KD_pair_MD_EPmCounterId_AC_const_MC__AC_Common_KD__KD_LomBH_KD__KD_IfCommonLomBrowserHandler_KD__KD_CounterHandle_OD__AC__OD__AC__OD__KD__KD__Rb_tree_impl {  struct l_class_OC_BaseEvent field0;  struct l_struct_OC_std_KD__KD__Rb_tree_node_base field1;  unsigned long long field2;};

struct l_class_OC_std_KD__KD__Rb_tree {  struct l_struct_OC_std_KD__KD__Rb_tree_MD_EPmCounterId_MC__AC_std_KD__KD_pair_MD_EPmCounterId_AC_const_MC__AC_Common_KD__KD_LomBH_KD__KD_IfCommonLomBrowserHandler_KD__KD_CounterHandle_OD__MC__AC_std_KD__KD__Select1st_MD_std_KD__KD_pair_MD_EPmCounterId_AC_const_MC__AC_Common_KD__KD_LomBH_KD__KD_IfCommonLomBrowserHandler_KD__KD_CounterHandle_OD__AC__OD__MC__AC_std_KD__KD_less_MD_EPmCounterId_OD__MC__AC_std_KD__KD_allocator_MD_std_KD__KD_pair_MD_EPmCounterId_AC_const_MC__AC_Common_KD__KD_LomBH_KD__KD_IfCommonLomBrowserHandler_KD__KD_CounterHandle_OD__AC__OD__AC__OD__KD__KD__Rb_tree_impl field0;};

struct l_class_OC_std_KD__KD_map {  struct l_class_OC_std_KD__KD__Rb_tree field0;};

struct l_class_OC_Uec_KD__KD_UecCellData {  struct l_unnamed94 field0;  struct l_struct_OC_Uec_KD__KD_SUecCellData field1;  struct l_class_OC_std_KD__KD_map field2;};

struct l_class_OC_Uec_KD__KD_UecDebugCallstack {  struct l_class_OC_std_KD__KD_basic_string field0;};

struct l_unnamed90 { unsigned char array[207]; };

struct l_struct_OC_SAsnDynstr {  unsigned int field0;  unsigned char *field1;};

struct l_class_OC_Uec_KD__KD_UecDirectTransfer {  struct l_unnamed90 field0;  struct l_class_OC_CStateT *field1;  struct l_struct_OC_SAsnDynstr field2;};

struct l_unnamed92 { unsigned char array[16]; };

struct l_class_OC_Uec_KD__KD_UecEvent {  struct l_unnamed92 field0;  unsigned int field1;  unsigned int field2;  unsigned int field3;  unsigned char *field4;  unsigned int field5;  unsigned int field6;  unsigned char *field7;  struct l_class_OC_Uec_KD__KD_UecBaseData *field8;  unsigned int field9;};

struct l_unnamed91 { unsigned char array[80]; };

struct l_class_OC_std_KD__KD_deque {  struct l_unnamed91 field0;};

struct l_class_OC_std_KD__KD_queue {  struct l_class_OC_std_KD__KD_deque field0;};

struct l_class_OC_Uec_KD__KD_UecEventBuffer {  struct l_class_OC_std_KD__KD_queue field0;};

struct l_unnamed79 { unsigned char array[120]; };

struct l_class_OC_Uec_KD__KD_UecFsmBase {  struct l_unnamed79 field0;};

struct l_unnamed19 { struct l_class_OC_Uec_KD__KD_UecBaseData *array[56]; };

struct l_class_OC_Uec_KD__KD_UecManagerBase {  struct l_unnamed94 field0;  unsigned int field1;  unsigned int field2;  struct l_class_OC_std_KD__KD_queue field3;  struct l_class_OC_Uec_KD__KD_UecBaseData *field4;  unsigned int field5;  unsigned int field6;  struct l_unnamed19 field7;  struct l_class_OC_std_KD__KD_queue field8;  unsigned char field9;  unsigned char field10;  unsigned char field11;  struct l_class_OC_std_KD__KD_map field12;  struct l_class_OC_Uec_KD__KD_UecBaseData *field13;};

struct l_class_OC_std_KD__KD_list {  struct l_unnamed92 field0;};

struct l_class_OC_Uec_KD__KD_UecServiceBase {  struct l_unnamed94 field0;  struct l_unnamed79 field1;  struct l_class_OC_Uec_KD__KD_UecBaseData *field2;  struct l_class_OC_Uec_KD__KD_UecBaseData *field3;  struct l_class_OC_Uec_KD__KD_UecManagerBase *field4;  unsigned int field5;  unsigned int field6;  unsigned int field7;  struct l_unnamed54 field8;  struct l_class_OC_std_KD__KD_list field9;  unsigned int field10;  struct l_class_OC_Uec_KD__KD_UecBaseData *field11;  unsigned char field12;  unsigned char field13;  unsigned char field14;  unsigned char field15;  unsigned char field16;  unsigned char field17;  unsigned char field18;};

struct l_unnamed102 { unsigned char array[208]; };

struct l_struct_OC_Uec_KD__KD_UecTimerWheel_KD__KD_TimeSlot {  struct l_struct_OC_Uec_KD__KD_UecTimerWheel_KD__KD_TimerBlock *field0;  unsigned short field1;  unsigned short field2;};

struct l_unnamed23 { struct l_struct_OC_Uec_KD__KD_UecTimerWheel_KD__KD_TimeSlot array[32]; };

struct l_struct_OC_Uec_KD__KD_UecTimerWheel_KD__KD_TimerBlock {  struct l_struct_OC_Uec_KD__KD_UecTimerWheel_KD__KD_TimerBlock *field0;  struct l_struct_OC_Uec_KD__KD_UecTimerWheel_KD__KD_TimerBlock *field1;  struct l_struct_OC_Uec_KD__KD_UecTimerWheel_KD__KD_TimeSlot *field2;  unsigned int field3;  unsigned int field4;};

struct l_unnamed103 { struct l_struct_OC_Uec_KD__KD_UecTimerWheel_KD__KD_TimerBlock array[32]; };

struct l_unnamed74 { struct l_struct_OC_Uec_KD__KD_UecTimerWheel_KD__KD_TimerBlock array[1000]; };

struct l_class_OC_Uec_KD__KD_UecTimerWheel {  unsigned int  (**field0) ( int, ...);  struct l_unnamed23 field1;  struct l_unnamed103 field2;  struct l_unnamed74 field3;  struct l_struct_OC_Uec_KD__KD_UecTimerWheel_KD__KD_TimerBlock *field4;  unsigned int field5;  unsigned int field6;  unsigned int field7;  unsigned int field8;  struct l_class_OC_Uec_KD__KD_UecTimePort *field9;};

struct l_class_OC_Uec_KD__KD_UecTimePort {  struct l_unnamed102 field0;  unsigned int field1;  unsigned int field2;  struct l_class_OC_Uec_KD__KD_UecTimerWheel field3;  struct l_class_OC_Uec_KD__KD_UecBaseData *field4;  struct l_class_OC_std_KD__KD_map field5;  unsigned char *field6;  unsigned int field7;};

struct l_struct_OC_SSTMsi {  unsigned int field0;  unsigned char field1;  unsigned char field2;  unsigned char field3;  unsigned char field4;};

struct l_union_OC_UInnerUErrcInitialUEIdentity {  struct l_struct_OC_SSTMsi field0;};

struct l_struct_OC_UErrcInitialUEIdentity {  unsigned int field0;  struct l_union_OC_UInnerUErrcInitialUEIdentity field1;};

struct l_struct_OC_SCellcSchedulingRequestConfiguration {  unsigned int field0;  unsigned int field1;  unsigned int field2;  unsigned int field3;};

struct l_struct_OC_SCellcSrbConfiguration {  struct l_struct_OC_SCellcSchedulingWeight field0;  unsigned int field1;  struct l_struct_OC_SCellcSchedulingRequestConfiguration field2;};

struct l_struct_OC_Uec_KD__KD_SUecUeSb {  unsigned char field0;  unsigned int field1;  unsigned int field2;  unsigned int field3;  unsigned int field4;};

struct l_unnamed59 { struct l_struct_OC_Uec_KD__KD_SUecUeSb array[2]; };

struct l_struct_OC_Uec_KD__KD_SUecUeSbData {  unsigned int field0;  unsigned char field1;  struct l_struct_OC_SCellcSrbConfiguration field2;  struct l_unnamed59 field3;};

struct l_struct_OC_STransportLayerAddress {  unsigned int field0;  struct l_unnamed92 field1;};

struct l_struct_OC_SErrcN4TxAntennaTm4 {  struct l_unnamed94 field0;};

struct l_union_OC_UInnerUErrcCodebookSubsetRestriction {  struct l_struct_OC_SErrcN4TxAntennaTm4 field0;};

struct l_struct_OC_UErrcCodebookSubsetRestriction {  unsigned int field0;  struct l_union_OC_UInnerUErrcCodebookSubsetRestriction field1;};

struct l_struct_OC_SCellcDrbConfiguration {  unsigned int field0;  unsigned int field1;  unsigned int field2;  unsigned int field3;  unsigned int field4;  unsigned int field5;  struct l_struct_OC_UErrcCodebookSubsetRestriction field6;};

struct l_struct_OC_SX2apAllocationAndRetentionPriority {  unsigned char field0;  unsigned char field1;  unsigned char field2;  unsigned char field3;  unsigned int field4;  unsigned int field5;};

struct l_struct_OC_Uec_KD__KD_SUecUeDb {  unsigned char field0;  unsigned int field1;  unsigned int field2;  unsigned char field3;  unsigned int field4;  unsigned int field5;  unsigned int field6;  unsigned int field7;  unsigned int field8;  unsigned int field9;  unsigned int field10;  unsigned char field11;  unsigned int field12;  unsigned char field13;  struct l_struct_OC_STransportLayerAddress field14;  unsigned char field15;  struct l_struct_OC_SCellcDrbConfiguration field16;  unsigned int field17;  unsigned int field18;  unsigned int field19;  unsigned int field20;  unsigned int field21;  unsigned int field22;  unsigned int field23;  unsigned char field24;  unsigned char field25;  struct l_struct_OC_SX2apAllocationAndRetentionPriority field26;};

struct l_unnamed21 { struct l_struct_OC_Uec_KD__KD_SUecUeDb array[6]; };

struct l_struct_OC_Uec_KD__KD_SUecUeDbData {  unsigned char field0;  struct l_struct_OC_SS1apUEAggregateMaximumBitrate field1;  unsigned int field2;  struct l_unnamed21 field3;};

struct l_struct_OC_SGummei {  struct l_struct_OC_SAmRlcPbTab field0;  unsigned short field1;  unsigned char field2;  unsigned char field3;};

struct l_struct_OC_US1apCause {  unsigned int field0;  struct l_struct_OC_SAntennaInfo field1;};

struct l_unnamed22 { struct l_struct_OC_SAmRlcPbTab array[15]; };

struct l_struct_OC_SS1apEPLMNs {  unsigned char field0;  unsigned char field1;  unsigned char field2;  unsigned char field3;  struct l_unnamed22 field4;};

struct l_struct_OC_Uec_KD__KD_SUecForbiddenLACs {  unsigned short field0;  struct l_struct_OC_SErrcN4TxAntennaTm5 *field1;};

struct l_struct_OC_Uec_KD__KD_SUecForbiddenLAsItem {  struct l_struct_OC_SAmRlcPbTab field0;  struct l_struct_OC_Uec_KD__KD_SUecForbiddenLACs field1;};

struct l_unnamed60 { struct l_struct_OC_Uec_KD__KD_SUecForbiddenLAsItem array[16]; };

struct l_struct_OC_Uec_KD__KD_SUecForbiddenLAs {  unsigned char field0;  struct l_unnamed60 field1;};

struct l_struct_OC_Uec_KD__KD_SUecHandoverRestrictionList {  struct l_struct_OC_SAmRlcPbTab field0;  unsigned int field1;  struct l_struct_OC_SS1apEPLMNs field2;  unsigned int field3;  struct l_struct_OC_Uec_KD__KD_SUecForbiddenLAs field4;  unsigned int field5;  struct l_struct_OC_Uec_KD__KD_SUecForbiddenLAs field6;  unsigned int field7;  unsigned int field8;};

struct l_struct_OC_SErrcSetupSoundingRSULConfigDedicated {  unsigned int field0;  unsigned int field1;  unsigned char field2;  unsigned char field3;  unsigned short field4;  unsigned char field5;  unsigned char field6;  unsigned char field7;  unsigned char field8;  unsigned int field9;};

struct l_struct_OC_Uec_KD__KD_SUecUeData {  unsigned char field0;  unsigned int field1;  struct l_struct_OC_SGummei field2;  unsigned int field3;  unsigned int field4;  unsigned int field5;  unsigned int field6;  unsigned int field7;  struct l_struct_OC_SCellcCqiConfigurationParameter field8;  unsigned int field9;  struct l_struct_OC_US1apCause field10;  struct l_struct_OC_US1apCause field11;  unsigned int field12;  struct l_struct_OC_Uec_KD__KD_SUecHandoverRestrictionList field13;  unsigned int field14;  struct l_struct_OC_SErrcSetupSoundingRSULConfigDedicated field15;  unsigned char field16;};

struct l_unnamed95 { unsigned int array[2]; };

struct l_struct_OC_Uec_KD__KD_SUecUeInactivity {  struct l_unnamed95 field0;};

struct l_struct_OC_Uec_KD__KD_SUecUeContextReleaseInfo {  unsigned int field0;  struct l_struct_OC_US1apCause field1;};

struct l_struct_OC_SErrcSupportedROHCProfiles {  unsigned char field0;  unsigned char field1;  unsigned char field2;  unsigned char field3;  unsigned char field4;  unsigned char field5;  unsigned char field6;  unsigned char field7;  unsigned char field8;};

struct l_struct_OC_SErrcPDCPParameters {  struct l_struct_OC_SErrcSupportedROHCProfiles field0;  unsigned char field1;  unsigned char field2;  unsigned char field3;  unsigned int field4;};

struct l_unnamed96 { struct l_struct_OC_SErrcPDSCHConfigCommon array[64]; };

struct l_struct_OC_SErrcSupportedBandListEUTRA {  unsigned char field0;  struct l_unnamed96 field1;};

struct l_struct_OC_SErrcRFParameters {  struct l_struct_OC_SErrcSupportedBandListEUTRA field0;  unsigned char field1;  unsigned char field2;  unsigned char field3;};

struct l_unnamed98 { struct l_class_OC_BaseEvent array[64]; };

struct l_struct_OC_SErrcInterFreqBandList {  unsigned char field0;  struct l_unnamed98 field1;};

struct l_struct_OC_SErrcBandInfoEUTRA {  struct l_struct_OC_SErrcInterFreqBandList field0;  unsigned char field1;  unsigned char field2;  unsigned char field3;  unsigned int field4;  struct l_struct_OC_SErrcInterFreqBandList field5;  unsigned char field6;  unsigned char field7;  unsigned char field8;};

struct l_unnamed97 { struct l_struct_OC_SErrcBandInfoEUTRA array[64]; };

struct l_struct_OC_SErrcBandListEUTRA {  unsigned char field0;  unsigned char field1;  unsigned char field2;  unsigned char field3;  struct l_unnamed97 field4;};

struct l_struct_OC_SErrcMeasParameters {  struct l_struct_OC_SErrcBandListEUTRA field0;};

struct l_unnamed99 { unsigned int array[64]; };

struct l_struct_OC_SErrcSupportedBandListGERAN {  unsigned char field0;  unsigned char field1;  unsigned char field2;  unsigned char field3;  struct l_unnamed99 field4;};

struct l_struct_OC_SErrcIRATParametersUTRAFDD {  struct l_struct_OC_SErrcSupportedBandListGERAN field0;};

struct l_struct_OC_SErrcIRATParametersGERAN {  struct l_struct_OC_SErrcSupportedBandListGERAN field0;  unsigned char field1;  unsigned char field2;  unsigned char field3;  unsigned char field4;};

struct l_unnamed100 { unsigned int array[32]; };

struct l_struct_OC_SErrcSupportedBandList1XRTT {  unsigned char field0;  unsigned char field1;  unsigned char field2;  unsigned char field3;  struct l_unnamed100 field4;};

struct l_struct_OC_SErrcIRATParametersCDMA20001XRTT {  struct l_struct_OC_SErrcSupportedBandList1XRTT field0;  unsigned int field1;  unsigned int field2;};

struct l_struct_OC_SErrcInterRATParameters {  unsigned int field0;  struct l_struct_OC_SErrcIRATParametersUTRAFDD field1;  unsigned int field2;  struct l_struct_OC_SErrcIRATParametersUTRAFDD field3;  unsigned int field4;  struct l_struct_OC_SErrcIRATParametersUTRAFDD field5;  unsigned int field6;  struct l_struct_OC_SErrcIRATParametersUTRAFDD field7;  unsigned int field8;  struct l_struct_OC_SErrcIRATParametersGERAN field9;  unsigned int field10;  struct l_struct_OC_SErrcIRATParametersCDMA20001XRTT field11;  unsigned int field12;  struct l_struct_OC_SErrcIRATParametersCDMA20001XRTT field13;};

struct l_struct_OC_SErrcUEEUTRACapability {  unsigned int field0;  unsigned char field1;  unsigned char field2;  unsigned char field3;  unsigned char field4;  struct l_struct_OC_SErrcPDCPParameters field5;  struct l_struct_OC_SErrcPDSCHConfigCommon field6;  unsigned short field7;  struct l_struct_OC_SErrcRFParameters field8;  struct l_struct_OC_SErrcMeasParameters field9;  unsigned int field10;  struct l_struct_OC_SErrcCsgIdentity field11;  struct l_struct_OC_SErrcInterRATParameters field12;  unsigned int field13;  struct l_class_OC_BaseEvent field14;  unsigned char field15;  unsigned char field16;  unsigned char field17;};

struct l_struct_OC_Uec_KD__KD_SUecUeCapabilityInfo {  struct l_struct_OC_SErrcUEEUTRACapability field0;  struct l_struct_OC_SAsnDynstr field1;  struct l_struct_OC_SAsnDynstr field2;};

struct l_struct_OC_SErrcMeasIdToAddMod {  unsigned char field0;  unsigned char field1;  unsigned char field2;};

struct l_unnamed101 { struct l_struct_OC_SErrcMeasIdToAddMod array[32]; };

struct l_struct_OC_SErrcMeasIdToAddModList {  unsigned char field0;  struct l_unnamed101 field1;};

struct l_struct_OC_Uec_KD__KD_SUecUeContextData {  unsigned int field0;  unsigned int field1;  unsigned int field2;  unsigned int field3;  unsigned int field4;  unsigned int field5;  unsigned int field6;  unsigned int field7;  unsigned char field8;  struct l_struct_OC_UErrcInitialUEIdentity field9;  unsigned int field10;  unsigned int field11;  unsigned int field12;  unsigned int field13;  unsigned int field14;  unsigned char field15;  unsigned char field16;  unsigned char field17;  unsigned int field18;  unsigned char field19;  unsigned int field20;  unsigned int field21;  unsigned int field22;  struct l_struct_OC_Uec_KD__KD_SUecUeSbData field23;  struct l_struct_OC_Uec_KD__KD_SUecUeDbData field24;  struct l_struct_OC_Uec_KD__KD_SUecUeData field25;  unsigned int field26;  struct l_struct_OC_Uec_KD__KD_SUecUeInactivity field27;  struct l_struct_OC_Uec_KD__KD_SUecUeContextReleaseInfo field28;  struct l_struct_OC_SCellcSchedulingWeight field29;  struct l_struct_OC_Uec_KD__KD_SUecUeCapabilityInfo field30;  unsigned int field31;  struct l_struct_OC_SErrcMeasIdToAddModList field32;};

struct l_class_OC_Uec_KD__KD_UecUeContextData {  struct l_unnamed94 field0;  struct l_struct_OC_Uec_KD__KD_SUecUeContextData field1;  struct l_class_OC_Uec_KD__KD_UecUeSecurityInformation *field2;  unsigned int field3;  struct l_class_OC_std_KD__KD_map field4;  struct l_class_OC_std_KD__KD_map field5;  struct l_class_OC_std_KD__KD_map field6;  struct l_class_OC_Uec_KD__KD_UecEventBuffer field7;  struct l_class_OC_std_KD__KD_map field8;};

struct l_unnamed61 { unsigned char array[32]; };

struct l_struct_OC_SS1apSecurityKey {  struct l_unnamed61 field0;};

struct l_class_OC_Uec_KD__KD_UecUeSecurityInformation {  unsigned int  (**field0) ( int, ...);  struct l_struct_OC_SAsnDynstr field1;  struct l_struct_OC_SAsnDynstr field2;  struct l_struct_OC_SCellcSchedulingWeight *field3;  struct l_struct_OC_SS1apSecurityKey field4;  unsigned int field5;  struct l_struct_OC_SS1apSecurityKey field6;  unsigned char field7;  unsigned char field8;  unsigned int field9;  struct l_unnamed92 field10;  struct l_unnamed92 field11;  struct l_unnamed92 field12;  struct l_class_OC_std_KD__KD_map field13;  unsigned int field14;  unsigned int field15;};

struct l_struct_OC_std_KD__KD_ios_base_KD__KD__Words {  unsigned char *field0;  unsigned long long field1;};

struct l_unnamed88 { struct l_struct_OC_std_KD__KD_ios_base_KD__KD__Words array[8]; };

struct l_class_OC_std_KD__KD_locale {  struct l_class_OC_std_KD__KD_locale_KD__KD__Impl *field0;};

struct l_class_OC_std_KD__KD_ios_base {  unsigned int  (**field0) ( int, ...);  unsigned long long field1;  unsigned long long field2;  unsigned int field3;  unsigned int field4;  unsigned int field5;  struct l_struct_OC_std_KD__KD_ios_base_KD__KD__Callback_list *field6;  struct l_struct_OC_std_KD__KD_ios_base_KD__KD__Words field7;  struct l_unnamed88 field8;  unsigned int field9;  struct l_struct_OC_std_KD__KD_ios_base_KD__KD__Words *field10;  struct l_class_OC_std_KD__KD_locale field11;};

struct l_class_OC_std_KD__KD_locale_KD__KD__Impl {  unsigned int field0;  struct l_class_OC_std_KD__KD_locale_KD__KD_facet **field1;  unsigned long long field2;  struct l_class_OC_std_KD__KD_locale_KD__KD_facet **field3;  unsigned char **field4;};

struct l_class_OC_std_KD__KD_locale_KD__KD_facet {  unsigned int  (**field0) ( int, ...);  unsigned int field1;};

struct l_union_OC_UInnerUErrcDedicatedInfoType {  struct l_struct_OC_SAsnDynstr field0;};

struct l_struct_OC_UErrcDedicatedInfoType {  unsigned int field0;  struct l_union_OC_UInnerUErrcDedicatedInfoType field1;};

struct l_struct_OC_SErrcDLInformationTransferR8IEs {  struct l_struct_OC_UErrcDedicatedInfoType field0;  unsigned int field1;  struct l_class_OC_BaseEvent field2;  unsigned char field3;  unsigned char field4;  unsigned char field5;};

struct l_union_OC_UInnerUErrcC1CriticalExtensionsDLInformationTransfer {  struct l_struct_OC_SErrcDLInformationTransferR8IEs field0;};

struct l_struct_OC_UErrcC1CriticalExtensionsDLInformationTransfer {  unsigned int field0;  struct l_union_OC_UInnerUErrcC1CriticalExtensionsDLInformationTransfer field1;};

struct l_union_OC_UInnerUErrcCriticalExtensionsDLInformationTransfer {  struct l_struct_OC_UErrcC1CriticalExtensionsDLInformationTransfer field0;};

struct l_struct_OC_UErrcCriticalExtensionsDLInformationTransfer {  unsigned int field0;  struct l_union_OC_UInnerUErrcCriticalExtensionsDLInformationTransfer field1;};

struct l_struct_OC_SErrcDLInformationTransfer {  unsigned char field0;  unsigned char field1;  unsigned char field2;  unsigned char field3;  struct l_struct_OC_UErrcCriticalExtensionsDLInformationTransfer field4;};

struct l_struct_OC_SErrcULInformationTransfer {  struct l_struct_OC_UErrcCriticalExtensionsDLInformationTransfer field0;};

struct l_unnamed65 { struct l_struct_OC_SErrcN4TxAntennaTm5 array[4096]; };

struct l_struct_OC_SS1apForbiddenLACs {  unsigned short field0;  struct l_unnamed65 field1;};

struct l_struct_OC_SS1apForbiddenLAsItem {  struct l_struct_OC_SAmRlcPbTab field0;  struct l_struct_OC_SS1apForbiddenLACs field1;  unsigned short field2;};

struct l_unnamed64 { struct l_struct_OC_SS1apForbiddenLAsItem array[16]; };

struct l_struct_OC_SS1apForbiddenLAs {  unsigned char field0;  unsigned char field1;  unsigned char field2;  unsigned char field3;  struct l_unnamed64 field4;};

struct l_struct_OC_SS1apGUMMEI {  struct l_struct_OC_SAmRlcPbTab field0;  struct l_struct_OC_SErrcN4TxAntennaTm5 field1;  unsigned char field2;  unsigned char field3;};

struct l_struct_OC_SS1apHandoverRestrictionList {  struct l_struct_OC_SAmRlcPbTab field0;  unsigned int field1;  struct l_struct_OC_SS1apEPLMNs field2;  unsigned int field3;  struct l_struct_OC_SS1apForbiddenLAs field4;  unsigned int field5;  struct l_struct_OC_SS1apForbiddenLAs field6;  unsigned int field7;  unsigned int field8;};

struct l_struct_OC_SS1apTAI {  struct l_struct_OC_SAmRlcPbTab field0;  struct l_struct_OC_SErrcN4TxAntennaTm5 field1;  unsigned short field2;};

struct l_struct_OC_SS1apUFDownlinkNASTransport {  unsigned long long field0;  unsigned int field1;  struct l_struct_OC_SAsnDynstr field2;  unsigned int field3;  struct l_struct_OC_SS1apHandoverRestrictionList field4;};

struct l_struct_OC_SS1apUFInitialUEMessage {  unsigned int field0;  struct l_struct_OC_SAsnDynstr field1;  struct l_struct_OC_SS1apTAI field2;  struct l_struct_OC_SErrcPLMNIdentityInfo field3;  unsigned int field4;  unsigned int field5;  struct l_struct_OC_SSTMsi field6;  unsigned int field7;  struct l_struct_OC_SErrcCsgIdentity field8;  unsigned int field9;  struct l_struct_OC_SS1apGUMMEI field10;};

struct l_struct_OC_SS1apUFNASNonDeliveryIndication {  unsigned long long field0;  unsigned int field1;  struct l_struct_OC_SAsnDynstr field2;  struct l_struct_OC_US1apCause field3;  unsigned int field4;};

struct l_struct_OC_SS1apUFUplinkNASTransport {  unsigned long long field0;  unsigned int field1;  struct l_struct_OC_SAsnDynstr field2;  struct l_struct_OC_SErrcPLMNIdentityInfo field3;  struct l_struct_OC_SS1apTAI field4;  unsigned int field5;};

struct l_struct_OC_TUP_L3MessageInd {  unsigned int field0;  unsigned int field1;  unsigned int field2;  unsigned int field3;  unsigned long long field4;  unsigned int field5;  struct l_unnamed85 field6;  unsigned char field7;  unsigned char field8;  unsigned char field9;};

struct l_struct_OC_TUP_L3MessageReq {  unsigned int field0;  unsigned int field1;  unsigned int field2;  unsigned int field3;  unsigned int field4;  unsigned int field5;  unsigned long long field6;  unsigned int field7;  struct l_unnamed85 field8;  unsigned char field9;  unsigned char field10;  unsigned char field11;};

struct l_struct_OC_TUP_SrbReceiveInd {  unsigned int field0;  unsigned int field1;  unsigned int field2;  unsigned int field3;  unsigned int field4;  unsigned int field5;  unsigned int field6;  struct l_unnamed85 field7;  unsigned char field8;  unsigned char field9;  unsigned char field10;};

struct l_struct_OC_TUP_SrbSendReq {  unsigned int field0;  unsigned int field1;  unsigned int field2;  unsigned int field3;  unsigned int field4;  unsigned int field5;  unsigned int field6;  unsigned int field7;  unsigned int field8;  unsigned int field9;  unsigned int field10;  unsigned int field11;  struct l_unnamed85 field12;  unsigned char field13;  unsigned char field14;  unsigned char field15;};

struct l_struct_OC_TUP_SrbSendResp {  struct l_struct_OC_SAmRlcPbTab field0;  unsigned int field1;  unsigned int field2;  unsigned int field3;  unsigned int field4;  unsigned int field5;  unsigned int field6;  unsigned int field7;  struct l_unnamed85 field8;  unsigned char field9;  unsigned char field10;  unsigned char field11;};

struct l_struct_OC_Uec_KD__KD_UEC_DTF_DirectTransfer {  unsigned int field0;  unsigned char field1;  struct l_struct_OC_SAsnDynstr field2;};

struct l_struct_OC_std_KD__KD__Rb_tree_iterator {  struct l_struct_OC_std_KD__KD__Rb_tree_node_base *field0;};

struct l_unnamed78 {  unsigned int field0;  struct l_struct_OC_SAsnDynstr field1;};

struct l_struct_OC_std_KD__KD__Rb_tree_node {  struct l_unnamed61 field0;  struct l_unnamed78 field1;};

struct l_struct_OC_std_KD__KD_basic_string_MD_char_MC__AC_std_KD__KD_char_traits_MD_char_OD__MC__AC_std_KD__KD_allocator_MD_char_OD__AC__OD__KD__KD__Rep_base {  unsigned long long field0;  unsigned long long field1;  unsigned int field2;};

struct l_struct_OC_std_KD__KD_ios_base_KD__KD__Callback_list {  struct l_struct_OC_std_KD__KD_ios_base_KD__KD__Callback_list *field0;  void  (*field1) (unsigned int , struct l_class_OC_std_KD__KD_ios_base *, unsigned int );  unsigned int field2;  unsigned int field3;};

struct l_struct_OC_std_KD__KD_pair {  struct l_struct_OC_std_KD__KD__Rb_tree_iterator field0;  unsigned char field1;};

struct l_unnamed0 { unsigned char array[13]; };

struct l_unnamed1 { unsigned char array[20]; };

struct l_unnamed10 { unsigned char array[84]; };

struct l_unnamed11 { unsigned char array[22]; };

struct l_unnamed13 { unsigned char array[58]; };

struct l_unnamed14 { unsigned char array[29]; };

struct l_unnamed15 { unsigned char array[78]; };

struct l_unnamed16 { unsigned char array[37]; };

struct l_unnamed17 { unsigned char array[44]; };

struct l_unnamed2 { unsigned char array[36]; };

struct l_unnamed20 { unsigned char array[27]; };

struct l_unnamed3 { unsigned char array[116]; };

struct l_unnamed31 { unsigned char array[74]; };

struct l_unnamed32 { unsigned char array[17]; };

struct l_unnamed33 { unsigned char array[66]; };

struct l_unnamed34 { unsigned char array[101]; };

struct l_unnamed35 { unsigned char array[123]; };

struct l_unnamed36 { unsigned char array[21]; };

struct l_unnamed37 { unsigned char array[111]; };

struct l_unnamed38 { unsigned char array[79]; };

struct l_unnamed39 { unsigned char array[85]; };

struct l_unnamed4 { unsigned char array[5]; };

struct l_unnamed40 { unsigned char array[65]; };

struct l_unnamed41 { unsigned char array[39]; };

struct l_unnamed42 { unsigned char array[59]; };

struct l_unnamed43 { unsigned char *array[5]; };

struct l_unnamed44 { unsigned char array[135]; };

struct l_unnamed45 { unsigned char array[30]; };

struct l_unnamed46 { unsigned char array[89]; };

struct l_unnamed47 { unsigned char *array[8]; };

struct l_unnamed49 {  unsigned int field0;  void  (*field1) (void);};

struct l_unnamed48 { struct l_unnamed49 array[1]; };

struct l_unnamed5 { unsigned char array[49]; };

struct l_unnamed51 { unsigned char array[53]; };

struct l_unnamed52 { unsigned char array[11]; };

struct l_unnamed53 { unsigned char array[104]; };

struct l_unnamed55 { unsigned char array[23]; };

struct l_unnamed56 { unsigned char array[38]; };

struct l_unnamed57 { unsigned char array[25]; };

struct l_unnamed58 { unsigned char array[48]; };

struct l_unnamed6 { unsigned char *array[17]; };

struct l_unnamed62 { unsigned char array[33]; };

struct l_unnamed63 { unsigned char array[114]; };

struct l_unnamed66 { unsigned char array[86]; };

struct l_unnamed67 { unsigned char array[42]; };

struct l_unnamed68 { unsigned char array[26]; };

struct l_unnamed69 { unsigned char array[41]; };

struct l_unnamed7 { unsigned char array[10]; };

struct l_unnamed70 { unsigned char array[19]; };

struct l_unnamed71 { unsigned char array[76]; };

struct l_unnamed75 { unsigned char array[82]; };

struct l_unnamed76 { unsigned char array[28]; };

struct l_unnamed77 { unsigned char array[18]; };

struct l_unnamed8 { unsigned char array[108]; };

struct l_unnamed80 { unsigned char array[45]; };

struct l_unnamed81 { unsigned char array[46]; };

struct l_unnamed84 { unsigned char array[47]; };

struct l_unnamed86 { unsigned char array[14]; };

struct l_unnamed87 { unsigned char array[15]; };

struct l_unnamed89 { unsigned long long array[1]; };

struct l_unnamed9 { unsigned char array[90]; };

struct l_unnamed93 { unsigned char array[43]; };


/* External Global Variable Declarations */
extern unsigned char *__dso_handle;
extern struct l_unnamed6 _ZTVN3Uec17UecDirectTransferE;
extern struct l_class_OC_BaseEvent mout;
extern struct l_unnamed89 _ZNSs4_Rep20_S_empty_rep_storageE;

/* Function Declarations */
double fmod(double, double);
float fmodf(float, float);
long double fmodl(long double, long double);
static void __cxx_global_var_init(void);
void _ZNSt8ios_base4InitC1Ev(struct l_class_OC_BaseEvent *);
void _ZNSt8ios_base4InitD1Ev(struct l_class_OC_BaseEvent *);
unsigned int __cxa_atexit(void  (*) (unsigned char *), unsigned char *, unsigned char *);
unsigned int _ZN6Common5Codec17CAsn1Codec_Helper9calcBytesEi(unsigned int );
static void __cxx_global_var_init1(void);
void _ZN3Uec17UecDirectTransferC2EPNS_14UecManagerBaseE(struct l_class_OC_Uec_KD__KD_UecDirectTransfer *llvm_cbe_this, struct l_class_OC_Uec_KD__KD_UecManagerBase *llvm_cbe_p_uecManagerBase_ptr);
void _ZN3Uec14UecServiceBaseC2EPNS_14UecManagerBaseENS_13EUecServiceIdEPKc(struct l_class_OC_Uec_KD__KD_UecServiceBase *, struct l_class_OC_Uec_KD__KD_UecManagerBase *, unsigned int , unsigned char *);
unsigned int __gxx_personality_v0(int vararg_dummy_arg,...);
void _ZSt9terminatev(void);
void _ZN3Uec14UecServiceBaseD2Ev(struct l_class_OC_Uec_KD__KD_UecServiceBase *);
void _Unwind_Resume_or_Rethrow(unsigned char *);
unsigned char *_Znwm(unsigned long long );
void _ZN7CStateTIN3Uec17UecDirectTransferEEC1EPS1_MS1_FjP6CEventESs(struct l_class_OC_CStateT *llvm_cbe_this, struct l_class_OC_Uec_KD__KD_UecDirectTransfer *llvm_cbe_itsFsmPtr, struct l_struct_OC_SS1apUEAggregateMaximumBitrate , struct l_class_OC_std_KD__KD_basic_string *llvm_cbe_name) __ATTRIBUTE_WEAK__;
unsigned int _ZN3Uec17UecDirectTransfer9StateIdleEP6CEvent(struct l_class_OC_Uec_KD__KD_UecDirectTransfer *llvm_cbe_this, struct l_class_OC_CEvent *llvm_cbe_p_cEvent_ptr);
void _ZNSsC1EPKcRKSaIcE(struct l_class_OC_std_KD__KD_basic_string *, unsigned char *, struct l_class_OC_BaseEvent *);
void _ZNSaIcEC1Ev(struct l_class_OC_BaseEvent *llvm_cbe_this);
void _ZNSaIcED1Ev(struct l_class_OC_BaseEvent *llvm_cbe_this);
void _ZNSsD1Ev(struct l_class_OC_std_KD__KD_basic_string *llvm_cbe_this);
void _ZN6Common8CFsmBase15SetInitialStateEP10CStateBase(struct l_class_OC_Common_KD__KD_CFsmBase *, struct l_class_OC_CStateBase *);
struct l_class_OC_Uec_KD__KD_UecManagerBase *_ZNK3Uec14UecServiceBase13getUecManagerEv(struct l_class_OC_Uec_KD__KD_UecServiceBase *llvm_cbe_this) __ATTRIBUTE_WEAK__;
void _ZN3Uec14UecManagerBase17registerBehaviourEijNS_13EUecServiceIdENS_12EUecActionIdES1_(struct l_class_OC_Uec_KD__KD_UecManagerBase *, unsigned int , unsigned int , unsigned int , unsigned int , unsigned int );
void _ZThn8_N3Uec17UecDirectTransferD0Ev(struct l_class_OC_Uec_KD__KD_UecDirectTransfer *llvm_cbe_this);
void _ZN3Uec17UecDirectTransferD0Ev(struct l_class_OC_Uec_KD__KD_UecDirectTransfer *llvm_cbe_this);
void _ZdlPv(unsigned char *);
void _ZThn8_N3Uec17UecDirectTransferD1Ev(struct l_class_OC_Uec_KD__KD_UecDirectTransfer *llvm_cbe_this);
void _ZN3Uec17UecDirectTransferD2Ev(struct l_class_OC_Uec_KD__KD_UecDirectTransfer *llvm_cbe_this);
unsigned int _ZN3Uec17UecDirectTransfer7doResetEv(struct l_class_OC_Uec_KD__KD_UecDirectTransfer *llvm_cbe_this);
void _ZdaPv(unsigned char *);
void _ZN3Uec14UecServiceBase19setProcessingStatusEib(struct l_class_OC_Uec_KD__KD_UecServiceBase *llvm_cbe_this, unsigned int llvm_cbe_p_state, bool llvm_cbe_isForcedMode) __ATTRIBUTE_WEAK__;
unsigned int _ZNK3Uec17UecDirectTransfer30handleRrcDlInformationTransferEPNS_8UecEventE(struct l_class_OC_Uec_KD__KD_UecDirectTransfer *llvm_cbe_this, struct l_class_OC_Uec_KD__KD_UecEvent *llvm_cbe_p_uecEvent_ptr);
void _ZN3Uec17UecDebugCallstackC1ERKSs(struct l_class_OC_Uec_KD__KD_UecDebugCallstack *, struct l_class_OC_std_KD__KD_basic_string *);
void _ZN3Uec17UecDebugCallstackD1Ev(struct l_class_OC_Uec_KD__KD_UecDebugCallstack *);
unsigned char *_ZNK3Uec8UecEvent14getAsn1PayLoadEv(struct l_class_OC_Uec_KD__KD_UecEvent *llvm_cbe_this) __ATTRIBUTE_WEAK__;
struct l_class_OC_BaseEvent *_ZN11DummyStreamlsIPKcEERS_T_(struct l_class_OC_BaseEvent *llvm_cbe_this, unsigned char *llvm_cbe_arg) __ATTRIBUTE_WEAK__;
struct l_class_OC_BaseEvent *_ZN11DummyStreamlsIPFRSt8ios_baseS2_EEERS_T_(struct l_class_OC_BaseEvent *llvm_cbe_this, struct l_class_OC_std_KD__KD_ios_base * (*llvm_cbe_arg) (struct l_class_OC_std_KD__KD_ios_base *)) __ATTRIBUTE_WEAK__;
struct l_class_OC_std_KD__KD_ios_base *_ZSt3decRSt8ios_base(struct l_class_OC_std_KD__KD_ios_base *llvm_cbe___base) __ATTRIBUTE_WEAK__;
struct l_class_OC_BaseEvent *_ZN11DummyStreamlsIjEERS_T_(struct l_class_OC_BaseEvent *llvm_cbe_this, unsigned int llvm_cbe_arg) __ATTRIBUTE_WEAK__;
unsigned int _ZNK3Uec8UecEvent13getInstanceIdEv(struct l_class_OC_Uec_KD__KD_UecEvent *llvm_cbe_this) __ATTRIBUTE_WEAK__;
struct l_class_OC_BaseEvent *_ZN11DummyStreamlsI49EDiscUErrcCriticalExtensionsDLInformationTransferEERS_T_(struct l_class_OC_BaseEvent *llvm_cbe_this, unsigned int llvm_cbe_arg) __ATTRIBUTE_WEAK__;
struct l_class_OC_BaseEvent *_ZN11DummyStreamlsI51EDiscUErrcC1CriticalExtensionsDLInformationTransferEERS_T_(struct l_class_OC_BaseEvent *llvm_cbe_this, unsigned int llvm_cbe_arg) __ATTRIBUTE_WEAK__;
struct l_class_OC_BaseEvent *_ZN11DummyStreamlsI27EDiscUErrcDedicatedInfoTypeEERS_T_(struct l_class_OC_BaseEvent *llvm_cbe_this, unsigned int llvm_cbe_arg) __ATTRIBUTE_WEAK__;
struct l_class_OC_std_KD__KD_ios_base *_ZSt3hexRSt8ios_base(struct l_class_OC_std_KD__KD_ios_base *llvm_cbe___base) __ATTRIBUTE_WEAK__;
unsigned int _ZNK6CEvent10GetEventIdEv(struct l_class_OC_CEvent *llvm_cbe_this) __ATTRIBUTE_WEAK__;
struct l_class_OC_BaseEvent *_ZN11DummyStreamlsIPhEERS_T_(struct l_class_OC_BaseEvent *llvm_cbe_this, unsigned char *llvm_cbe_arg) __ATTRIBUTE_WEAK__;
unsigned int _ZN3Uec17UecDirectTransfer30handleRrcUlInformationTransferEPNS_8UecEventE(struct l_class_OC_Uec_KD__KD_UecDirectTransfer *llvm_cbe_this, struct l_class_OC_Uec_KD__KD_UecEvent *llvm_cbe_p_uecEvent_ptr);
struct l_class_OC_BaseEvent *_ZN11DummyStreamlsI49EDiscUErrcCriticalExtensionsULInformationTransferEERS_T_(struct l_class_OC_BaseEvent *llvm_cbe_this, unsigned int llvm_cbe_arg) __ATTRIBUTE_WEAK__;
struct l_class_OC_BaseEvent *_ZN11DummyStreamlsI51EDiscUErrcC1CriticalExtensionsULInformationTransferEERS_T_(struct l_class_OC_BaseEvent *llvm_cbe_this, unsigned int llvm_cbe_arg) __ATTRIBUTE_WEAK__;
unsigned int _ZNK3Uec14UecManagerBase13getInstanceIdEv(struct l_class_OC_Uec_KD__KD_UecManagerBase *llvm_cbe_this) __ATTRIBUTE_WEAK__;
struct l_class_OC_BaseEvent *_ZN11DummyStreamlsI34EDiscUErrcDedicatedInformationTypeEERS_T_(struct l_class_OC_BaseEvent *llvm_cbe_this, unsigned int llvm_cbe_arg) __ATTRIBUTE_WEAK__;
unsigned int _ZN3Uec17UecDirectTransfer24handleS1ApDlNasTransportEPNS_8UecEventE(struct l_class_OC_Uec_KD__KD_UecDirectTransfer *llvm_cbe_this, struct l_class_OC_Uec_KD__KD_UecEvent *llvm_cbe_p_uecEvent_ptr);
struct l_class_OC_Uec_KD__KD_UecBaseData *_ZNK3Uec14UecServiceBase16getUecDataAccessEv(struct l_class_OC_Uec_KD__KD_UecServiceBase *llvm_cbe_this) __ATTRIBUTE_WEAK__;
bool _ZNK3Uec16UecUeContextData16isValidMmeUeS1IdEv(struct l_class_OC_Uec_KD__KD_UecUeContextData *llvm_cbe_this) __ATTRIBUTE_WEAK__;
void _ZN3Uec16UecUeContextData12setMmeUeS1IdEj(struct l_class_OC_Uec_KD__KD_UecUeContextData *llvm_cbe_this, unsigned int llvm_cbe_p_mmeUeS1Id) __ATTRIBUTE_WEAK__;
struct l_class_OC_BaseEvent *_ZN11DummyStreamlsIiEERS_T_(struct l_class_OC_BaseEvent *llvm_cbe_this, unsigned int llvm_cbe_arg) __ATTRIBUTE_WEAK__;
struct l_class_OC_BaseEvent *_ZN11DummyStreamlsIxEERS_T_(struct l_class_OC_BaseEvent *llvm_cbe_this, unsigned long long llvm_cbe_arg) __ATTRIBUTE_WEAK__;
unsigned int _ZNK3Uec16UecUeContextData24getUeAssS1SigConnTimerIdEv(struct l_class_OC_Uec_KD__KD_UecUeContextData *llvm_cbe_this) __ATTRIBUTE_WEAK__;
struct l_class_OC_Uec_KD__KD_UecBaseData *_ZNK3Uec14UecServiceBase23getUecServiceControllerEv(struct l_class_OC_Uec_KD__KD_UecServiceBase *llvm_cbe_this) __ATTRIBUTE_WEAK__;
unsigned int _ZN3Uec11UecTimePort9stopTimerEj(struct l_class_OC_Uec_KD__KD_UecTimePort *, unsigned int );
void _ZN3Uec16UecUeContextData24setUeAssS1SigConnTimerIdEj(struct l_class_OC_Uec_KD__KD_UecUeContextData *llvm_cbe_this, unsigned int llvm_cbe_p_timerId) __ATTRIBUTE_WEAK__;
unsigned int _ZNK3Uec16UecUeContextData12getEnbUeS1IdEv(struct l_class_OC_Uec_KD__KD_UecUeContextData *llvm_cbe_this) __ATTRIBUTE_WEAK__;
unsigned int _ZNK3Uec16UecUeContextData12getMmeUeS1IdEv(struct l_class_OC_Uec_KD__KD_UecUeContextData *llvm_cbe_this) __ATTRIBUTE_WEAK__;
unsigned int _ZN3Uec17UecDirectTransfer31handleEmbeddedHoRestrictionListEPK27SS1apUFDownlinkNASTransportPNS_16UecUeContextDataE(struct l_class_OC_Uec_KD__KD_UecDirectTransfer *llvm_cbe_this, struct l_struct_OC_SS1apUFDownlinkNASTransport *llvm_cbe_p_asn1PayloadPtr, struct l_class_OC_Uec_KD__KD_UecUeContextData *llvm_cbe_p_uecUeContextDataPtr);
unsigned int _ZN3Uec17UecDirectTransfer28handleTupL3MessageIndicationEPNS_8UecEventE(struct l_class_OC_Uec_KD__KD_UecDirectTransfer *llvm_cbe_this, struct l_class_OC_Uec_KD__KD_UecEvent *llvm_cbe_p_uecEvent_ptr);
unsigned char *_ZNK3Uec8UecEvent10getPayLoadEv(struct l_class_OC_Uec_KD__KD_UecEvent *llvm_cbe_this) __ATTRIBUTE_WEAK__;
unsigned int _ZNK3Uec16UecUeContextData11getS1LinkIdEv(struct l_class_OC_Uec_KD__KD_UecUeContextData *llvm_cbe_this) __ATTRIBUTE_WEAK__;
unsigned int _ZN3Uec17UecDirectTransfer22handleTupSrbReceiveIndEPNS_8UecEventE(struct l_class_OC_Uec_KD__KD_UecDirectTransfer *llvm_cbe_this, struct l_class_OC_Uec_KD__KD_UecEvent *llvm_cbe_p_uecEvent_ptr);
unsigned int _ZNK3Uec16UecUeContextData9getCellIdEv(struct l_class_OC_Uec_KD__KD_UecUeContextData *llvm_cbe_this) __ATTRIBUTE_WEAK__;
unsigned int _ZNK3Uec16UecUeContextData15getUplaneCellIdEv(struct l_class_OC_Uec_KD__KD_UecUeContextData *llvm_cbe_this) __ATTRIBUTE_WEAK__;
struct l_class_OC_BaseEvent *_ZN11DummyStreamlsI21EIntegrityCheckResultEERS_T_(struct l_class_OC_BaseEvent *llvm_cbe_this, unsigned int llvm_cbe_arg) __ATTRIBUTE_WEAK__;
unsigned int _ZN3Uec17UecDirectTransfer20handleTupSrbSendRespEPNS_8UecEventE(struct l_class_OC_Uec_KD__KD_UecDirectTransfer *llvm_cbe_this, struct l_class_OC_Uec_KD__KD_UecEvent *llvm_cbe_p_uecEvent_ptr);
struct l_class_OC_BaseEvent *_ZN11DummyStreamlsI10EStatusLteEERS_T_(struct l_class_OC_BaseEvent *llvm_cbe_this, unsigned int llvm_cbe_arg) __ATTRIBUTE_WEAK__;
struct l_class_OC_BaseEvent *_ZN11DummyStreamlsI9ECauseLteEERS_T_(struct l_class_OC_BaseEvent *llvm_cbe_this, unsigned int llvm_cbe_arg) __ATTRIBUTE_WEAK__;
struct l_class_OC_BaseEvent *_ZN11DummyStreamlsI17ESpecificCauseLteEERS_T_(struct l_class_OC_BaseEvent *llvm_cbe_this, unsigned int llvm_cbe_arg) __ATTRIBUTE_WEAK__;
unsigned int _ZNK3Uec17UecDirectTransfer26handleUecDtfDirectTransferEPNS_8UecEventE(struct l_class_OC_Uec_KD__KD_UecDirectTransfer *llvm_cbe_this, struct l_class_OC_Uec_KD__KD_UecEvent *llvm_cbe_p_uecEvent_ptr);
unsigned int _ZN3Uec17UecDirectTransfer28sendRrcDlInformationTransferEPNS_8UecEventE(struct l_class_OC_Uec_KD__KD_UecDirectTransfer *llvm_cbe_this, struct l_class_OC_Uec_KD__KD_UecEvent *llvm_cbe_p_uecEvent_ptr);
unsigned int _ZN3Uec17UecDirectTransfer20setTupSrbSendRequestER14TUP_SrbSendReqj(struct l_class_OC_Uec_KD__KD_UecDirectTransfer *llvm_cbe_this, struct l_struct_OC_TUP_SrbSendReq *llvm_cbe_p_payLoad, unsigned int llvm_cbe_p_respFlag);
unsigned int _ZN3Uec17UecDirectTransfer27setRrcDlInformationTransferEPNS_8UecEventER26SErrcDLInformationTransfer(struct l_class_OC_Uec_KD__KD_UecDirectTransfer *llvm_cbe_this, struct l_class_OC_Uec_KD__KD_UecEvent *llvm_cbe_p_uecEvent_ptr, struct l_struct_OC_SErrcDLInformationTransfer *llvm_cbe_p_asn1PayLoad);
void _ZN3Uec8UecEventC1EjNS_15EUecServiceTypeEjPvjiS2_j(struct l_class_OC_Uec_KD__KD_UecEvent *llvm_cbe_this, unsigned int llvm_cbe_messageId, unsigned int llvm_cbe_serviceType, unsigned int llvm_cbe_instanceId, unsigned char *llvm_cbe_payLoadPtr, unsigned int llvm_cbe_payloadSize, unsigned int llvm_cbe_asn1PayLoadId, unsigned char *llvm_cbe_asn1PayLoadPtr, unsigned int llvm_cbe_asn1PayloadSize) __ATTRIBUTE_WEAK__;
void _ZN3Uec8UecEventD1Ev(struct l_class_OC_Uec_KD__KD_UecEvent *llvm_cbe_this) __ATTRIBUTE_WEAK__;
unsigned int _ZN3Uec17UecDirectTransfer24sendS1ApInitialUeMessageEPNS_8UecEventE(struct l_class_OC_Uec_KD__KD_UecDirectTransfer *llvm_cbe_this, struct l_class_OC_Uec_KD__KD_UecEvent *llvm_cbe_p_uecEvent_ptr);
unsigned int _ZN3Uec17UecDirectTransfer22setTupL3MessageRequestER16TUP_L3MessageReq(struct l_class_OC_Uec_KD__KD_UecDirectTransfer *llvm_cbe_this, struct l_struct_OC_TUP_L3MessageReq *llvm_cbe_p_payLoad);
unsigned int _ZN3Uec17UecDirectTransfer23setS1ApInitialUeMessageEPNS_8UecEventER23SS1apUFInitialUEMessage(struct l_class_OC_Uec_KD__KD_UecDirectTransfer *llvm_cbe_this, struct l_class_OC_Uec_KD__KD_UecEvent *llvm_cbe_p_uecEvent_ptr, struct l_struct_OC_SS1apUFInitialUEMessage *llvm_cbe_p_asn1PayLoad);
unsigned int _ZN3Uec17UecDirectTransfer32sendS1ApNasNonDeliveryIndicationEPNS_8UecEventE(struct l_class_OC_Uec_KD__KD_UecDirectTransfer *llvm_cbe_this, struct l_class_OC_Uec_KD__KD_UecEvent *llvm_cbe_p_uecEvent_ptr);
struct l_class_OC_BaseEvent *_ZN11DummyStreamlsIbEERS_T_(struct l_class_OC_BaseEvent *llvm_cbe_this, bool llvm_cbe_arg) __ATTRIBUTE_WEAK__;
unsigned int _ZN3Uec17UecDirectTransfer31setS1ApNasNonDeliveryIndicationEPNS_8UecEventER31SS1apUFNASNonDeliveryIndication(struct l_class_OC_Uec_KD__KD_UecDirectTransfer *llvm_cbe_this, struct l_class_OC_Uec_KD__KD_UecEvent *llvm_cbe_p_uecEvent_ptr, struct l_struct_OC_SS1apUFNASNonDeliveryIndication *llvm_cbe_p_asn1PayLoad);
unsigned int _ZN3Uec17UecDirectTransfer22sendS1ApUlNasTransportEPNS_8UecEventE(struct l_class_OC_Uec_KD__KD_UecDirectTransfer *llvm_cbe_this, struct l_class_OC_Uec_KD__KD_UecEvent *llvm_cbe_p_uecEvent_ptr);
void _ZN3Uec16UecUeContextData12insertNasPduERK10SAsnDynstr(struct l_class_OC_Uec_KD__KD_UecUeContextData *llvm_cbe_this, struct l_struct_OC_SAsnDynstr *llvm_cbe_p_nasPdu) __ATTRIBUTE_WEAK__;
unsigned int _ZN3Uec17UecDirectTransfer21setS1ApUlNasTransportEPNS_8UecEventER25SS1apUFUplinkNASTransport(struct l_class_OC_Uec_KD__KD_UecDirectTransfer *llvm_cbe_this, struct l_class_OC_Uec_KD__KD_UecEvent *llvm_cbe_p_uecEvent_ptr, struct l_struct_OC_SS1apUFUplinkNASTransport *llvm_cbe_p_asn1PayLoad);
bool _ZN3Uec16UecUeContextData14isMmeConnectedEv(struct l_class_OC_Uec_KD__KD_UecUeContextData *llvm_cbe_this) __ATTRIBUTE_WEAK__;
unsigned int _ZNK3Uec8UecEvent16getAsn1PayLoadIdEv(struct l_class_OC_Uec_KD__KD_UecEvent *llvm_cbe_this) __ATTRIBUTE_WEAK__;
unsigned char *_Znam(unsigned long long );
unsigned int _ZNK3Uec11UecCellData6getTacEv(struct l_class_OC_Uec_KD__KD_UecCellData *llvm_cbe_this) __ATTRIBUTE_WEAK__;
struct l_struct_OC_SS1apUEAggregateMaximumBitrate _ZNK3Uec16UecUeContextData11getS1GummeiEv(struct l_class_OC_Uec_KD__KD_UecUeContextData *llvm_cbe_this) __ATTRIBUTE_WEAK__;
unsigned int _ZNK3Uec11UecCellData9getCellIdEv(struct l_class_OC_Uec_KD__KD_UecCellData *llvm_cbe_this) __ATTRIBUTE_WEAK__;
struct l_struct_OC_SAmRlcPbTab *_ZNK3Uec11UecCellData12getPlmnRestLEj(struct l_class_OC_Uec_KD__KD_UecCellData *llvm_cbe_this, unsigned int llvm_cbe_i) __ATTRIBUTE_WEAK__;
bool _ZNK3Uec16UecUeContextData24isValidInitialUeIdentityEv(struct l_class_OC_Uec_KD__KD_UecUeContextData *llvm_cbe_this) __ATTRIBUTE_WEAK__;
struct l_struct_OC_UErrcInitialUEIdentity *_ZNK3Uec16UecUeContextData20getInitialUeIdentityEv(struct l_class_OC_Uec_KD__KD_UecUeContextData *llvm_cbe_this) __ATTRIBUTE_WEAK__;
unsigned int _ZN3Uec12UecParamConv26convErrcEstablishmentCauseE23EErrcEstablishmentCause(unsigned int llvm_cbe_p_enum) __ATTRIBUTE_WEAK__;
unsigned int *_ZNK3Uec16UecUeContextData21getEstablishmentCauseEv(struct l_class_OC_Uec_KD__KD_UecUeContextData *llvm_cbe_this) __ATTRIBUTE_WEAK__;
unsigned int _ZNK3Uec16UecUeContextData14getNasDeliveryEv(struct l_class_OC_Uec_KD__KD_UecUeContextData *llvm_cbe_this) __ATTRIBUTE_WEAK__;
unsigned int _ZNK3Uec16UecUeContextData8getCrntiEv(struct l_class_OC_Uec_KD__KD_UecUeContextData *llvm_cbe_this) __ATTRIBUTE_WEAK__;
bool _ZN3Uec16UecUeContextData10isSbActiveEj(struct l_class_OC_Uec_KD__KD_UecUeContextData *llvm_cbe_this, unsigned int llvm_cbe_i) __ATTRIBUTE_WEAK__;
unsigned int _ZN3Uec17UecDirectTransfer11storeNasPduEPNS_8UecEventE(struct l_class_OC_Uec_KD__KD_UecDirectTransfer *llvm_cbe_this, struct l_class_OC_Uec_KD__KD_UecEvent *llvm_cbe_p_uecEvent_ptr);
struct l_class_OC_BaseEvent *_ZN11DummyStreamlsIhEERS_T_(struct l_class_OC_BaseEvent *llvm_cbe_this, unsigned char llvm_cbe_arg) __ATTRIBUTE_WEAK__;
void _ZN3Uec16UecUeContextData26setHandoverRestrictionListERK28SS1apHandoverRestrictionList(struct l_class_OC_Uec_KD__KD_UecUeContextData *llvm_cbe_this, struct l_struct_OC_SS1apHandoverRestrictionList *llvm_cbe_p_handoverRestrictionList) __ATTRIBUTE_WEAK__;
void _ZN3Uec14UecServiceBase15serviceFinishedEiNS_13EUecServiceIdE(struct l_class_OC_Uec_KD__KD_UecServiceBase *, unsigned int , unsigned int );
struct l_class_OC_BaseEvent *_ZN11DummyStreamlsIPvEERS_T_(struct l_class_OC_BaseEvent *llvm_cbe_this, unsigned char *llvm_cbe_arg) __ATTRIBUTE_WEAK__;
unsigned int _ZNK3Uec16UecUeContextData11getLockStepEv(struct l_class_OC_Uec_KD__KD_UecUeContextData *llvm_cbe_this) __ATTRIBUTE_WEAK__;
void _ZN3Uec27UEC_CTY_TupuProceedLockStepC1Ev(struct l_struct_OC_SAntennaInfo *llvm_cbe_this) __ATTRIBUTE_WEAK__;
struct l_class_OC_BaseEvent *_ZN11DummyStreamlsIN3Uec15EUecNasDeliveryEEERS_T_(struct l_class_OC_BaseEvent *llvm_cbe_this, unsigned int llvm_cbe_arg) __ATTRIBUTE_WEAK__;
struct l_class_OC_BaseEvent *_ZN11DummyStreamlsIN3Uec13EUecDirectionEEERS_T_(struct l_class_OC_BaseEvent *llvm_cbe_this, unsigned int llvm_cbe_arg) __ATTRIBUTE_WEAK__;
unsigned int _ZN3Uec14UecServiceBase12processEventEPNS_8UecEventEPNS_18UecServicesEventIfE(struct l_class_OC_Uec_KD__KD_UecServiceBase *, struct l_class_OC_Uec_KD__KD_UecEvent *, struct l_class_OC_Uec_KD__KD_UecBaseData *);
unsigned int _ZN3Uec14UecServiceBase12processEventEPNS_8UecEventE(struct l_class_OC_Uec_KD__KD_UecServiceBase *, struct l_class_OC_Uec_KD__KD_UecEvent *);
void _ZN3Uec14UecServiceBase6notifyENS_13EUecServiceIdENS_15EUecServiceTypeEjiS1_(struct l_class_OC_Uec_KD__KD_UecServiceBase *, unsigned int , unsigned int , unsigned int , unsigned int , unsigned int );
unsigned int _ZN3Uec14UecServiceBase12resetServiceEv(struct l_class_OC_Uec_KD__KD_UecServiceBase *);
unsigned int _ZN3Uec14UecServiceBase8rollBackEv(struct l_class_OC_Uec_KD__KD_UecServiceBase *);
unsigned int _ZN3Uec14UecServiceBase12getServiceIdEv(struct l_class_OC_Uec_KD__KD_UecServiceBase *llvm_cbe_this) __ATTRIBUTE_WEAK__;
void _ZN3Uec10UecFsmBase7InitFSMEv(struct l_class_OC_Uec_KD__KD_UecFsmBase *llvm_cbe_this) __ATTRIBUTE_WEAK__;
void _ZN3Uec10UecFsmBase8StartFSMEv(struct l_class_OC_Uec_KD__KD_UecFsmBase *llvm_cbe_this) __ATTRIBUTE_WEAK__;
void _ZN7CStateTIN3Uec17UecDirectTransferEEC2EPS1_MS1_FjP6CEventESs(struct l_class_OC_CStateT *llvm_cbe_this, struct l_class_OC_Uec_KD__KD_UecDirectTransfer *llvm_cbe_itsFsmPtr, struct l_struct_OC_SS1apUEAggregateMaximumBitrate , struct l_class_OC_std_KD__KD_basic_string *llvm_cbe_name) __ATTRIBUTE_WEAK__;
void _ZN10CStateBaseC2ESs(struct l_class_OC_CStateBase *llvm_cbe_this, struct l_class_OC_std_KD__KD_basic_string *llvm_cbe_name) __ATTRIBUTE_WEAK__;
void _ZNSsC1ERKSs(struct l_class_OC_std_KD__KD_basic_string *, struct l_class_OC_std_KD__KD_basic_string *);
void _ZN10CStateBaseD2Ev(struct l_class_OC_CStateBase *llvm_cbe_this) __ATTRIBUTE_WEAK__;
void _ZN7CStateTIN3Uec17UecDirectTransferEED1Ev(struct l_class_OC_CStateT *llvm_cbe_this) __ATTRIBUTE_WEAK__;
void _ZN7CStateTIN3Uec17UecDirectTransferEED0Ev(struct l_class_OC_CStateT *llvm_cbe_this) __ATTRIBUTE_WEAK__;
unsigned int _ZN7CStateTIN3Uec17UecDirectTransferEE11HandleEventEP6CEvent(struct l_class_OC_CStateT *llvm_cbe_this, struct l_class_OC_CEvent *llvm_cbe_pEvent) __ATTRIBUTE_WEAK__;
void _ZN7CStateTIN3Uec17UecDirectTransferEE14InitTransitionEv(struct l_class_OC_CStateT *llvm_cbe_this) __ATTRIBUTE_WEAK__;
void _ZN7CStateTIN3Uec17UecDirectTransferEE11EntryActionEv(struct l_class_OC_CStateT *llvm_cbe_this) __ATTRIBUTE_WEAK__;
void _ZN7CStateTIN3Uec17UecDirectTransferEE10ExitActionEv(struct l_class_OC_CStateT *llvm_cbe_this) __ATTRIBUTE_WEAK__;
void _ZN6Common8CFsmBase15StateTransitionEP10CStateBase(struct l_class_OC_Common_KD__KD_CFsmBase *, struct l_class_OC_CStateBase *);
void _ZN7CStateTIN3Uec17UecDirectTransferEED2Ev(struct l_class_OC_CStateT *llvm_cbe_this) __ATTRIBUTE_WEAK__;
void _ZN10CStateBaseD1Ev(struct l_class_OC_CStateBase *llvm_cbe_this) __ATTRIBUTE_WEAK__;
void _ZN10CStateBaseD0Ev(struct l_class_OC_CStateBase *llvm_cbe_this) __ATTRIBUTE_WEAK__;
void __cxa_pure_virtual(void);
void _ZNSsC1Ev(struct l_class_OC_std_KD__KD_basic_string *llvm_cbe_this);
struct l_class_OC_std_KD__KD_basic_string *_ZNSsaSERKSs(struct l_class_OC_std_KD__KD_basic_string *llvm_cbe_this, struct l_class_OC_std_KD__KD_basic_string *llvm_cbe___str);
struct l_class_OC_std_KD__KD_basic_string *_ZNSs6assignERKSs(struct l_class_OC_std_KD__KD_basic_string *, struct l_class_OC_std_KD__KD_basic_string *);
void _ZNSsC2Ev(struct l_class_OC_std_KD__KD_basic_string *llvm_cbe_this);
void _ZNSs12_Alloc_hiderC1EPcRKSaIcE(struct l_struct_OC_std_KD__KD_basic_string_MD_char_MC__AC_std_KD__KD_char_traits_MD_char_OD__MC__AC_std_KD__KD_allocator_MD_char_OD__AC__OD__KD__KD__Alloc_hider *llvm_cbe_this, unsigned char *llvm_cbe___dat, struct l_class_OC_BaseEvent *llvm_cbe___a);
struct l_class_OC_std_KD__KD_vector *_ZNSs12_S_empty_repEv(void);
unsigned char *_ZNSs4_Rep10_M_refdataEv(struct l_class_OC_std_KD__KD_vector *llvm_cbe_this);
void _ZNSs12_Alloc_hiderD1Ev(struct l_struct_OC_std_KD__KD_basic_string_MD_char_MC__AC_std_KD__KD_char_traits_MD_char_OD__MC__AC_std_KD__KD_allocator_MD_char_OD__AC__OD__KD__KD__Alloc_hider *llvm_cbe_this) __ATTRIBUTE_WEAK__;
void _ZNSs12_Alloc_hiderD2Ev(struct l_struct_OC_std_KD__KD_basic_string_MD_char_MC__AC_std_KD__KD_char_traits_MD_char_OD__MC__AC_std_KD__KD_allocator_MD_char_OD__AC__OD__KD__KD__Alloc_hider *llvm_cbe_this) __ATTRIBUTE_WEAK__;
void _ZNSaIcED2Ev(struct l_class_OC_BaseEvent *llvm_cbe_this);
void __cxa_call_unexpected(unsigned char *);
void _ZN9__gnu_cxx13new_allocatorIcED2Ev(struct l_class_OC_BaseEvent *llvm_cbe_this) __ATTRIBUTE_WEAK__;
struct l_class_OC_std_KD__KD_vector *_ZNSs4_Rep12_S_empty_repEv(void);
void _ZNSs12_Alloc_hiderC2EPcRKSaIcE(struct l_struct_OC_std_KD__KD_basic_string_MD_char_MC__AC_std_KD__KD_char_traits_MD_char_OD__MC__AC_std_KD__KD_allocator_MD_char_OD__AC__OD__KD__KD__Alloc_hider *llvm_cbe_this, unsigned char *llvm_cbe___dat, struct l_class_OC_BaseEvent *llvm_cbe___a);
void _ZNSaIcEC2ERKS_(struct l_class_OC_BaseEvent *llvm_cbe_this, struct l_class_OC_BaseEvent *llvm_cbe___a);
void _ZN9__gnu_cxx13new_allocatorIcEC2ERKS1_(struct l_class_OC_BaseEvent *llvm_cbe_this, struct l_class_OC_BaseEvent *) __ATTRIBUTE_WEAK__;
void _ZNSaIcEC2Ev(struct l_class_OC_BaseEvent *llvm_cbe_this);
void _ZN9__gnu_cxx13new_allocatorIcEC2Ev(struct l_class_OC_BaseEvent *llvm_cbe_this) __ATTRIBUTE_WEAK__;
void _ZNSsD2Ev(struct l_class_OC_std_KD__KD_basic_string *llvm_cbe_this);
struct l_class_OC_std_KD__KD_vector *_ZNKSs6_M_repEv(struct l_class_OC_std_KD__KD_basic_string *llvm_cbe_this);
void _ZNSs4_Rep10_M_disposeERKSaIcE(struct l_class_OC_std_KD__KD_vector *llvm_cbe_this, struct l_class_OC_BaseEvent *llvm_cbe___a);
struct l_class_OC_BaseEvent _ZNKSs13get_allocatorEv(struct l_class_OC_std_KD__KD_basic_string *llvm_cbe_this);
void _ZNSaIcEC1ERKS_(struct l_class_OC_BaseEvent *llvm_cbe_this, struct l_class_OC_BaseEvent *llvm_cbe___a);
static unsigned int _ZN9__gnu_cxx27__exchange_and_add_dispatchEPii(unsigned int *llvm_cbe___mem, unsigned int llvm_cbe___val);
void _ZNSs4_Rep10_M_destroyERKSaIcE(struct l_class_OC_std_KD__KD_vector *, struct l_class_OC_BaseEvent *);
static unsigned int _Z18__gthread_active_pv(void);
static unsigned int _ZN9__gnu_cxx18__exchange_and_addEPVii(unsigned int *llvm_cbe___mem, unsigned int llvm_cbe___val);
static unsigned int _ZN9__gnu_cxx25__exchange_and_add_singleEPii(unsigned int *llvm_cbe___mem, unsigned int llvm_cbe___val);
extern unsigned int pthread_cancel(unsigned long long ) __EXTERNAL_WEAK__;
unsigned char *_ZNKSs7_M_dataEv(struct l_class_OC_std_KD__KD_basic_string *llvm_cbe_this);
void _ZN3Uec27UEC_CTY_TupuProceedLockStepC2Ev(struct l_struct_OC_SAntennaInfo *llvm_cbe_this) __ATTRIBUTE_WEAK__;
void _ZN3Uec16UecUeContextData28clearHandoverRestrictionListEv(struct l_class_OC_Uec_KD__KD_UecUeContextData *llvm_cbe_this) __ATTRIBUTE_WEAK__;
struct l_class_OC_BaseEvent *_ZN11DummyStreamlsI23EErrcEstablishmentCauseEERS_T_(struct l_class_OC_BaseEvent *llvm_cbe_this, unsigned int llvm_cbe_arg) __ATTRIBUTE_WEAK__;
void _ZNSt4pairISt17_Rb_tree_iteratorIS_IKj10SAsnDynstrEEbEC1Ev(struct l_struct_OC_std_KD__KD_pair *llvm_cbe_this) __ATTRIBUTE_WEAK__;
struct l_struct_OC_SS1apUEAggregateMaximumBitrate _ZNSt3mapIj10SAsnDynstrSt4lessIjESaISt4pairIKjS0_EEE6insertERKS5_(struct l_class_OC_std_KD__KD_map *llvm_cbe_this, struct l_unnamed78 *llvm_cbe___x) __ATTRIBUTE_WEAK__;
void _ZNSt4pairIKj10SAsnDynstrEC1IjS1_EERKS_IT_T0_E(struct l_unnamed78 *llvm_cbe_this, struct l_unnamed78 *llvm_cbe___p) __ATTRIBUTE_WEAK__;
void _ZNSt4pairIj10SAsnDynstrEC1ERKjRKS0_(struct l_unnamed78 *llvm_cbe_this, unsigned int *llvm_cbe___a, struct l_struct_OC_SAsnDynstr *llvm_cbe___b) __ATTRIBUTE_WEAK__;
void _ZN3Uec9uecAssertEiPKcS1_z(unsigned int , unsigned char *, unsigned char *,...);
struct l_unnamed78 *_ZNKSt17_Rb_tree_iteratorISt4pairIKj10SAsnDynstrEEptEv(struct l_struct_OC_std_KD__KD__Rb_tree_iterator *llvm_cbe_this) __ATTRIBUTE_WEAK__;
void _ZNSt4pairIj10SAsnDynstrEC2ERKjRKS0_(struct l_unnamed78 *llvm_cbe_this, unsigned int *llvm_cbe___a, struct l_struct_OC_SAsnDynstr *llvm_cbe___b) __ATTRIBUTE_WEAK__;
void _ZNSt4pairIKj10SAsnDynstrEC2IjS1_EERKS_IT_T0_E(struct l_unnamed78 *llvm_cbe_this, struct l_unnamed78 *llvm_cbe___p) __ATTRIBUTE_WEAK__;
struct l_struct_OC_SS1apUEAggregateMaximumBitrate _ZNSt8_Rb_treeIjSt4pairIKj10SAsnDynstrESt10_Select1stIS3_ESt4lessIjESaIS3_EE16_M_insert_uniqueERKS3_(struct l_class_OC_std_KD__KD__Rb_tree *llvm_cbe_this, struct l_unnamed78 *llvm_cbe___x) __ATTRIBUTE_WEAK__;
struct l_struct_OC_std_KD__KD__Rb_tree_node *_ZNSt8_Rb_treeIjSt4pairIKj10SAsnDynstrESt10_Select1stIS3_ESt4lessIjESaIS3_EE8_M_beginEv(struct l_class_OC_std_KD__KD__Rb_tree *llvm_cbe_this) __ATTRIBUTE_WEAK__;
struct l_struct_OC_std_KD__KD__Rb_tree_node *_ZNSt8_Rb_treeIjSt4pairIKj10SAsnDynstrESt10_Select1stIS3_ESt4lessIjESaIS3_EE6_M_endEv(struct l_class_OC_std_KD__KD__Rb_tree *llvm_cbe_this) __ATTRIBUTE_WEAK__;
bool _ZNKSt4lessIjEclERKjS2_(struct l_class_OC_BaseEvent *llvm_cbe_this, unsigned int *llvm_cbe___x, unsigned int *llvm_cbe___y) __ATTRIBUTE_WEAK__;
unsigned int *_ZNKSt10_Select1stISt4pairIKj10SAsnDynstrEEclERKS3_(struct l_class_OC_BaseEvent *llvm_cbe_this, struct l_unnamed78 *llvm_cbe___x) __ATTRIBUTE_WEAK__;
unsigned int *_ZNSt8_Rb_treeIjSt4pairIKj10SAsnDynstrESt10_Select1stIS3_ESt4lessIjESaIS3_EE6_S_keyEPKSt13_Rb_tree_nodeIS3_E(struct l_struct_OC_std_KD__KD__Rb_tree_node *llvm_cbe___x) __ATTRIBUTE_WEAK__;
struct l_struct_OC_std_KD__KD__Rb_tree_node *_ZNSt8_Rb_treeIjSt4pairIKj10SAsnDynstrESt10_Select1stIS3_ESt4lessIjESaIS3_EE7_S_leftEPSt18_Rb_tree_node_base(struct l_struct_OC_std_KD__KD__Rb_tree_node_base *llvm_cbe___x) __ATTRIBUTE_WEAK__;
struct l_struct_OC_std_KD__KD__Rb_tree_node *_ZNSt8_Rb_treeIjSt4pairIKj10SAsnDynstrESt10_Select1stIS3_ESt4lessIjESaIS3_EE8_S_rightEPSt18_Rb_tree_node_base(struct l_struct_OC_std_KD__KD__Rb_tree_node_base *llvm_cbe___x) __ATTRIBUTE_WEAK__;
void _ZNSt17_Rb_tree_iteratorISt4pairIKj10SAsnDynstrEEC1EPSt13_Rb_tree_nodeIS3_E(struct l_struct_OC_std_KD__KD__Rb_tree_iterator *llvm_cbe_this, struct l_struct_OC_std_KD__KD__Rb_tree_node *llvm_cbe___x) __ATTRIBUTE_WEAK__;
bool _ZNKSt17_Rb_tree_iteratorISt4pairIKj10SAsnDynstrEEeqERKS4_(struct l_struct_OC_std_KD__KD__Rb_tree_iterator *llvm_cbe_this, struct l_struct_OC_std_KD__KD__Rb_tree_iterator *llvm_cbe___x) __ATTRIBUTE_WEAK__;
unsigned long long _ZNSt8_Rb_treeIjSt4pairIKj10SAsnDynstrESt10_Select1stIS3_ESt4lessIjESaIS3_EE5beginEv(struct l_class_OC_std_KD__KD__Rb_tree *llvm_cbe_this) __ATTRIBUTE_WEAK__;
void _ZNSt4pairISt17_Rb_tree_iteratorIS_IKj10SAsnDynstrEEbEC1ERKS4_RKb(struct l_struct_OC_std_KD__KD_pair *llvm_cbe_this, struct l_struct_OC_std_KD__KD__Rb_tree_iterator *llvm_cbe___a, unsigned char *llvm_cbe___b) __ATTRIBUTE_WEAK__;
unsigned long long _ZNSt8_Rb_treeIjSt4pairIKj10SAsnDynstrESt10_Select1stIS3_ESt4lessIjESaIS3_EE10_M_insert_EPKSt18_Rb_tree_node_baseSC_RKS3_(struct l_class_OC_std_KD__KD__Rb_tree *llvm_cbe_this, struct l_struct_OC_std_KD__KD__Rb_tree_node_base *llvm_cbe___x, struct l_struct_OC_std_KD__KD__Rb_tree_node_base *llvm_cbe___y, struct l_unnamed78 *llvm_cbe___v) __ATTRIBUTE_WEAK__;
struct l_struct_OC_std_KD__KD__Rb_tree_iterator *_ZNSt17_Rb_tree_iteratorISt4pairIKj10SAsnDynstrEEmmEv(struct l_struct_OC_std_KD__KD__Rb_tree_iterator *llvm_cbe_this) __ATTRIBUTE_WEAK__;
unsigned int *_ZNSt8_Rb_treeIjSt4pairIKj10SAsnDynstrESt10_Select1stIS3_ESt4lessIjESaIS3_EE6_S_keyEPKSt18_Rb_tree_node_base(struct l_struct_OC_std_KD__KD__Rb_tree_node_base *llvm_cbe___x) __ATTRIBUTE_WEAK__;
struct l_unnamed78 *_ZNSt8_Rb_treeIjSt4pairIKj10SAsnDynstrESt10_Select1stIS3_ESt4lessIjESaIS3_EE8_S_valueEPKSt18_Rb_tree_node_base(struct l_struct_OC_std_KD__KD__Rb_tree_node_base *llvm_cbe___x) __ATTRIBUTE_WEAK__;
struct l_struct_OC_std_KD__KD__Rb_tree_node_base *_ZSt18_Rb_tree_decrementPSt18_Rb_tree_node_base(struct l_struct_OC_std_KD__KD__Rb_tree_node_base *);
struct l_struct_OC_std_KD__KD__Rb_tree_node *_ZNSt8_Rb_treeIjSt4pairIKj10SAsnDynstrESt10_Select1stIS3_ESt4lessIjESaIS3_EE14_M_create_nodeERKS3_(struct l_class_OC_std_KD__KD__Rb_tree *llvm_cbe_this, struct l_unnamed78 *llvm_cbe___x) __ATTRIBUTE_WEAK__;
void _ZSt29_Rb_tree_insert_and_rebalancebPSt18_Rb_tree_node_baseS0_RS_(bool , struct l_struct_OC_std_KD__KD__Rb_tree_node_base *, struct l_struct_OC_std_KD__KD__Rb_tree_node_base *, struct l_struct_OC_std_KD__KD__Rb_tree_node_base *);
struct l_struct_OC_std_KD__KD__Rb_tree_node *_ZNSt8_Rb_treeIjSt4pairIKj10SAsnDynstrESt10_Select1stIS3_ESt4lessIjESaIS3_EE11_M_get_nodeEv(struct l_class_OC_std_KD__KD__Rb_tree *llvm_cbe_this) __ATTRIBUTE_WEAK__;
struct l_class_OC_BaseEvent _ZNKSt8_Rb_treeIjSt4pairIKj10SAsnDynstrESt10_Select1stIS3_ESt4lessIjESaIS3_EE13get_allocatorEv(struct l_class_OC_std_KD__KD__Rb_tree *llvm_cbe_this) __ATTRIBUTE_WEAK__;
void _ZNSaISt4pairIKj10SAsnDynstrEED1Ev(struct l_class_OC_BaseEvent *llvm_cbe_this) __ATTRIBUTE_WEAK__;
void _ZN9__gnu_cxx13new_allocatorISt4pairIKj10SAsnDynstrEE9constructEPS4_RKS4_(struct l_class_OC_BaseEvent *llvm_cbe_this, struct l_unnamed78 *llvm_cbe___p, struct l_unnamed78 *llvm_cbe___val) __ATTRIBUTE_WEAK__;
unsigned char *__cxa_begin_catch(unsigned char *);
void _ZNSt8_Rb_treeIjSt4pairIKj10SAsnDynstrESt10_Select1stIS3_ESt4lessIjESaIS3_EE11_M_put_nodeEPSt13_Rb_tree_nodeIS3_E(struct l_class_OC_std_KD__KD__Rb_tree *llvm_cbe_this, struct l_struct_OC_std_KD__KD__Rb_tree_node *llvm_cbe___p) __ATTRIBUTE_WEAK__;
void __cxa_rethrow(void);
void __cxa_end_catch(void);
void _ZN9__gnu_cxx13new_allocatorISt13_Rb_tree_nodeISt4pairIKj10SAsnDynstrEEE10deallocateEPS6_m(struct l_class_OC_BaseEvent *llvm_cbe_this, struct l_struct_OC_std_KD__KD__Rb_tree_node *llvm_cbe___p, unsigned long long ) __ATTRIBUTE_WEAK__;
unsigned char *_ZnwmPv(unsigned long long , unsigned char *llvm_cbe___p) __ATTRIBUTE_WEAK__;
void _ZNSaISt4pairIKj10SAsnDynstrEED2Ev(struct l_class_OC_BaseEvent *llvm_cbe_this) __ATTRIBUTE_WEAK__;
void _ZN9__gnu_cxx13new_allocatorISt4pairIKj10SAsnDynstrEED2Ev(struct l_class_OC_BaseEvent *llvm_cbe_this) __ATTRIBUTE_WEAK__;
void _ZNSaISt4pairIKj10SAsnDynstrEEC1ISt13_Rb_tree_nodeIS2_EEERKSaIT_E(struct l_class_OC_BaseEvent *llvm_cbe_this, struct l_class_OC_BaseEvent *) __ATTRIBUTE_WEAK__;
struct l_class_OC_BaseEvent *_ZNKSt8_Rb_treeIjSt4pairIKj10SAsnDynstrESt10_Select1stIS3_ESt4lessIjESaIS3_EE21_M_get_Node_allocatorEv(struct l_class_OC_std_KD__KD__Rb_tree *llvm_cbe_this) __ATTRIBUTE_WEAK__;
void _ZNSaISt4pairIKj10SAsnDynstrEEC2ISt13_Rb_tree_nodeIS2_EEERKSaIT_E(struct l_class_OC_BaseEvent *llvm_cbe_this, struct l_class_OC_BaseEvent *) __ATTRIBUTE_WEAK__;
void _ZN9__gnu_cxx13new_allocatorISt4pairIKj10SAsnDynstrEEC2Ev(struct l_class_OC_BaseEvent *llvm_cbe_this) __ATTRIBUTE_WEAK__;
struct l_struct_OC_std_KD__KD__Rb_tree_node *_ZN9__gnu_cxx13new_allocatorISt13_Rb_tree_nodeISt4pairIKj10SAsnDynstrEEE8allocateEmPKv(struct l_class_OC_BaseEvent *llvm_cbe_this, unsigned long long llvm_cbe___n, unsigned char *) __ATTRIBUTE_WEAK__;
unsigned long long _ZNK9__gnu_cxx13new_allocatorISt13_Rb_tree_nodeISt4pairIKj10SAsnDynstrEEE8max_sizeEv(struct l_class_OC_BaseEvent *llvm_cbe_this) __ATTRIBUTE_WEAK__;
void _ZSt17__throw_bad_allocv(void);
void _ZNSt4pairISt17_Rb_tree_iteratorIS_IKj10SAsnDynstrEEbEC2ERKS4_RKb(struct l_struct_OC_std_KD__KD_pair *llvm_cbe_this, struct l_struct_OC_std_KD__KD__Rb_tree_iterator *llvm_cbe___a, unsigned char *llvm_cbe___b) __ATTRIBUTE_WEAK__;
void _ZNSt17_Rb_tree_iteratorISt4pairIKj10SAsnDynstrEEC2EPSt13_Rb_tree_nodeIS3_E(struct l_struct_OC_std_KD__KD__Rb_tree_iterator *llvm_cbe_this, struct l_struct_OC_std_KD__KD__Rb_tree_node *llvm_cbe___x) __ATTRIBUTE_WEAK__;
struct l_unnamed78 *_ZNSt8_Rb_treeIjSt4pairIKj10SAsnDynstrESt10_Select1stIS3_ESt4lessIjESaIS3_EE8_S_valueEPKSt13_Rb_tree_nodeIS3_E(struct l_struct_OC_std_KD__KD__Rb_tree_node *llvm_cbe___x) __ATTRIBUTE_WEAK__;
void _ZNSt4pairISt17_Rb_tree_iteratorIS_IKj10SAsnDynstrEEbEC2Ev(struct l_struct_OC_std_KD__KD_pair *llvm_cbe_this) __ATTRIBUTE_WEAK__;
void _ZNSt17_Rb_tree_iteratorISt4pairIKj10SAsnDynstrEEC1Ev(struct l_struct_OC_std_KD__KD__Rb_tree_iterator *llvm_cbe_this) __ATTRIBUTE_WEAK__;
void _ZNSt17_Rb_tree_iteratorISt4pairIKj10SAsnDynstrEEC2Ev(struct l_struct_OC_std_KD__KD__Rb_tree_iterator *llvm_cbe_this) __ATTRIBUTE_WEAK__;
void _ZN3Uec8UecEventD2Ev(struct l_class_OC_Uec_KD__KD_UecEvent *llvm_cbe_this) __ATTRIBUTE_WEAK__;
void _ZN6CEventD2Ev(struct l_class_OC_CEvent *llvm_cbe_this) __ATTRIBUTE_WEAK__;
void _ZN3Uec8UecEventD0Ev(struct l_class_OC_Uec_KD__KD_UecEvent *llvm_cbe_this) __ATTRIBUTE_WEAK__;
struct l_class_OC_CEvent *_ZNK3Uec8UecEvent5CloneEv(struct l_class_OC_Uec_KD__KD_UecEvent *llvm_cbe_this) __ATTRIBUTE_WEAK__;
void _ZN3Uec8UecEventC1ERKS0_(struct l_class_OC_Uec_KD__KD_UecEvent *llvm_cbe_this, struct l_class_OC_Uec_KD__KD_UecEvent *llvm_cbe_rhs) __ATTRIBUTE_WEAK__;
void _ZN3Uec8UecEventC2ERKS0_(struct l_class_OC_Uec_KD__KD_UecEvent *llvm_cbe_this, struct l_class_OC_Uec_KD__KD_UecEvent *llvm_cbe_rhs) __ATTRIBUTE_WEAK__;
void _ZN6CEventC2ERKS_(struct l_class_OC_CEvent *llvm_cbe_this, struct l_class_OC_CEvent *) __ATTRIBUTE_WEAK__;
void _ZN6CEventD1Ev(struct l_class_OC_CEvent *llvm_cbe_this) __ATTRIBUTE_WEAK__;
void _ZN6CEventD0Ev(struct l_class_OC_CEvent *llvm_cbe_this) __ATTRIBUTE_WEAK__;
struct l_class_OC_CEvent *_ZNK6CEvent5CloneEv(struct l_class_OC_CEvent *llvm_cbe_this) __ATTRIBUTE_WEAK__;
void _ZN6CEventC1ERKS_(struct l_class_OC_CEvent *llvm_cbe_this, struct l_class_OC_CEvent *) __ATTRIBUTE_WEAK__;
void _ZN3Uec8UecEventC2EjNS_15EUecServiceTypeEjPvjiS2_j(struct l_class_OC_Uec_KD__KD_UecEvent *llvm_cbe_this, unsigned int llvm_cbe_messageId, unsigned int llvm_cbe_serviceType, unsigned int llvm_cbe_instanceId, unsigned char *llvm_cbe_payLoadPtr, unsigned int llvm_cbe_payloadSize, unsigned int llvm_cbe_asn1PayLoadId, unsigned char *llvm_cbe_asn1PayLoadPtr, unsigned int llvm_cbe_asn1PayloadSize) __ATTRIBUTE_WEAK__;
void _ZN6CEventC2ENS_10EEventTypeEj(struct l_class_OC_CEvent *llvm_cbe_this, unsigned int llvm_cbe_eventType, unsigned int llvm_cbe_eventId) __ATTRIBUTE_WEAK__;
void _ZN3Uec16UecUeContextData10connectMmeEv(struct l_class_OC_Uec_KD__KD_UecUeContextData *llvm_cbe_this) __ATTRIBUTE_WEAK__;
unsigned int _ZNSt8ios_base4setfESt13_Ios_FmtflagsS0_(struct l_class_OC_std_KD__KD_ios_base *llvm_cbe_this, unsigned int llvm_cbe___fmtfl, unsigned int llvm_cbe___mask) __ATTRIBUTE_WEAK__;
unsigned int *_ZStaNRSt13_Ios_FmtflagsS_(unsigned int *llvm_cbe___a, unsigned int llvm_cbe___b) __ATTRIBUTE_WEAK__;
unsigned int _ZStcoSt13_Ios_Fmtflags(unsigned int llvm_cbe___a) __ATTRIBUTE_WEAK__;
unsigned int *_ZStoRRSt13_Ios_FmtflagsS_(unsigned int *llvm_cbe___a, unsigned int llvm_cbe___b) __ATTRIBUTE_WEAK__;
unsigned int _ZStanSt13_Ios_FmtflagsS_(unsigned int llvm_cbe___a, unsigned int llvm_cbe___b) __ATTRIBUTE_WEAK__;
unsigned int _ZStorSt13_Ios_FmtflagsS_(unsigned int llvm_cbe___a, unsigned int llvm_cbe___b) __ATTRIBUTE_WEAK__;
static void _GLOBAL__I_a(void) __ATTRIBUTE_CTOR__;
void abort(void);
unsigned char *memset(unsigned char *, unsigned int , unsigned long long );
unsigned char *memcpy(unsigned char *, unsigned char *, unsigned long long );


/* Global Variable Declarations */
static struct l_class_OC_BaseEvent _ZStL8__ioinit;
static struct l_class_OC_BaseEvent _ZN5boost6tuplesL6ignoreE;
static struct l_unnamed77 _OC_str;
extern struct l_unnamed6 _ZTVN3Uec17UecDirectTransferE;
static struct l_unnamed7 _OC_str2;
static struct l_unnamed9 __PRETTY_FUNCTION___OC__ZNK3Uec17UecDirectTransfer30handleRrcDlInformationTransferEPNS_8UecEventE;
static struct l_unnamed81 _OC_str3;
static struct l_unnamed18 _OC_str4;
static struct l_unnamed20 _OC_str5;
static struct l_unnamed86 _OC_str6;
static struct l_unnamed16 _OC_str7;
static struct l_unnamed86 _OC_str8;
static struct l_unnamed10 __PRETTY_FUNCTION___OC__ZN3Uec17UecDirectTransfer30handleRrcUlInformationTransferEPNS_8UecEventE;
static struct l_unnamed81 _OC_str9;
static struct l_unnamed20 _OC_str10;
static struct l_unnamed11 _OC_str11;
static struct l_unnamed15 __PRETTY_FUNCTION___OC__ZN3Uec17UecDirectTransfer24handleS1ApDlNasTransportEPNS_8UecEventE;
static struct l_unnamed82 _OC_str12;
static struct l_unnamed84 _OC_str13;
static struct l_unnamed86 _OC_str14;
static struct l_unnamed76 _OC_str15;
static struct l_unnamed13 _OC_str16;
static struct l_unnamed87 _OC_str17;
static struct l_unnamed87 _OC_str18;
static struct l_unnamed87 _OC_str19;
static struct l_unnamed83 _OC_str20;
static struct l_unnamed8 _OC_str21;
static struct l_unnamed75 __PRETTY_FUNCTION___OC__ZN3Uec17UecDirectTransfer28handleTupL3MessageIndicationEPNS_8UecEventE;
static struct l_unnamed17 _OC_str22;
static struct l_unnamed80 _OC_str23;
static struct l_unnamed87 _OC_str24;
static struct l_unnamed86 _OC_str25;
static struct l_unnamed0 _OC_str26;
static struct l_unnamed92 _OC_str27;
static struct l_unnamed71 __PRETTY_FUNCTION___OC__ZN3Uec17UecDirectTransfer22handleTupSrbReceiveIndEPNS_8UecEventE;
static struct l_unnamed80 _OC_str28;
static struct l_unnamed82 _OC_str29;
static struct l_unnamed87 _OC_str30;
static struct l_unnamed94 _OC_str31;
static struct l_unnamed69 _OC_str32;
static struct l_unnamed1 _OC_str33;
static struct l_unnamed70 _OC_str34;
static struct l_unnamed86 _OC_str35;
static struct l_unnamed14 _OC_str36;
static struct l_unnamed68 _OC_str37;
static struct l_unnamed31 __PRETTY_FUNCTION___OC__ZN3Uec17UecDirectTransfer20handleTupSrbSendRespEPNS_8UecEventE;
static struct l_unnamed2 _OC_str38;
static struct l_unnamed69 _OC_str39;
static struct l_unnamed94 _OC_str40;
static struct l_unnamed92 _OC_str41;
static struct l_unnamed32 _OC_str42;
static struct l_unnamed67 _OC_str43;
static struct l_unnamed33 _OC_str44;
static struct l_unnamed66 __PRETTY_FUNCTION___OC__ZNK3Uec17UecDirectTransfer26handleUecDtfDirectTransferEPNS_8UecEventE;
static struct l_unnamed67 _OC_str45;
static struct l_unnamed75 __PRETTY_FUNCTION___OC__ZN3Uec17UecDirectTransfer28sendRrcDlInformationTransferEPNS_8UecEventE;
static struct l_unnamed17 _OC_str46;
static struct l_unnamed63 _OC_str47;
static struct l_unnamed62 _OC_str48;
static struct l_unnamed15 __PRETTY_FUNCTION___OC__ZN3Uec17UecDirectTransfer24sendS1ApInitialUeMessageEPNS_8UecEventE;
static struct l_unnamed82 _OC_str49;
static struct l_unnamed63 _OC_str50;
static struct l_unnamed66 __PRETTY_FUNCTION___OC__ZN3Uec17UecDirectTransfer32sendS1ApNasNonDeliveryIndicationEPNS_8UecEventE;
static struct l_unnamed58 _OC_str51;
static struct l_unnamed34 _OC_str52;
static struct l_unnamed35 _OC_str53;
static struct l_unnamed71 __PRETTY_FUNCTION___OC__ZN3Uec17UecDirectTransfer22sendS1ApUlNasTransportEPNS_8UecEventE;
static struct l_unnamed56 _OC_str54;
static struct l_unnamed36 _OC_str55;
static struct l_unnamed58 _OC_str56;
static struct l_unnamed3 _OC_str57;
static struct l_unnamed82 _OC_str58;
static struct l_unnamed37 __PRETTY_FUNCTION___OC__ZN3Uec17UecDirectTransfer27setRrcDlInformationTransferEPNS_8UecEventER26SErrcDLInformationTransfer;
static struct l_unnamed93 _OC_str59;
static struct l_unnamed55 _OC_str60;
static struct l_unnamed70 _OC_str61;
static struct l_unnamed53 __PRETTY_FUNCTION___OC__ZN3Uec17UecDirectTransfer23setS1ApInitialUeMessageEPNS_8UecEventER23SS1apUFInitialUEMessage;
static struct l_unnamed79 __PRETTY_FUNCTION___OC__ZN3Uec17UecDirectTransfer31setS1ApNasNonDeliveryIndicationEPNS_8UecEventER31SS1apUFNASNonDeliveryIndication;
static struct l_unnamed84 _OC_str62;
static struct l_unnamed20 _OC_str63;
static struct l_unnamed53 __PRETTY_FUNCTION___OC__ZN3Uec17UecDirectTransfer21setS1ApUlNasTransportEPNS_8UecEventER25SS1apUFUplinkNASTransport;
static struct l_unnamed16 _OC_str64;
static struct l_unnamed68 _OC_str65;
static struct l_unnamed38 __PRETTY_FUNCTION___OC__ZN3Uec17UecDirectTransfer22setTupL3MessageRequestER16TUP_L3MessageReq;
static struct l_unnamed39 __PRETTY_FUNCTION___OC__ZN3Uec17UecDirectTransfer20setTupSrbSendRequestER14TUP_SrbSendReqj;
static struct l_unnamed40 __PRETTY_FUNCTION___OC__ZN3Uec17UecDirectTransfer11storeNasPduEPNS_8UecEventE;
static struct l_unnamed20 _OC_str66;
static struct l_unnamed52 _OC_str67;
static struct l_unnamed44 __PRETTY_FUNCTION___OC__ZN3Uec17UecDirectTransfer31handleEmbeddedHoRestrictionListEPK27SS1apUFDownlinkNASTransportPNS_16UecUeContextDataE;
static struct l_unnamed51 _OC_str68;
static struct l_unnamed17 _OC_str69;
static struct l_unnamed80 _OC_str70;
static struct l_unnamed55 _OC_str71;
static struct l_unnamed50 _OC_str72;
static struct l_unnamed93 _OC_str73;
static struct l_unnamed93 _OC_str74;
static struct l_unnamed84 _OC_str75;
static struct l_unnamed45 _OC_str76;
static struct l_unnamed83 _OC_str77;
static struct l_unnamed20 _OC_str78;
static struct l_unnamed20 _OC_str79;
static struct l_unnamed83 _OC_str80;
static struct l_unnamed51 __PRETTY_FUNCTION___OC__ZN3Uec17UecDirectTransfer9StateIdleEP6CEvent;
static struct l_unnamed57 _OC_str81;
static struct l_unnamed77 _OC_str82;
static struct l_unnamed62 _OC_str83;
static struct l_unnamed18 _OC_str84;
static struct l_unnamed93 _OC_str85;
static struct l_unnamed57 _OC_str86;
static struct l_unnamed41 _OC_str87;
static struct l_unnamed42 _OC_str88;
static struct l_unnamed76 _OC_str89;
static struct l_unnamed93 _OC_str90;
static struct l_unnamed81 _OC_str91;
static struct l_unnamed52 _OC_str92;
static struct l_unnamed55 _OC_str93;
static struct l_unnamed57 _OC_str94;
extern struct l_unnamed47 _ZTV7CStateTIN3Uec17UecDirectTransferEE __ATTRIBUTE_WEAK__;
extern struct l_unnamed47 _ZTV10CStateBase __ATTRIBUTE_WEAK__;
static unsigned char *_ZZ18__gthread_active_pvE20__gthread_active_ptr;
static struct l_unnamed84 _OC_str95;
static struct l_unnamed46 _OC_str96;
static struct l_unnamed93 _OC_str97;
static struct l_unnamed5 _OC_str98;
static struct l_unnamed4 _OC_str99;
extern struct l_unnamed43 _ZTVN3Uec8UecEventE __ATTRIBUTE_WEAK__;
extern struct l_unnamed43 _ZTV6CEvent __ATTRIBUTE_WEAK__;
static struct l_unnamed56 _OC_str100;


/* Global Variable Definitions and Initialization */
static struct l_class_OC_BaseEvent _ZStL8__ioinit;
static struct l_class_OC_BaseEvent _ZN5boost6tuplesL6ignoreE;
static struct l_unnamed77 _OC_str = { "UecDirectTransfer" };
struct l_unnamed6 _ZTVN3Uec17UecDirectTransferE = { { ((unsigned char *)/*NULL*/0), ((unsigned char *)/*NULL*/0), ((unsigned char *)_ZN3Uec17UecDirectTransferD2Ev), ((unsigned char *)_ZN3Uec17UecDirectTransferD0Ev), ((unsigned char *)_ZN3Uec14UecServiceBase12processEventEPNS_8UecEventEPNS_18UecServicesEventIfE), ((unsigned char *)_ZN3Uec14UecServiceBase12processEventEPNS_8UecEventE), ((unsigned char *)_ZN3Uec14UecServiceBase6notifyENS_13EUecServiceIdENS_15EUecServiceTypeEjiS1_), ((unsigned char *)_ZN3Uec14UecServiceBase12resetServiceEv), ((unsigned char *)_ZN3Uec14UecServiceBase8rollBackEv), ((unsigned char *)_ZN3Uec14UecServiceBase12getServiceIdEv), ((unsigned char *)_ZN3Uec17UecDirectTransfer7doResetEv), ((unsigned char *)(unsigned long)18446744073709551608ull), ((unsigned char *)/*NULL*/0), ((unsigned char *)_ZThn8_N3Uec17UecDirectTransferD1Ev), ((unsigned char *)_ZThn8_N3Uec17UecDirectTransferD0Ev), ((unsigned char *)_ZN3Uec10UecFsmBase7InitFSMEv), ((unsigned char *)_ZN3Uec10UecFsmBase8StartFSMEv) } };
static struct l_unnamed7 _OC_str2 = { "StateIdle" };
static struct l_unnamed9 __PRETTY_FUNCTION___OC__ZNK3Uec17UecDirectTransfer30handleRrcDlInformationTransferEPNS_8UecEventE = { "EBaseStatus Uec::UecDirectTransfer::handleRrcDlInformationTransfer(Uec::UecEvent *) const" };
static struct l_unnamed81 _OC_str3 = { "handleRrcDlInformationTransfer() ueContextId:" };
static struct l_unnamed18 _OC_str4 = { " unknown discriminator:" };
static struct l_unnamed20 _OC_str5 = { " unknown c1.discriminator:" };
static struct l_unnamed86 _OC_str6 = { " payLoadId:0x" };
static struct l_unnamed16 _OC_str7 = { " NAD PDU discarded missing nasPdu.n:" };
static struct l_unnamed86 _OC_str8 = { " nasPdu.st:0x" };
static struct l_unnamed10 __PRETTY_FUNCTION___OC__ZN3Uec17UecDirectTransfer30handleRrcUlInformationTransferEPNS_8UecEventE = { "EBaseStatus Uec::UecDirectTransfer::handleRrcUlInformationTransfer(Uec::UecEvent *)" };
static struct l_unnamed81 _OC_str9 = { "handleRrcUlInformationTransfer() ueContextId:" };
static struct l_unnamed20 _OC_str10 = { " unexpected discrimonator:" };
static struct l_unnamed11 _OC_str11 = { " or c1.discriminator:" };
static struct l_unnamed15 __PRETTY_FUNCTION___OC__ZN3Uec17UecDirectTransfer24handleS1ApDlNasTransportEPNS_8UecEventE = { "EBaseStatus Uec::UecDirectTransfer::handleS1ApDlNasTransport(Uec::UecEvent *)" };
static struct l_unnamed82 _OC_str12 = { "handleS1ApDlNasTransport() ueContextId:" };
static struct l_unnamed84 _OC_str13 = { " S1Ap interface synchronized with eNBUES1APID:" };
static struct l_unnamed86 _OC_str14 = { " mMEUES1APID:" };
static struct l_unnamed76 _OC_str15 = { " stopped TEstS1Con timerId:" };
static struct l_unnamed13 _OC_str16 = { " NAD PDU discarded wrong destination address eNBUES1APID." };
static struct l_unnamed87 _OC_str17 = { " getEnbUeS1Id(" };
static struct l_unnamed87 _OC_str18 = { ") mMEUES1APID:" };
static struct l_unnamed87 _OC_str19 = { " getMmeUeS1Id(" };
static struct l_unnamed83 _OC_str20 = { ")" };
static struct l_unnamed8 _OC_str21 = { ", ERROR:  Unable to handle HO RestrictionList due to invalid parameters! DirectTransfer shall be continued." };
static struct l_unnamed75 __PRETTY_FUNCTION___OC__ZN3Uec17UecDirectTransfer28handleTupL3MessageIndicationEPNS_8UecEventE = { "EBaseStatus Uec::UecDirectTransfer::handleTupL3MessageIndication(Uec::UecEvent *)" };
static struct l_unnamed17 _OC_str22 = { "handleTupL3MessageIndication() ueContextId:" };
static struct l_unnamed80 _OC_str23 = { " payloadId:TUPC_L3S1_MESSAGE_IND_MSG linkId:" };
static struct l_unnamed87 _OC_str24 = { " uecContextId:" };
static struct l_unnamed86 _OC_str25 = { " getS1LinkId(" };
static struct l_unnamed0 _OC_str26 = { ") or linkId:" };
static struct l_unnamed92 _OC_str27 = { " does not match" };
static struct l_unnamed71 __PRETTY_FUNCTION___OC__ZN3Uec17UecDirectTransfer22handleTupSrbReceiveIndEPNS_8UecEventE = { "EBaseStatus Uec::UecDirectTransfer::handleTupSrbReceiveInd(Uec::UecEvent *)" };
static struct l_unnamed80 _OC_str28 = { "handleTupSrbReceiveIndication() ueContextId:" };
static struct l_unnamed82 _OC_str29 = { " payloadId:TUP_SRB_RECEIVE_IND lnCelId:" };
static struct l_unnamed87 _OC_str30 = { " uplaneCellId:" };
static struct l_unnamed94 _OC_str31 = { " crnti:" };
static struct l_unnamed69 _OC_str32 = { " payloadId:TUP_SRB_RECEIVE_IND: lnCelId:" };
static struct l_unnamed1 _OC_str33 = { " getUplaneCellId():" };
static struct l_unnamed70 _OC_str34 = { " and uplaneCellId:" };
static struct l_unnamed86 _OC_str35 = { " do not match" };
static struct l_unnamed14 _OC_str36 = { " hasRrcIntegrityCheckResult:" };
static struct l_unnamed68 _OC_str37 = { " rrcIntegrityCheckResult:" };
static struct l_unnamed31 __PRETTY_FUNCTION___OC__ZN3Uec17UecDirectTransfer20handleTupSrbSendRespEPNS_8UecEventE = { "EBaseStatus Uec::UecDirectTransfer::handleTupSrbSendResp(Uec::UecEvent *)" };
static struct l_unnamed2 _OC_str38 = { "handleTupSrbSendResp() ueContextId:" };
static struct l_unnamed69 _OC_str39 = { " payLoadId:TUP_SRB_SEND_RESP_MSG status:" };
static struct l_unnamed94 _OC_str40 = { " cause:" };
static struct l_unnamed92 _OC_str41 = { " specificCause:" };
static struct l_unnamed32 _OC_str42 = { " numOfL3Payload:" };
static struct l_unnamed67 _OC_str43 = { " payloadId:TUP_SRB_SEND_RESP_MSG lnCelId:" };
static struct l_unnamed33 _OC_str44 = { " payLoadId:TUP_SRB_SEND_RESP_MSG unexpected messageResult status:" };
static struct l_unnamed66 __PRETTY_FUNCTION___OC__ZNK3Uec17UecDirectTransfer26handleUecDtfDirectTransferEPNS_8UecEventE = { "EBaseStatus Uec::UecDirectTransfer::handleUecDtfDirectTransfer(Uec::UecEvent *) const" };
static struct l_unnamed67 _OC_str45 = { "handleUecDtfDirectTransfer() ueContextId:" };
static struct l_unnamed75 __PRETTY_FUNCTION___OC__ZN3Uec17UecDirectTransfer28sendRrcDlInformationTransferEPNS_8UecEventE = { "EBaseStatus Uec::UecDirectTransfer::sendRrcDlInformationTransfer(Uec::UecEvent *)" };
static struct l_unnamed17 _OC_str46 = { "sendRrcDlInformationTransfer() ueContextId:" };
static struct l_unnamed63 _OC_str47 = { " TUP_SGNLSRB_PORT->sendEvent() payLoadId:TUP_SRB_SEND_REQ_MSG asn1PayLoadId:RRC_DL_INFORMATION_TRANSFER nasPdu.n:" };
static struct l_unnamed62 _OC_str48 = { " sendEvent() returned l_success:" };
static struct l_unnamed15 __PRETTY_FUNCTION___OC__ZN3Uec17UecDirectTransfer24sendS1ApInitialUeMessageEPNS_8UecEventE = { "EBaseStatus Uec::UecDirectTransfer::sendS1ApInitialUeMessage(Uec::UecEvent *)" };
static struct l_unnamed82 _OC_str49 = { "sendS1ApInitialUeMessage() ueContextId:" };
static struct l_unnamed63 _OC_str50 = { " TUP_SGNLNW_PORT->sendEvent() payLoadId:TUPC_L3S1_MESSAGE_REQ_MSG asn1PayLoadId:S1AP_INITIAL_UE_MESSAGE nasPdu.n:" };
static struct l_unnamed66 __PRETTY_FUNCTION___OC__ZN3Uec17UecDirectTransfer32sendS1ApNasNonDeliveryIndicationEPNS_8UecEventE = { "EBaseStatus Uec::UecDirectTransfer::sendS1ApNasNonDeliveryIndication(Uec::UecEvent *)" };
static struct l_unnamed58 _OC_str51 = { "sendS1ApNasNonDeliveryIndication() ueContextId:" };
static struct l_unnamed34 _OC_str52 = { " S1ApNasNonDeliveryIndication message discarded mmeUeS1X2Id not yet synchronized isValidMmeUeS1X2Id:" };
static struct l_unnamed35 _OC_str53 = { " TUP_SGNLNW_PORT->sendEvent() payLoadId:TUPC_L3S1_MESSAGE_REQ_MSG asn1PayLoadId:S1AP_NAS_NON_DELIVERY_INDICATION nasPdu.n:" };
static struct l_unnamed71 __PRETTY_FUNCTION___OC__ZN3Uec17UecDirectTransfer22sendS1ApUlNasTransportEPNS_8UecEventE = { "EBaseStatus Uec::UecDirectTransfer::sendS1ApUlNasTransport(Uec::UecEvent *)" };
static struct l_unnamed56 _OC_str54 = { "sendS1ApUlNasTransport() ueContextId:" };
static struct l_unnamed36 _OC_str55 = { " invalid RRC payload" };
static struct l_unnamed58 _OC_str56 = { " NAS PDU buffered mmeUeS1X2Id not yet available" };
static struct l_unnamed3 _OC_str57 = { " TUP_SGNLNW_PORT->sendEvent() payLoadId:TUPC_L3S1_MESSAGE_REQ_MSG asn1PayLoadId:S1AP_UPLINK_NAS_TRANSPORT nasPdu.n:" };
static struct l_unnamed82 _OC_str58 = { "sendS1ApUlNasTransport() is SUPPRESSED." };
static struct l_unnamed37 __PRETTY_FUNCTION___OC__ZN3Uec17UecDirectTransfer27setRrcDlInformationTransferEPNS_8UecEventER26SErrcDLInformationTransfer = { "EBaseStatus Uec::UecDirectTransfer::setRrcDlInformationTransfer(Uec::UecEvent *, SErrcDLInformationTransfer &)" };
static struct l_unnamed93 _OC_str59 = { "setRrcDlInformationTransfer() ueContextId:" };
static struct l_unnamed55 _OC_str60 = { " unexpected payLoadId:" };
static struct l_unnamed70 _OC_str61 = { " or asn1PayLoadId:" };
static struct l_unnamed53 __PRETTY_FUNCTION___OC__ZN3Uec17UecDirectTransfer23setS1ApInitialUeMessageEPNS_8UecEventER23SS1apUFInitialUEMessage = { "EBaseStatus Uec::UecDirectTransfer::setS1ApInitialUeMessage(Uec::UecEvent *, SS1apUFInitialUEMessage &)" };
static struct l_unnamed79 __PRETTY_FUNCTION___OC__ZN3Uec17UecDirectTransfer31setS1ApNasNonDeliveryIndicationEPNS_8UecEventER31SS1apUFNASNonDeliveryIndication = { "EBaseStatus Uec::UecDirectTransfer::setS1ApNasNonDeliveryIndication(Uec::UecEvent *, SS1apUFNASNonDeliveryIndication &)" };
static struct l_unnamed84 _OC_str62 = { "setS1ApNasNonDeliveryIndication() ueContextId:" };
static struct l_unnamed20 _OC_str63 = { " unexpected asn1PayLoadId:" };
static struct l_unnamed53 __PRETTY_FUNCTION___OC__ZN3Uec17UecDirectTransfer21setS1ApUlNasTransportEPNS_8UecEventER25SS1apUFUplinkNASTransport = { "EBaseStatus Uec::UecDirectTransfer::setS1ApUlNasTransport(Uec::UecEvent *, SS1apUFUplinkNASTransport &)" };
static struct l_unnamed16 _OC_str64 = { "setS1ApUlNasTransport() ueContextId:" };
static struct l_unnamed68 _OC_str65 = { " unknown informationType:" };
static struct l_unnamed38 __PRETTY_FUNCTION___OC__ZN3Uec17UecDirectTransfer22setTupL3MessageRequestER16TUP_L3MessageReq = { "EBaseStatus Uec::UecDirectTransfer::setTupL3MessageRequest(TUP_L3MessageReq &)" };
static struct l_unnamed39 __PRETTY_FUNCTION___OC__ZN3Uec17UecDirectTransfer20setTupSrbSendRequestER14TUP_SrbSendReqj = { "EBaseStatus Uec::UecDirectTransfer::setTupSrbSendRequest(TUP_SrbSendReq &, TBoolean)" };
static struct l_unnamed40 __PRETTY_FUNCTION___OC__ZN3Uec17UecDirectTransfer11storeNasPduEPNS_8UecEventE = { "EBaseStatus Uec::UecDirectTransfer::storeNasPdu(Uec::UecEvent *)" };
static struct l_unnamed20 _OC_str66 = { "storeNasPdu() ueContextId:" };
static struct l_unnamed52 _OC_str67 = { " nASPDU.n:" };
static struct l_unnamed44 __PRETTY_FUNCTION___OC__ZN3Uec17UecDirectTransfer31handleEmbeddedHoRestrictionListEPK27SS1apUFDownlinkNASTransportPNS_16UecUeContextDataE = { "EBaseStatus Uec::UecDirectTransfer::handleEmbeddedHoRestrictionList(SS1apUFDownlinkNASTransport const *const, Uec::UecUeContextData *)" };
static struct l_unnamed51 _OC_str68 = { "handleEmbeddedHoRestrictionList() with ueContextId: " };
static struct l_unnamed17 _OC_str69 = { ", No embedded HO RestrictionList available." };
static struct l_unnamed80 _OC_str70 = { ", ERROR: invalid number of equivalentPLMNs: " };
static struct l_unnamed55 _OC_str71 = { ", expected range [0..." };
static struct l_unnamed50 _OC_str72 = { "]!" };
static struct l_unnamed93 _OC_str73 = { ", ERROR: invalid number of forbidden TAs: " };
static struct l_unnamed93 _OC_str74 = { ", ERROR: invalid number of forbidden LAs: " };
static struct l_unnamed84 _OC_str75 = { ", Embedded HO RestrictionList has been stored." };
static struct l_unnamed45 _OC_str76 = { " Number of equivalent PLMNs: " };
static struct l_unnamed83 _OC_str77 = { "," };
static struct l_unnamed20 _OC_str78 = { " Number of forbidden TAs: " };
static struct l_unnamed20 _OC_str79 = { " Number of forbidden LAs: " };
static struct l_unnamed83 _OC_str80 = { "." };
static struct l_unnamed51 __PRETTY_FUNCTION___OC__ZN3Uec17UecDirectTransfer9StateIdleEP6CEvent = { "TBoolean Uec::UecDirectTransfer::StateIdle(CEvent *)" };
static struct l_unnamed57 _OC_str81 = { "StateIdle() ueContextId:" };
static struct l_unnamed77 _OC_str82 = { " asn1PayLoadId:0x" };
static struct l_unnamed62 _OC_str83 = { " not available getAsn1PayLoad:0x" };
static struct l_unnamed18 _OC_str84 = { " or getAsn1PayLoadId:0x" };
static struct l_unnamed93 _OC_str85 = { " asn1PayLoadId:RRC_UL_INFORMATION_TRANSFER" };
static struct l_unnamed57 _OC_str86 = { " ->processEvent() failed" };
static struct l_unnamed41 _OC_str87 = { " unexpected asn1PayLoadId payLoadId:0x" };
static struct l_unnamed42 _OC_str88 = { " asn1PayLoadId:S1AP_DOWNLINK_NAS_TRANSPORT getNasDelivery(" };
static struct l_unnamed76 _OC_str89 = { " unexpected getNasDelivery(" };
static struct l_unnamed93 _OC_str90 = { " asn1PayLoadId:RRC_DL_INFORMATION_TRANSFER" };
static struct l_unnamed81 _OC_str91 = { " payLoadId:UEC_DTF_DIRECT_TRANSFER direction:" };
static struct l_unnamed52 _OC_str92 = { " nasPdu.n:" };
static struct l_unnamed55 _OC_str93 = { " unexpected direction:" };
static struct l_unnamed57 _OC_str94 = { " unexpected payLoadId:0x" };
struct l_unnamed47 _ZTV7CStateTIN3Uec17UecDirectTransferEE __ATTRIBUTE_WEAK__ = { { ((unsigned char *)/*NULL*/0), ((unsigned char *)/*NULL*/0), ((unsigned char *)_ZN7CStateTIN3Uec17UecDirectTransferEED1Ev), ((unsigned char *)_ZN7CStateTIN3Uec17UecDirectTransferEED0Ev), ((unsigned char *)_ZN7CStateTIN3Uec17UecDirectTransferEE11HandleEventEP6CEvent), ((unsigned char *)_ZN7CStateTIN3Uec17UecDirectTransferEE14InitTransitionEv), ((unsigned char *)_ZN7CStateTIN3Uec17UecDirectTransferEE11EntryActionEv), ((unsigned char *)_ZN7CStateTIN3Uec17UecDirectTransferEE10ExitActionEv) } };
struct l_unnamed47 _ZTV10CStateBase __ATTRIBUTE_WEAK__ = { { ((unsigned char *)/*NULL*/0), ((unsigned char *)/*NULL*/0), ((unsigned char *)_ZN10CStateBaseD1Ev), ((unsigned char *)_ZN10CStateBaseD0Ev), ((unsigned char *)__cxa_pure_virtual), ((unsigned char *)__cxa_pure_virtual), ((unsigned char *)__cxa_pure_virtual), ((unsigned char *)__cxa_pure_virtual) } };
static unsigned char *_ZZ18__gthread_active_pvE20__gthread_active_ptr = ((unsigned char *)pthread_cancel);
static struct l_unnamed84 _OC_str95 = { "convErrcEstablishmentCause, unexpected p_enum:" };
static struct l_unnamed46 _OC_str96 = { "/home/aherz/Desktop/svn/personal/NSN/UEC_Sources/SC_UEC/SoftInclude/UecUeContextData.hpp" };
static struct l_unnamed93 _OC_str97 = { "Unable to insert to nasDeliveryMap key[%d]" };
static struct l_unnamed5 _OC_str98 = { "UecUeContextData m_uecNasDeliveryMap.insert(key:" };
static struct l_unnamed4 _OC_str99 = { ") n:" };
struct l_unnamed43 _ZTVN3Uec8UecEventE __ATTRIBUTE_WEAK__ = { { ((unsigned char *)/*NULL*/0), ((unsigned char *)/*NULL*/0), ((unsigned char *)_ZN3Uec8UecEventD1Ev), ((unsigned char *)_ZN3Uec8UecEventD0Ev), ((unsigned char *)_ZNK3Uec8UecEvent5CloneEv) } };
struct l_unnamed43 _ZTV6CEvent __ATTRIBUTE_WEAK__ = { { ((unsigned char *)/*NULL*/0), ((unsigned char *)/*NULL*/0), ((unsigned char *)_ZN6CEventD1Ev), ((unsigned char *)_ZN6CEventD0Ev), ((unsigned char *)_ZNK6CEvent5CloneEv) } };
static struct l_unnamed56 _OC_str100 = { "MME connected to UE with ueContextId " };


/* Function Bodies */
static inline int llvm_fcmp_ord(double X, double Y) { return X == X && Y == Y; }
static inline int llvm_fcmp_uno(double X, double Y) { return X != X || Y != Y; }
static inline int llvm_fcmp_ueq(double X, double Y) { return X == Y || llvm_fcmp_uno(X, Y); }
static inline int llvm_fcmp_une(double X, double Y) { return X != Y; }
static inline int llvm_fcmp_ult(double X, double Y) { return X <  Y || llvm_fcmp_uno(X, Y); }
static inline int llvm_fcmp_ugt(double X, double Y) { return X >  Y || llvm_fcmp_uno(X, Y); }
static inline int llvm_fcmp_ule(double X, double Y) { return X <= Y || llvm_fcmp_uno(X, Y); }
static inline int llvm_fcmp_uge(double X, double Y) { return X >= Y || llvm_fcmp_uno(X, Y); }
static inline int llvm_fcmp_oeq(double X, double Y) { return X == Y ; }
static inline int llvm_fcmp_one(double X, double Y) { return X != Y && llvm_fcmp_ord(X, Y); }
static inline int llvm_fcmp_olt(double X, double Y) { return X <  Y ; }
static inline int llvm_fcmp_ogt(double X, double Y) { return X >  Y ; }
static inline int llvm_fcmp_ole(double X, double Y) { return X <= Y ; }
static inline int llvm_fcmp_oge(double X, double Y) { return X >= Y ; }

#line 0 "LLVM INTERNAL"
static void __cxx_global_var_init(void) {
  unsigned int llvm_cbe_tmp__1;

#line 0 "LLVM INTERNAL"
  _ZNSt8ios_base4InitC1Ev((&_ZStL8__ioinit));
#line 0 "LLVM INTERNAL"
  llvm_cbe_tmp__1 = __cxa_atexit(((void  (*) (unsigned char *))_ZNSt8ios_base4InitD1Ev), ((&_ZStL8__ioinit.field0)), ((unsigned char *)(&__dso_handle)));
#line 0 "LLVM INTERNAL"
  return;
}


#line 0 "LLVM INTERNAL"
unsigned int _ZN6Common5Codec17CAsn1Codec_Helper9calcBytesEi(unsigned int llvm_cbe_tmp__2) {
  unsigned int llvm_cbe_retval;    /* Address-exposed local */
  unsigned int llvm_cbe__2e_addr;    /* Address-exposed local */
  unsigned int llvm_cbe_tmp__3;

#line 0 "LLVM INTERNAL"
  *(&llvm_cbe__2e_addr) = llvm_cbe_tmp__2;
#line 12 "CAsn1Codec_Helper.hpp"
  *(&llvm_cbe_retval) = 0u;
#line 13 "CAsn1Codec_Helper.hpp"
  llvm_cbe_tmp__3 = *(&llvm_cbe_retval);
#line 13 "CAsn1Codec_Helper.hpp"
  return llvm_cbe_tmp__3;
}


#line 0 "LLVM INTERNAL"
static void __cxx_global_var_init1(void) {
  struct l_class_OC_BaseEvent llvm_cbe_tmp;    /* Address-exposed local */

#line 0 "LLVM INTERNAL"
  return;
}


#line 0 "LLVM INTERNAL"
void _ZN3Uec17UecDirectTransferC2EPNS_14UecManagerBaseE(struct l_class_OC_Uec_KD__KD_UecDirectTransfer *llvm_cbe_this, struct l_class_OC_Uec_KD__KD_UecManagerBase *llvm_cbe_p_uecManagerBase_ptr) {
  struct l_class_OC_Uec_KD__KD_UecDirectTransfer *llvm_cbe_this_2e_addr;    /* Address-exposed local */
  struct l_class_OC_Uec_KD__KD_UecManagerBase *llvm_cbe_p_uecManagerBase_ptr_2e_addr;    /* Address-exposed local */
  struct l_struct_OC_SS1apUEAggregateMaximumBitrate llvm_cbe_agg_2e_tmp;    /* Address-exposed local */
  struct l_class_OC_std_KD__KD_basic_string llvm_cbe_agg_2e_tmp9;    /* Address-exposed local */
  struct l_class_OC_BaseEvent llvm_cbe_agg_2e_tmp10;    /* Address-exposed local */
  struct l_class_OC_Uec_KD__KD_UecDirectTransfer *llvm_cbe_this1;
  struct l_class_OC_Uec_KD__KD_UecManagerBase *llvm_cbe_tmp;
  struct l_struct_OC_SAsnDynstr *llvm_cbe_tmp3;
  unsigned char *llvm_cbe_call;
  struct l_class_OC_CStateT *llvm_cbe_tmp__4;
  struct l_struct_OC_SS1apUEAggregateMaximumBitrate llvm_cbe_tmp__5;
  struct l_class_OC_CStateT **llvm_cbe_tmp22;
  struct l_class_OC_CStateT *llvm_cbe_tmp23;
  struct l_class_OC_CStateT *llvm_cbe_tmp28;
  struct l_class_OC_Uec_KD__KD_UecManagerBase *llvm_cbe_call31;
  struct l_class_OC_Uec_KD__KD_UecManagerBase *llvm_cbe_call34;
  struct l_class_OC_Uec_KD__KD_UecManagerBase *llvm_cbe_call37;
  struct l_class_OC_Uec_KD__KD_UecManagerBase *llvm_cbe_call40;
  struct l_class_OC_Uec_KD__KD_UecManagerBase *llvm_cbe_call43;

#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_this_2e_addr) = llvm_cbe_this;
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_p_uecManagerBase_ptr_2e_addr) = llvm_cbe_p_uecManagerBase_ptr;
#line 0 "LLVM INTERNAL"
  llvm_cbe_this1 = *(&llvm_cbe_this_2e_addr);
#line 0 "LLVM INTERNAL"
  llvm_cbe_tmp = *(&llvm_cbe_p_uecManagerBase_ptr_2e_addr);
#line 0 "LLVM INTERNAL"
  _ZN3Uec14UecServiceBaseC2EPNS_14UecManagerBaseENS_13EUecServiceIdEPKc((((struct l_class_OC_Uec_KD__KD_UecServiceBase *)llvm_cbe_this1)), llvm_cbe_tmp, 13u, ((&_OC_str.array[((signed int )0u)])));
#line 0 "LLVM INTERNAL"
  *(((unsigned char ***)llvm_cbe_this1)) = ((&_ZTVN3Uec17UecDirectTransferE.array[((signed long long )2ull)]));
#line 0 "LLVM INTERNAL"
  *(((unsigned char ***)((&(((unsigned char *)llvm_cbe_this1))[((signed long long )8ull)])))) = ((&_ZTVN3Uec17UecDirectTransferE.array[((signed long long )13ull)]));
#line 0 "LLVM INTERNAL"
  llvm_cbe_tmp3 = (&llvm_cbe_this1->field2);
#line 39 "UecDirectTransfer.cpp"
  *((&((&llvm_cbe_this1->field2))->field0)) = 0u;
#line 40 "UecDirectTransfer.cpp"
  *((&((&llvm_cbe_this1->field2))->field1)) = ((unsigned char *)/*NULL*/0);
#line 43 "UecDirectTransfer.cpp"
  llvm_cbe_call = _Znwm(104ull);
#line 43 "UecDirectTransfer.cpp"
  llvm_cbe_tmp__4 = ((struct l_class_OC_CStateT *)llvm_cbe_call);
#line 43 "UecDirectTransfer.cpp"
  *((&llvm_cbe_agg_2e_tmp.field0)) = ((unsigned long long )(unsigned long)_ZN3Uec17UecDirectTransfer9StateIdleEP6CEvent);
#line 43 "UecDirectTransfer.cpp"
  *((&llvm_cbe_agg_2e_tmp.field1)) = 0ull;
#line 43 "UecDirectTransfer.cpp"
  _ZNSaIcEC1Ev((&llvm_cbe_agg_2e_tmp10));
#line 43 "UecDirectTransfer.cpp"
  _ZNSsC1EPKcRKSaIcE((&llvm_cbe_agg_2e_tmp9), ((&_OC_str2.array[((signed int )0u)])), (&llvm_cbe_agg_2e_tmp10));
#line 43 "UecDirectTransfer.cpp"
  llvm_cbe_tmp__5 = ((struct __attribute__ ((packed, aligned(1))) {struct l_struct_OC_SS1apUEAggregateMaximumBitrate data; } *)(&llvm_cbe_agg_2e_tmp))->data;
#line 43 "UecDirectTransfer.cpp"
  _ZN7CStateTIN3Uec17UecDirectTransferEEC1EPS1_MS1_FjP6CEventESs(llvm_cbe_tmp__4, llvm_cbe_this1, llvm_cbe_tmp__5, (&llvm_cbe_agg_2e_tmp9));
#line 43 "UecDirectTransfer.cpp"
  llvm_cbe_tmp22 = (&llvm_cbe_this1->field1);
#line 43 "UecDirectTransfer.cpp"
  *llvm_cbe_tmp22 = llvm_cbe_tmp__4;
#line 43 "UecDirectTransfer.cpp"
  llvm_cbe_tmp23 = *llvm_cbe_tmp22;
#line 43 "UecDirectTransfer.cpp"
  _ZNSsD1Ev((&llvm_cbe_agg_2e_tmp9));
#line 43 "UecDirectTransfer.cpp"
  _ZNSaIcED1Ev((&llvm_cbe_agg_2e_tmp10));
#line 46 "UecDirectTransfer.cpp"
  llvm_cbe_tmp28 = *((&llvm_cbe_this1->field1));
#line 46 "UecDirectTransfer.cpp"
  _ZN6Common8CFsmBase15SetInitialStateEP10CStateBase((((struct l_class_OC_Common_KD__KD_CFsmBase *)((&(((unsigned char *)llvm_cbe_this1))[((signed long long )8ull)])))), (((struct l_class_OC_CStateBase *)llvm_cbe_tmp28)));
#line 48 "UecDirectTransfer.cpp"
  llvm_cbe_call31 = _ZNK3Uec14UecServiceBase13getUecManagerEv((((struct l_class_OC_Uec_KD__KD_UecServiceBase *)llvm_cbe_this1)));
#line 48 "UecDirectTransfer.cpp"
  _ZN3Uec14UecManagerBase17registerBehaviourEijNS_13EUecServiceIdENS_12EUecActionIdES1_(llvm_cbe_call31, 0u, 9038u, 1u, 1u, 13u);
#line 55 "UecDirectTransfer.cpp"
  llvm_cbe_call34 = _ZNK3Uec14UecServiceBase13getUecManagerEv((((struct l_class_OC_Uec_KD__KD_UecServiceBase *)llvm_cbe_this1)));
#line 55 "UecDirectTransfer.cpp"
  _ZN3Uec14UecManagerBase17registerBehaviourEijNS_13EUecServiceIdENS_12EUecActionIdES1_(llvm_cbe_call34, 1u, 10205u, 1u, 1u, 13u);
#line 64 "UecDirectTransfer.cpp"
  llvm_cbe_call37 = _ZNK3Uec14UecServiceBase13getUecManagerEv((((struct l_class_OC_Uec_KD__KD_UecServiceBase *)llvm_cbe_this1)));
#line 64 "UecDirectTransfer.cpp"
  _ZN3Uec14UecManagerBase17registerBehaviourEijNS_13EUecServiceIdENS_12EUecActionIdES1_(llvm_cbe_call37, 1u, 10205u, 28u, 4u, 13u);
#line 72 "UecDirectTransfer.cpp"
  llvm_cbe_call40 = _ZNK3Uec14UecServiceBase13getUecManagerEv((((struct l_class_OC_Uec_KD__KD_UecServiceBase *)llvm_cbe_this1)));
#line 72 "UecDirectTransfer.cpp"
  _ZN3Uec14UecManagerBase17registerBehaviourEijNS_13EUecServiceIdENS_12EUecActionIdES1_(llvm_cbe_call40, 2u, 9040u, 1u, 1u, 13u);
#line 79 "UecDirectTransfer.cpp"
  llvm_cbe_call43 = _ZNK3Uec14UecServiceBase13getUecManagerEv((((struct l_class_OC_Uec_KD__KD_UecServiceBase *)llvm_cbe_this1)));
#line 79 "UecDirectTransfer.cpp"
  _ZN3Uec14UecManagerBase17registerBehaviourEijNS_13EUecServiceIdENS_12EUecActionIdES1_(llvm_cbe_call43, 0u, 12672u, 1u, 1u, 13u);
#line 85 "UecDirectTransfer.cpp"
  return;
}


#line 0 "LLVM INTERNAL"
void _ZN7CStateTIN3Uec17UecDirectTransferEEC1EPS1_MS1_FjP6CEventESs(struct l_class_OC_CStateT *llvm_cbe_this, struct l_class_OC_Uec_KD__KD_UecDirectTransfer *llvm_cbe_itsFsmPtr, struct l_struct_OC_SS1apUEAggregateMaximumBitrate llvm_cbe_tmp__6, struct l_class_OC_std_KD__KD_basic_string *llvm_cbe_name) {
  struct l_class_OC_CStateT *llvm_cbe_this_2e_addr;    /* Address-exposed local */
  struct l_class_OC_Uec_KD__KD_UecDirectTransfer *llvm_cbe_itsFsmPtr_2e_addr;    /* Address-exposed local */
  struct l_struct_OC_SS1apUEAggregateMaximumBitrate llvm_cbe_stateFuncPtr;    /* Address-exposed local */
  struct l_class_OC_CStateT *llvm_cbe_this1;
  struct l_class_OC_Uec_KD__KD_UecDirectTransfer *llvm_cbe_tmp;
  struct l_struct_OC_SS1apUEAggregateMaximumBitrate llvm_cbe_tmp__7;

#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_this_2e_addr) = llvm_cbe_this;
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_itsFsmPtr_2e_addr) = llvm_cbe_itsFsmPtr;
#line 0 "LLVM INTERNAL"
  ((struct __attribute__ ((packed, aligned(1))) {struct l_struct_OC_SS1apUEAggregateMaximumBitrate data; } *)(&llvm_cbe_stateFuncPtr))->data = llvm_cbe_tmp__6;
#line 0 "LLVM INTERNAL"
  llvm_cbe_this1 = *(&llvm_cbe_this_2e_addr);
#line 0 "LLVM INTERNAL"
  llvm_cbe_tmp = *(&llvm_cbe_itsFsmPtr_2e_addr);
#line 0 "LLVM INTERNAL"
  llvm_cbe_tmp__7 = ((struct __attribute__ ((packed, aligned(1))) {struct l_struct_OC_SS1apUEAggregateMaximumBitrate data; } *)(&llvm_cbe_stateFuncPtr))->data;
#line 0 "LLVM INTERNAL"
  _ZN7CStateTIN3Uec17UecDirectTransferEEC2EPS1_MS1_FjP6CEventESs(llvm_cbe_this1, llvm_cbe_tmp, llvm_cbe_tmp__7, llvm_cbe_name);
#line 73 "CStateT.hpp"
  return;
}


void _ZNSaIcEC1Ev(struct l_class_OC_BaseEvent *llvm_cbe_this);

void _ZN3Uec27UEC_CTY_TupuProceedLockStepC1Ev(struct l_struct_OC_SAntennaInfo *llvm_cbe_this);

#line 0 "LLVM INTERNAL"
unsigned int _ZN3Uec17UecDirectTransfer9StateIdleEP6CEvent(struct l_class_OC_Uec_KD__KD_UecDirectTransfer *llvm_cbe_this, struct l_class_OC_CEvent *llvm_cbe_p_cEvent_ptr) {
  unsigned int llvm_cbe_retval;    /* Address-exposed local */
  struct l_class_OC_Uec_KD__KD_UecDirectTransfer *llvm_cbe_this_2e_addr;    /* Address-exposed local */
  struct l_class_OC_CEvent *llvm_cbe_p_cEvent_ptr_2e_addr;    /* Address-exposed local */
  struct l_class_OC_std_KD__KD_basic_string llvm_cbe__currentFunction_;    /* Address-exposed local */
  struct l_class_OC_BaseEvent llvm_cbe_agg_2e_tmp;    /* Address-exposed local */
  struct l_class_OC_Uec_KD__KD_UecDebugCallstack llvm_cbe__callstackTmp_;    /* Address-exposed local */
  unsigned int llvm_cbe_l_success;    /* Address-exposed local */
  struct l_class_OC_Uec_KD__KD_UecEvent *llvm_cbe_l_uecEvent_ptr;    /* Address-exposed local */
  struct l_class_OC_Uec_KD__KD_UecUeContextData *llvm_cbe_l_uecUeContextDataWrite_ptr;    /* Address-exposed local */
  struct l_struct_OC_SAntennaInfo llvm_cbe_l_payLoad;    /* Address-exposed local */
  struct l_class_OC_Uec_KD__KD_UecEvent llvm_cbe_l_uecEvent;    /* Address-exposed local */
  struct l_struct_OC_Uec_KD__KD_UEC_DTF_DirectTransfer *llvm_cbe_l_payLoad_ptr;    /* Address-exposed local */
  unsigned int llvm_cbe_cleanup_2e_dst;    /* Address-exposed local */
  unsigned int llvm_cbe_cleanup_2e_dst563;    /* Address-exposed local */
  struct l_class_OC_Uec_KD__KD_UecDirectTransfer *llvm_cbe_this1;
  struct l_class_OC_CEvent *llvm_cbe_tmp;
  struct l_class_OC_BaseEvent *llvm_cbe_call;
  struct l_class_OC_BaseEvent *llvm_cbe_call15;
  struct l_class_OC_Uec_KD__KD_UecEvent *llvm_cbe_tmp16;
  unsigned int llvm_cbe_call18;
  struct l_class_OC_BaseEvent *llvm_cbe_call20;
  struct l_class_OC_BaseEvent *llvm_cbe_call22;
  struct l_class_OC_BaseEvent *llvm_cbe_call24;
  struct l_class_OC_Uec_KD__KD_UecEvent *llvm_cbe_tmp25;
  unsigned int llvm_cbe_call27;
  struct l_class_OC_BaseEvent *llvm_cbe_call29;
  struct l_class_OC_BaseEvent *llvm_cbe_call31;
  struct l_class_OC_BaseEvent *llvm_cbe_call33;
  struct l_class_OC_Uec_KD__KD_UecEvent *llvm_cbe_tmp34;
  unsigned int llvm_cbe_call36;
  struct l_class_OC_BaseEvent *llvm_cbe_call38;
  struct l_class_OC_Uec_KD__KD_UecBaseData *llvm_cbe_call41;
  struct l_class_OC_Uec_KD__KD_UecBaseData * (**llvm_cbe_tmp__8) (struct l_class_OC_Uec_KD__KD_UecBaseData *, unsigned int );
  struct l_class_OC_Uec_KD__KD_UecBaseData * (*llvm_cbe_tmp__9) (struct l_class_OC_Uec_KD__KD_UecBaseData *, unsigned int );
  struct l_class_OC_Uec_KD__KD_UecBaseData *llvm_cbe_call43;
  struct l_class_OC_Uec_KD__KD_UecUeContextData *llvm_cbe_tmp44;
  struct l_class_OC_BaseEvent *llvm_cbe_call46;
  struct l_class_OC_BaseEvent *llvm_cbe_call48;
  struct l_class_OC_Uec_KD__KD_UecEvent *llvm_cbe_tmp49;
  unsigned int llvm_cbe_call51;
  struct l_class_OC_BaseEvent *llvm_cbe_call53;
  struct l_class_OC_Uec_KD__KD_UecEvent *llvm_cbe_tmp55;
  unsigned int llvm_cbe_call57;
  struct l_class_OC_Uec_KD__KD_UecEvent *llvm_cbe_tmp58;
  unsigned char *llvm_cbe_call60;
  struct l_class_OC_Uec_KD__KD_UecEvent *llvm_cbe_tmp61;
  unsigned int llvm_cbe_call63;
  struct l_class_OC_BaseEvent *llvm_cbe_call67;
  struct l_class_OC_BaseEvent *llvm_cbe_call69;
  struct l_class_OC_Uec_KD__KD_UecEvent *llvm_cbe_tmp70;
  unsigned int llvm_cbe_call72;
  struct l_class_OC_BaseEvent *llvm_cbe_call74;
  struct l_class_OC_BaseEvent *llvm_cbe_call76;
  struct l_class_OC_BaseEvent *llvm_cbe_call78;
  struct l_class_OC_Uec_KD__KD_UecEvent *llvm_cbe_tmp79;
  unsigned char *llvm_cbe_call81;
  struct l_class_OC_BaseEvent *llvm_cbe_call83;
  struct l_class_OC_BaseEvent *llvm_cbe_call85;
  struct l_class_OC_BaseEvent *llvm_cbe_call87;
  struct l_class_OC_Uec_KD__KD_UecEvent *llvm_cbe_tmp88;
  unsigned int llvm_cbe_call90;
  struct l_class_OC_BaseEvent *llvm_cbe_call92;
  struct l_class_OC_Uec_KD__KD_UecEvent *llvm_cbe_tmp95;
  unsigned int llvm_cbe_call97;
  unsigned int llvm_cbe_tmp98;
  unsigned int llvm_cbe_tmp101;
  struct l_class_OC_Uec_KD__KD_UecEvent *llvm_cbe_tmp104;
  unsigned int llvm_cbe_call106;
  struct l_class_OC_BaseEvent *llvm_cbe_call109;
  struct l_class_OC_BaseEvent *llvm_cbe_call111;
  struct l_class_OC_Uec_KD__KD_UecEvent *llvm_cbe_tmp112;
  unsigned int llvm_cbe_call114;
  struct l_class_OC_BaseEvent *llvm_cbe_call116;
  struct l_class_OC_BaseEvent *llvm_cbe_call118;
  struct l_class_OC_Uec_KD__KD_UecUeContextData *llvm_cbe_tmp119;
  unsigned int llvm_cbe_call121;
  struct l_class_OC_Uec_KD__KD_UecManagerBase *llvm_cbe_call128;
  unsigned int llvm_cbe_call130;
  struct l_class_OC_Uec_KD__KD_UecManagerBase *llvm_cbe_call138;
  struct l_class_OC_Uec_KD__KD_UecBaseData * (**llvm_cbe_tmp__10) (struct l_class_OC_Uec_KD__KD_UecManagerBase *, unsigned int );
  struct l_class_OC_Uec_KD__KD_UecBaseData * (*llvm_cbe_tmp__11) (struct l_class_OC_Uec_KD__KD_UecManagerBase *, unsigned int );
  struct l_class_OC_Uec_KD__KD_UecBaseData *llvm_cbe_call141;
  unsigned int  (**llvm_cbe_tmp__12) (struct l_class_OC_Uec_KD__KD_UecBaseData *, struct l_class_OC_Uec_KD__KD_UecEvent *);
  unsigned int  (*llvm_cbe_tmp__13) (struct l_class_OC_Uec_KD__KD_UecBaseData *, struct l_class_OC_Uec_KD__KD_UecEvent *);
  unsigned int llvm_cbe_call144;
  struct l_class_OC_BaseEvent *llvm_cbe_call148;
  struct l_class_OC_BaseEvent *llvm_cbe_call150;
  struct l_class_OC_Uec_KD__KD_UecManagerBase *llvm_cbe_call152;
  unsigned int llvm_cbe_call154;
  struct l_class_OC_BaseEvent *llvm_cbe_call156;
  struct l_class_OC_BaseEvent *llvm_cbe_call158;
  struct l_class_OC_Uec_KD__KD_UecEvent *llvm_cbe_tmp161;
  unsigned int llvm_cbe_call163;
  unsigned int llvm_cbe_tmp164;
  unsigned int llvm_cbe_tmp167;
  struct l_class_OC_Uec_KD__KD_UecEvent *llvm_cbe_tmp170;
  unsigned int llvm_cbe_call172;
  struct l_class_OC_BaseEvent *llvm_cbe_call183;
  struct l_class_OC_BaseEvent *llvm_cbe_call185;
  struct l_class_OC_Uec_KD__KD_UecEvent *llvm_cbe_tmp186;
  unsigned int llvm_cbe_call188;
  struct l_class_OC_BaseEvent *llvm_cbe_call190;
  struct l_class_OC_BaseEvent *llvm_cbe_call192;
  struct l_class_OC_BaseEvent *llvm_cbe_call194;
  struct l_class_OC_Uec_KD__KD_UecEvent *llvm_cbe_tmp195;
  unsigned int llvm_cbe_call197;
  struct l_class_OC_BaseEvent *llvm_cbe_call199;
  struct l_class_OC_Uec_KD__KD_UecEvent *llvm_cbe_tmp202;
  unsigned char *llvm_cbe_call204;
  struct l_class_OC_Uec_KD__KD_UecEvent *llvm_cbe_tmp207;
  unsigned int llvm_cbe_call209;
  struct l_class_OC_BaseEvent *llvm_cbe_call213;
  struct l_class_OC_BaseEvent *llvm_cbe_call215;
  struct l_class_OC_Uec_KD__KD_UecEvent *llvm_cbe_tmp216;
  unsigned int llvm_cbe_call218;
  struct l_class_OC_BaseEvent *llvm_cbe_call220;
  struct l_class_OC_BaseEvent *llvm_cbe_call222;
  struct l_class_OC_BaseEvent *llvm_cbe_call224;
  struct l_class_OC_Uec_KD__KD_UecEvent *llvm_cbe_tmp225;
  unsigned char *llvm_cbe_call227;
  struct l_class_OC_BaseEvent *llvm_cbe_call229;
  struct l_class_OC_BaseEvent *llvm_cbe_call231;
  struct l_class_OC_BaseEvent *llvm_cbe_call233;
  struct l_class_OC_Uec_KD__KD_UecEvent *llvm_cbe_tmp234;
  unsigned int llvm_cbe_call236;
  struct l_class_OC_BaseEvent *llvm_cbe_call238;
  struct l_class_OC_Uec_KD__KD_UecEvent *llvm_cbe_tmp241;
  unsigned int llvm_cbe_call243;
  unsigned int llvm_cbe_tmp244;
  unsigned int llvm_cbe_tmp247;
  struct l_class_OC_Uec_KD__KD_UecEvent *llvm_cbe_tmp250;
  unsigned int llvm_cbe_call252;
  struct l_class_OC_BaseEvent *llvm_cbe_call255;
  struct l_class_OC_BaseEvent *llvm_cbe_call257;
  struct l_class_OC_Uec_KD__KD_UecEvent *llvm_cbe_tmp258;
  unsigned int llvm_cbe_call260;
  struct l_class_OC_BaseEvent *llvm_cbe_call262;
  struct l_class_OC_BaseEvent *llvm_cbe_call264;
  struct l_class_OC_BaseEvent *llvm_cbe_call266;
  struct l_class_OC_Uec_KD__KD_UecUeContextData *llvm_cbe_tmp267;
  unsigned int llvm_cbe_call269;
  struct l_class_OC_BaseEvent *llvm_cbe_call271;
  struct l_class_OC_BaseEvent *llvm_cbe_call273;
  struct l_class_OC_Uec_KD__KD_UecEvent *llvm_cbe_tmp274;
  unsigned int llvm_cbe_call276;
  unsigned int llvm_cbe_tmp277;
  unsigned int llvm_cbe_tmp280;
  struct l_class_OC_Uec_KD__KD_UecUeContextData *llvm_cbe_tmp283;
  unsigned int llvm_cbe_call285;
  struct l_class_OC_Uec_KD__KD_UecEvent *llvm_cbe_tmp287;
  unsigned int llvm_cbe_call289;
  struct l_class_OC_Uec_KD__KD_UecEvent *llvm_cbe_tmp291;
  unsigned int llvm_cbe_call293;
  struct l_class_OC_Uec_KD__KD_UecEvent *llvm_cbe_tmp295;
  unsigned int llvm_cbe_call297;
  struct l_class_OC_BaseEvent *llvm_cbe_call300;
  struct l_class_OC_BaseEvent *llvm_cbe_call302;
  struct l_class_OC_Uec_KD__KD_UecEvent *llvm_cbe_tmp303;
  unsigned int llvm_cbe_call305;
  struct l_class_OC_BaseEvent *llvm_cbe_call307;
  struct l_class_OC_BaseEvent *llvm_cbe_call309;
  struct l_class_OC_BaseEvent *llvm_cbe_call311;
  struct l_class_OC_Uec_KD__KD_UecUeContextData *llvm_cbe_tmp312;
  unsigned int llvm_cbe_call314;
  struct l_class_OC_BaseEvent *llvm_cbe_call316;
  struct l_class_OC_BaseEvent *llvm_cbe_call318;
  struct l_class_OC_BaseEvent *llvm_cbe_call332;
  struct l_class_OC_BaseEvent *llvm_cbe_call334;
  struct l_class_OC_Uec_KD__KD_UecEvent *llvm_cbe_tmp335;
  unsigned int llvm_cbe_call337;
  struct l_class_OC_BaseEvent *llvm_cbe_call339;
  struct l_class_OC_BaseEvent *llvm_cbe_call341;
  struct l_class_OC_BaseEvent *llvm_cbe_call343;
  struct l_class_OC_Uec_KD__KD_UecEvent *llvm_cbe_tmp344;
  unsigned int llvm_cbe_call346;
  struct l_class_OC_BaseEvent *llvm_cbe_call348;
  struct l_class_OC_Uec_KD__KD_UecEvent *llvm_cbe_tmp352;
  unsigned int llvm_cbe_call354;
  unsigned int llvm_cbe_tmp355;
  unsigned int llvm_cbe_tmp358;
  struct l_class_OC_Uec_KD__KD_UecEvent *llvm_cbe_tmp361;
  unsigned int llvm_cbe_call363;
  struct l_class_OC_BaseEvent *llvm_cbe_call366;
  struct l_class_OC_BaseEvent *llvm_cbe_call368;
  struct l_class_OC_Uec_KD__KD_UecEvent *llvm_cbe_tmp369;
  unsigned int llvm_cbe_call371;
  struct l_class_OC_BaseEvent *llvm_cbe_call373;
  struct l_class_OC_BaseEvent *llvm_cbe_call375;
  struct l_class_OC_Uec_KD__KD_UecEvent *llvm_cbe_tmp376;
  unsigned int llvm_cbe_call378;
  unsigned int llvm_cbe_tmp379;
  unsigned int llvm_cbe_tmp382;
  struct l_class_OC_Uec_KD__KD_UecEvent *llvm_cbe_tmp385;
  unsigned int llvm_cbe_call387;
  struct l_class_OC_BaseEvent *llvm_cbe_call399;
  struct l_class_OC_BaseEvent *llvm_cbe_call401;
  struct l_class_OC_Uec_KD__KD_UecEvent *llvm_cbe_tmp402;
  unsigned int llvm_cbe_call404;
  struct l_class_OC_BaseEvent *llvm_cbe_call406;
  struct l_class_OC_BaseEvent *llvm_cbe_call408;
  struct l_class_OC_BaseEvent *llvm_cbe_call410;
  struct l_class_OC_Uec_KD__KD_UecEvent *llvm_cbe_tmp411;
  unsigned int llvm_cbe_call413;
  struct l_class_OC_BaseEvent *llvm_cbe_call415;
  struct l_class_OC_Uec_KD__KD_UecEvent *llvm_cbe_tmp420;
  unsigned char *llvm_cbe_call422;
  struct l_class_OC_BaseEvent *llvm_cbe_call424;
  struct l_class_OC_BaseEvent *llvm_cbe_call426;
  struct l_class_OC_Uec_KD__KD_UecEvent *llvm_cbe_tmp427;
  unsigned int llvm_cbe_call429;
  struct l_class_OC_BaseEvent *llvm_cbe_call431;
  struct l_class_OC_BaseEvent *llvm_cbe_call433;
  struct l_class_OC_BaseEvent *llvm_cbe_call435;
  struct l_struct_OC_Uec_KD__KD_UEC_DTF_DirectTransfer *llvm_cbe_tmp436;
  unsigned int llvm_cbe_tmp438;
  struct l_class_OC_BaseEvent *llvm_cbe_call440;
  struct l_class_OC_BaseEvent *llvm_cbe_call442;
  struct l_class_OC_BaseEvent *llvm_cbe_call444;
  struct l_struct_OC_Uec_KD__KD_UEC_DTF_DirectTransfer *llvm_cbe_tmp445;
  unsigned int llvm_cbe_tmp448;
  struct l_class_OC_BaseEvent *llvm_cbe_call450;
  struct l_class_OC_Uec_KD__KD_UecEvent *llvm_cbe_tmp451;
  unsigned int llvm_cbe_call453;
  unsigned int llvm_cbe_tmp454;
  struct l_struct_OC_Uec_KD__KD_UEC_DTF_DirectTransfer *llvm_cbe_tmp457;
  unsigned int llvm_cbe_tmp459;
  struct l_class_OC_Uec_KD__KD_UecEvent *llvm_cbe_tmp461;
  unsigned int llvm_cbe_call463;
  struct l_class_OC_Uec_KD__KD_UecEvent *llvm_cbe_tmp465;
  unsigned int llvm_cbe_call467;
  struct l_class_OC_BaseEvent *llvm_cbe_call470;
  struct l_class_OC_BaseEvent *llvm_cbe_call472;
  struct l_class_OC_Uec_KD__KD_UecEvent *llvm_cbe_tmp473;
  unsigned int llvm_cbe_call475;
  struct l_class_OC_BaseEvent *llvm_cbe_call477;
  struct l_class_OC_BaseEvent *llvm_cbe_call479;
  struct l_class_OC_BaseEvent *llvm_cbe_call481;
  struct l_struct_OC_Uec_KD__KD_UEC_DTF_DirectTransfer *llvm_cbe_tmp482;
  unsigned int llvm_cbe_tmp484;
  struct l_class_OC_BaseEvent *llvm_cbe_call486;
  struct l_struct_OC_Uec_KD__KD_UEC_DTF_DirectTransfer *llvm_cbe_tmp489;
  unsigned char llvm_cbe_tmp491;
  struct l_struct_OC_Uec_KD__KD_UEC_DTF_DirectTransfer *llvm_cbe_tmp494;
  unsigned char *llvm_cbe_tmp497;
  struct l_struct_OC_Uec_KD__KD_UEC_DTF_DirectTransfer *llvm_cbe_tmp500;
  unsigned char *llvm_cbe_tmp503;
  unsigned int llvm_cbe_tmp506;
  struct l_class_OC_BaseEvent *llvm_cbe_call510;
  struct l_class_OC_BaseEvent *llvm_cbe_call512;
  struct l_class_OC_Uec_KD__KD_UecEvent *llvm_cbe_tmp513;
  unsigned int llvm_cbe_call515;
  struct l_class_OC_BaseEvent *llvm_cbe_call517;
  struct l_class_OC_BaseEvent *llvm_cbe_call519;
  struct l_class_OC_BaseEvent *llvm_cbe_call521;
  struct l_class_OC_Uec_KD__KD_UecEvent *llvm_cbe_tmp522;
  unsigned int llvm_cbe_call524;
  struct l_class_OC_BaseEvent *llvm_cbe_call526;
  unsigned char *llvm_cbe_tmp531;
  unsigned char *llvm_cbe_tmp536;
  unsigned char *llvm_cbe_tmp__14;
  unsigned int llvm_cbe_tmp544;
  unsigned int llvm_cbe_tmp561;
  unsigned int llvm_cbe_tmp565;
  unsigned int llvm_cbe_tmp__15;

#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_this_2e_addr) = llvm_cbe_this;
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_p_cEvent_ptr_2e_addr) = llvm_cbe_p_cEvent_ptr;
#line 0 "LLVM INTERNAL"
  llvm_cbe_this1 = *(&llvm_cbe_this_2e_addr);
#line 1128 "UecDirectTransfer.cpp"
  _ZNSaIcEC1Ev((&llvm_cbe_agg_2e_tmp));
#line 1128 "UecDirectTransfer.cpp"
  _ZNSsC1EPKcRKSaIcE((&llvm_cbe__currentFunction_), ((&__PRETTY_FUNCTION___OC__ZN3Uec17UecDirectTransfer9StateIdleEP6CEvent.array[((signed int )0u)])), (&llvm_cbe_agg_2e_tmp));
#line 1128 "UecDirectTransfer.cpp"
  _ZNSaIcED1Ev((&llvm_cbe_agg_2e_tmp));
#line 1128 "UecDirectTransfer.cpp"
  _ZN3Uec17UecDebugCallstackC1ERKSs((&llvm_cbe__callstackTmp_), (&llvm_cbe__currentFunction_));
#line 1130 "UecDirectTransfer.cpp"
  *(&llvm_cbe_l_success) = 0u;
#line 1132 "UecDirectTransfer.cpp"
  llvm_cbe_tmp = *(&llvm_cbe_p_cEvent_ptr_2e_addr);
#line 1132 "UecDirectTransfer.cpp"
  *(&llvm_cbe_l_uecEvent_ptr) = (((struct l_class_OC_Uec_KD__KD_UecEvent *)llvm_cbe_tmp));
#line 1134 "UecDirectTransfer.cpp"
  llvm_cbe_call = _ZN11DummyStreamlsIPKcEERS_T_((&mout), ((&_OC_str81.array[((signed int )0u)])));
#line 1134 "UecDirectTransfer.cpp"
  llvm_cbe_call15 = _ZN11DummyStreamlsIPFRSt8ios_baseS2_EEERS_T_(llvm_cbe_call, _ZSt3decRSt8ios_base);
#line 1134 "UecDirectTransfer.cpp"
  llvm_cbe_tmp16 = *(&llvm_cbe_l_uecEvent_ptr);
#line 1134 "UecDirectTransfer.cpp"
  llvm_cbe_call18 = _ZNK3Uec8UecEvent13getInstanceIdEv(llvm_cbe_tmp16);
#line 1134 "UecDirectTransfer.cpp"
  llvm_cbe_call20 = _ZN11DummyStreamlsIjEERS_T_(llvm_cbe_call15, llvm_cbe_call18);
#line 1134 "UecDirectTransfer.cpp"
  llvm_cbe_call22 = _ZN11DummyStreamlsIPKcEERS_T_(llvm_cbe_call20, ((&_OC_str6.array[((signed int )0u)])));
#line 1134 "UecDirectTransfer.cpp"
  llvm_cbe_call24 = _ZN11DummyStreamlsIPFRSt8ios_baseS2_EEERS_T_(llvm_cbe_call22, _ZSt3hexRSt8ios_base);
#line 1134 "UecDirectTransfer.cpp"
  llvm_cbe_tmp25 = *(&llvm_cbe_l_uecEvent_ptr);
#line 1134 "UecDirectTransfer.cpp"
  llvm_cbe_call27 = _ZNK6CEvent10GetEventIdEv((((struct l_class_OC_CEvent *)llvm_cbe_tmp25)));
#line 1134 "UecDirectTransfer.cpp"
  llvm_cbe_call29 = _ZN11DummyStreamlsIjEERS_T_(llvm_cbe_call24, llvm_cbe_call27);
#line 1134 "UecDirectTransfer.cpp"
  llvm_cbe_call31 = _ZN11DummyStreamlsIPKcEERS_T_(llvm_cbe_call29, ((&_OC_str82.array[((signed int )0u)])));
#line 1134 "UecDirectTransfer.cpp"
  llvm_cbe_call33 = _ZN11DummyStreamlsIPFRSt8ios_baseS2_EEERS_T_(llvm_cbe_call31, _ZSt3hexRSt8ios_base);
#line 1134 "UecDirectTransfer.cpp"
  llvm_cbe_tmp34 = *(&llvm_cbe_l_uecEvent_ptr);
#line 1134 "UecDirectTransfer.cpp"
  llvm_cbe_call36 = _ZNK3Uec8UecEvent16getAsn1PayLoadIdEv(llvm_cbe_tmp34);
#line 1134 "UecDirectTransfer.cpp"
  llvm_cbe_call38 = _ZN11DummyStreamlsIiEERS_T_(llvm_cbe_call33, llvm_cbe_call36);
#line 1139 "UecDirectTransfer.cpp"
  llvm_cbe_call41 = _ZNK3Uec14UecServiceBase16getUecDataAccessEv((((struct l_class_OC_Uec_KD__KD_UecServiceBase *)llvm_cbe_this1)));
#line 1139 "UecDirectTransfer.cpp"
  llvm_cbe_tmp__8 = *(((struct l_class_OC_Uec_KD__KD_UecBaseData * (***) (struct l_class_OC_Uec_KD__KD_UecBaseData *, unsigned int ))llvm_cbe_call41));
#line 1139 "UecDirectTransfer.cpp"
  llvm_cbe_tmp__9 = *((&llvm_cbe_tmp__8[((signed long long )2ull)]));
#line 1139 "UecDirectTransfer.cpp"
  llvm_cbe_call43 = llvm_cbe_tmp__9(llvm_cbe_call41, 1u);
#line 1139 "UecDirectTransfer.cpp"
  *(&llvm_cbe_l_uecUeContextDataWrite_ptr) = (((struct l_class_OC_Uec_KD__KD_UecUeContextData *)llvm_cbe_call43));
#line 1141 "UecDirectTransfer.cpp"
  llvm_cbe_tmp44 = *(&llvm_cbe_l_uecUeContextDataWrite_ptr);
#line 1141 "UecDirectTransfer.cpp"
  if ((llvm_cbe_tmp44 != ((struct l_class_OC_Uec_KD__KD_UecUeContextData *)/*NULL*/0))) {    goto llvm_cbe_if_2e_end;  } else {    goto llvm_cbe_if_2e_then;  }


llvm_cbe_if_2e_then:
#line 1143 "UecDirectTransfer.cpp"
  llvm_cbe_call46 = _ZN11DummyStreamlsIPKcEERS_T_((&mout), ((&_OC_str81.array[((signed int )0u)])));
#line 1143 "UecDirectTransfer.cpp"
  llvm_cbe_call48 = _ZN11DummyStreamlsIPFRSt8ios_baseS2_EEERS_T_(llvm_cbe_call46, _ZSt3decRSt8ios_base);
#line 1143 "UecDirectTransfer.cpp"
  llvm_cbe_tmp49 = *(&llvm_cbe_l_uecEvent_ptr);
#line 1143 "UecDirectTransfer.cpp"
  llvm_cbe_call51 = _ZNK3Uec8UecEvent13getInstanceIdEv(llvm_cbe_tmp49);
#line 1143 "UecDirectTransfer.cpp"
  llvm_cbe_call53 = _ZN11DummyStreamlsIjEERS_T_(llvm_cbe_call48, llvm_cbe_call51);
#line 1146 "UecDirectTransfer.cpp"
  _ZN3Uec14UecServiceBase15serviceFinishedEiNS_13EUecServiceIdE((((struct l_class_OC_Uec_KD__KD_UecServiceBase *)llvm_cbe_this1)), 0u, 0u);
#line 1147 "UecDirectTransfer.cpp"
  *(&llvm_cbe_retval) = 1u;
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_cleanup_2e_dst) = 1u;
#line 1147 "UecDirectTransfer.cpp"
  goto llvm_cbe_cleanup;

llvm_cbe_if_2e_end:
#line 1150 "UecDirectTransfer.cpp"
  llvm_cbe_tmp55 = *(&llvm_cbe_l_uecEvent_ptr);
#line 1150 "UecDirectTransfer.cpp"
  llvm_cbe_call57 = _ZNK6CEvent10GetEventIdEv((((struct l_class_OC_CEvent *)llvm_cbe_tmp55)));
#line 1150 "UecDirectTransfer.cpp"
  switch (llvm_cbe_call57) {
  default:
    goto llvm_cbe_sw_2e_default508;;
  case 9038u:
    goto llvm_cbe_sw_2e_bb;    break;
  case 10205u:
    goto llvm_cbe_sw_2e_bb201;  case 9040u:
    goto llvm_cbe_sw_2e_bb351;  case 12672u:
    goto llvm_cbe_sw_2e_bb418;  }

llvm_cbe_sw_2e_bb:
#line 1154 "UecDirectTransfer.cpp"
  llvm_cbe_tmp58 = *(&llvm_cbe_l_uecEvent_ptr);
#line 1154 "UecDirectTransfer.cpp"
  llvm_cbe_call60 = _ZNK3Uec8UecEvent14getAsn1PayLoadEv(llvm_cbe_tmp58);
#line 1154 "UecDirectTransfer.cpp"
  if ((llvm_cbe_call60 == ((unsigned char *)/*NULL*/0))) {    goto llvm_cbe_if_2e_then65;  } else {    goto llvm_cbe_lor_2e_lhs_2e_false;  }


llvm_cbe_lor_2e_lhs_2e_false:
#line 1154 "UecDirectTransfer.cpp"
  llvm_cbe_tmp61 = *(&llvm_cbe_l_uecEvent_ptr);
#line 1154 "UecDirectTransfer.cpp"
  llvm_cbe_call63 = _ZNK3Uec8UecEvent16getAsn1PayLoadIdEv(llvm_cbe_tmp61);
#line 1154 "UecDirectTransfer.cpp"
  if ((llvm_cbe_call63 == 0u)) {    goto llvm_cbe_if_2e_then65;  } else {    goto llvm_cbe_if_2e_end94;  }


llvm_cbe_if_2e_then65:
#line 1158 "UecDirectTransfer.cpp"
  llvm_cbe_call67 = _ZN11DummyStreamlsIPKcEERS_T_((&mout), ((&_OC_str81.array[((signed int )0u)])));
#line 1158 "UecDirectTransfer.cpp"
  llvm_cbe_call69 = _ZN11DummyStreamlsIPFRSt8ios_baseS2_EEERS_T_(llvm_cbe_call67, _ZSt3decRSt8ios_base);
#line 1158 "UecDirectTransfer.cpp"
  llvm_cbe_tmp70 = *(&llvm_cbe_l_uecEvent_ptr);
#line 1158 "UecDirectTransfer.cpp"
  llvm_cbe_call72 = _ZNK3Uec8UecEvent13getInstanceIdEv(llvm_cbe_tmp70);
#line 1158 "UecDirectTransfer.cpp"
  llvm_cbe_call74 = _ZN11DummyStreamlsIjEERS_T_(llvm_cbe_call69, llvm_cbe_call72);
#line 1158 "UecDirectTransfer.cpp"
  llvm_cbe_call76 = _ZN11DummyStreamlsIPKcEERS_T_(llvm_cbe_call74, ((&_OC_str83.array[((signed int )0u)])));
#line 1158 "UecDirectTransfer.cpp"
  llvm_cbe_call78 = _ZN11DummyStreamlsIPFRSt8ios_baseS2_EEERS_T_(llvm_cbe_call76, _ZSt3hexRSt8ios_base);
#line 1158 "UecDirectTransfer.cpp"
  llvm_cbe_tmp79 = *(&llvm_cbe_l_uecEvent_ptr);
#line 1158 "UecDirectTransfer.cpp"
  llvm_cbe_call81 = _ZNK3Uec8UecEvent14getAsn1PayLoadEv(llvm_cbe_tmp79);
#line 1158 "UecDirectTransfer.cpp"
  llvm_cbe_call83 = _ZN11DummyStreamlsIPvEERS_T_(llvm_cbe_call78, llvm_cbe_call81);
#line 1158 "UecDirectTransfer.cpp"
  llvm_cbe_call85 = _ZN11DummyStreamlsIPKcEERS_T_(llvm_cbe_call83, ((&_OC_str84.array[((signed int )0u)])));
#line 1158 "UecDirectTransfer.cpp"
  llvm_cbe_call87 = _ZN11DummyStreamlsIPFRSt8ios_baseS2_EEERS_T_(llvm_cbe_call85, _ZSt3hexRSt8ios_base);
#line 1158 "UecDirectTransfer.cpp"
  llvm_cbe_tmp88 = *(&llvm_cbe_l_uecEvent_ptr);
#line 1158 "UecDirectTransfer.cpp"
  llvm_cbe_call90 = _ZNK3Uec8UecEvent16getAsn1PayLoadIdEv(llvm_cbe_tmp88);
#line 1158 "UecDirectTransfer.cpp"
  llvm_cbe_call92 = _ZN11DummyStreamlsIiEERS_T_(llvm_cbe_call87, llvm_cbe_call90);
#line 1162 "UecDirectTransfer.cpp"
  _ZN3Uec14UecServiceBase15serviceFinishedEiNS_13EUecServiceIdE((((struct l_class_OC_Uec_KD__KD_UecServiceBase *)llvm_cbe_this1)), 0u, 0u);
#line 1163 "UecDirectTransfer.cpp"
  *(&llvm_cbe_retval) = 1u;
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_cleanup_2e_dst) = 2u;
#line 1163 "UecDirectTransfer.cpp"
  goto llvm_cbe_cleanup;

llvm_cbe_if_2e_end94:
#line 1166 "UecDirectTransfer.cpp"
  llvm_cbe_tmp95 = *(&llvm_cbe_l_uecEvent_ptr);
#line 1166 "UecDirectTransfer.cpp"
  llvm_cbe_call97 = _ZN3Uec17UecDirectTransfer22handleTupSrbReceiveIndEPNS_8UecEventE(llvm_cbe_this1, llvm_cbe_tmp95);
#line 1166 "UecDirectTransfer.cpp"
  *(&llvm_cbe_l_success) = llvm_cbe_call97;
#line 1166 "UecDirectTransfer.cpp"
  llvm_cbe_tmp98 = *(&llvm_cbe_l_success);
#line 1166 "UecDirectTransfer.cpp"
  if ((1u == llvm_cbe_tmp98)) {    goto llvm_cbe_if_2e_end103;  } else {    goto llvm_cbe_if_2e_then100;  }


llvm_cbe_if_2e_then100:
#line 1168 "UecDirectTransfer.cpp"
  llvm_cbe_tmp101 = *(&llvm_cbe_l_success);
#line 1168 "UecDirectTransfer.cpp"
  _ZN3Uec14UecServiceBase15serviceFinishedEiNS_13EUecServiceIdE((((struct l_class_OC_Uec_KD__KD_UecServiceBase *)llvm_cbe_this1)), llvm_cbe_tmp101, 0u);
#line 1169 "UecDirectTransfer.cpp"
  *(&llvm_cbe_retval) = 1u;
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_cleanup_2e_dst) = 3u;
#line 1169 "UecDirectTransfer.cpp"
  goto llvm_cbe_cleanup;

llvm_cbe_if_2e_end103:
#line 1172 "UecDirectTransfer.cpp"
  llvm_cbe_tmp104 = *(&llvm_cbe_l_uecEvent_ptr);
#line 1172 "UecDirectTransfer.cpp"
  llvm_cbe_call106 = _ZNK3Uec8UecEvent16getAsn1PayLoadIdEv(llvm_cbe_tmp104);
#line 1176 "UecDirectTransfer.cpp"
  llvm_cbe_call109 = _ZN11DummyStreamlsIPKcEERS_T_((&mout), ((&_OC_str81.array[((signed int )0u)])));
#line 1176 "UecDirectTransfer.cpp"
  llvm_cbe_call111 = _ZN11DummyStreamlsIPFRSt8ios_baseS2_EEERS_T_(llvm_cbe_call109, _ZSt3decRSt8ios_base);
#line 1176 "UecDirectTransfer.cpp"
  llvm_cbe_tmp112 = *(&llvm_cbe_l_uecEvent_ptr);
#line 1176 "UecDirectTransfer.cpp"
  llvm_cbe_call114 = _ZNK3Uec8UecEvent13getInstanceIdEv(llvm_cbe_tmp112);
#line 1176 "UecDirectTransfer.cpp"
  llvm_cbe_call116 = _ZN11DummyStreamlsIjEERS_T_(llvm_cbe_call111, llvm_cbe_call114);
#line 0 "LLVM INTERNAL"
  if ((llvm_cbe_call106 == 0u)) {    goto llvm_cbe_sw_2e_bb107;  } else {    goto llvm_cbe_invoke_2e_cont173;  }


llvm_cbe_sw_2e_bb107:
#line 1176 "UecDirectTransfer.cpp"
  llvm_cbe_call118 = _ZN11DummyStreamlsIPKcEERS_T_(llvm_cbe_call116, ((&_OC_str85.array[((signed int )0u)])));
#line 1180 "UecDirectTransfer.cpp"
  llvm_cbe_tmp119 = *(&llvm_cbe_l_uecUeContextDataWrite_ptr);
#line 1180 "UecDirectTransfer.cpp"
  llvm_cbe_call121 = _ZNK3Uec16UecUeContextData11getLockStepEv(llvm_cbe_tmp119);
#line 1180 "UecDirectTransfer.cpp"
  if ((llvm_cbe_call121 == 1u)) {    goto llvm_cbe_if_2e_then123;  } else {    goto llvm_cbe_if_2e_end160;  }


llvm_cbe_if_2e_then123:
#line 1182 "UecDirectTransfer.cpp"
  _ZN3Uec27UEC_CTY_TupuProceedLockStepC1Ev((&llvm_cbe_l_payLoad));
#line 1185 "UecDirectTransfer.cpp"
  llvm_cbe_call128 = _ZNK3Uec14UecServiceBase13getUecManagerEv((((struct l_class_OC_Uec_KD__KD_UecServiceBase *)llvm_cbe_this1)));
#line 1185 "UecDirectTransfer.cpp"
  llvm_cbe_call130 = _ZNK3Uec14UecManagerBase13getInstanceIdEv(llvm_cbe_call128);
#line 1185 "UecDirectTransfer.cpp"
  _ZN3Uec8UecEventC1EjNS_15EUecServiceTypeEjPvjiS2_j((&llvm_cbe_l_uecEvent), 12665u, 3u, llvm_cbe_call130, (((unsigned char *)(&llvm_cbe_l_payLoad))), 4u, 0u, ((unsigned char *)/*NULL*/0), 0u);
#line 1188 "UecDirectTransfer.cpp"
  llvm_cbe_call138 = _ZNK3Uec14UecServiceBase13getUecManagerEv((((struct l_class_OC_Uec_KD__KD_UecServiceBase *)llvm_cbe_this1)));
#line 1188 "UecDirectTransfer.cpp"
  llvm_cbe_tmp__10 = *(((struct l_class_OC_Uec_KD__KD_UecBaseData * (***) (struct l_class_OC_Uec_KD__KD_UecManagerBase *, unsigned int ))llvm_cbe_call138));
#line 1188 "UecDirectTransfer.cpp"
  llvm_cbe_tmp__11 = *((&llvm_cbe_tmp__10[((signed long long )7ull)]));
#line 1188 "UecDirectTransfer.cpp"
  llvm_cbe_call141 = llvm_cbe_tmp__11(llvm_cbe_call138, 22u);
#line 1188 "UecDirectTransfer.cpp"
  llvm_cbe_tmp__12 = *(((unsigned int  (***) (struct l_class_OC_Uec_KD__KD_UecBaseData *, struct l_class_OC_Uec_KD__KD_UecEvent *))llvm_cbe_call141));
#line 1188 "UecDirectTransfer.cpp"
  llvm_cbe_tmp__13 = *((&llvm_cbe_tmp__12[((signed long long )3ull)]));
#line 1188 "UecDirectTransfer.cpp"
  llvm_cbe_call144 = llvm_cbe_tmp__13(llvm_cbe_call141, (&llvm_cbe_l_uecEvent));
#line 1188 "UecDirectTransfer.cpp"
  if ((1u != llvm_cbe_call144)) {    goto llvm_cbe_if_2e_then146;  } else {    goto llvm_cbe_if_2e_end159;  }


llvm_cbe_if_2e_then146:
#line 1190 "UecDirectTransfer.cpp"
  llvm_cbe_call148 = _ZN11DummyStreamlsIPKcEERS_T_((&mout), ((&_OC_str81.array[((signed int )0u)])));
#line 1190 "UecDirectTransfer.cpp"
  llvm_cbe_call150 = _ZN11DummyStreamlsIPFRSt8ios_baseS2_EEERS_T_(llvm_cbe_call148, _ZSt3decRSt8ios_base);
#line 1190 "UecDirectTransfer.cpp"
  llvm_cbe_call152 = _ZNK3Uec14UecServiceBase13getUecManagerEv((((struct l_class_OC_Uec_KD__KD_UecServiceBase *)llvm_cbe_this1)));
#line 1190 "UecDirectTransfer.cpp"
  llvm_cbe_call154 = _ZNK3Uec14UecManagerBase13getInstanceIdEv(llvm_cbe_call152);
#line 1190 "UecDirectTransfer.cpp"
  llvm_cbe_call156 = _ZN11DummyStreamlsIjEERS_T_(llvm_cbe_call150, llvm_cbe_call154);
#line 1190 "UecDirectTransfer.cpp"
  llvm_cbe_call158 = _ZN11DummyStreamlsIPKcEERS_T_(llvm_cbe_call156, ((&_OC_str86.array[((signed int )0u)])));
#line 0 "LLVM INTERNAL"
  goto llvm_cbe_if_2e_end159;

llvm_cbe_if_2e_end159:
#line 1194 "UecDirectTransfer.cpp"
  _ZN3Uec8UecEventD1Ev((&llvm_cbe_l_uecEvent));
#line 0 "LLVM INTERNAL"
  goto llvm_cbe_if_2e_end160;

llvm_cbe_if_2e_end160:
#line 1198 "UecDirectTransfer.cpp"
  llvm_cbe_tmp161 = *(&llvm_cbe_l_uecEvent_ptr);
#line 1198 "UecDirectTransfer.cpp"
  llvm_cbe_call163 = _ZN3Uec17UecDirectTransfer30handleRrcUlInformationTransferEPNS_8UecEventE(llvm_cbe_this1, llvm_cbe_tmp161);
#line 1198 "UecDirectTransfer.cpp"
  *(&llvm_cbe_l_success) = llvm_cbe_call163;
#line 1198 "UecDirectTransfer.cpp"
  llvm_cbe_tmp164 = *(&llvm_cbe_l_success);
#line 1198 "UecDirectTransfer.cpp"
  if ((1u == llvm_cbe_tmp164)) {    goto llvm_cbe_if_2e_end169;  } else {    goto llvm_cbe_if_2e_then166;  }


llvm_cbe_if_2e_then166:
#line 1200 "UecDirectTransfer.cpp"
  llvm_cbe_tmp167 = *(&llvm_cbe_l_success);
#line 1200 "UecDirectTransfer.cpp"
  _ZN3Uec14UecServiceBase15serviceFinishedEiNS_13EUecServiceIdE((((struct l_class_OC_Uec_KD__KD_UecServiceBase *)llvm_cbe_this1)), llvm_cbe_tmp167, 0u);
#line 1201 "UecDirectTransfer.cpp"
  *(&llvm_cbe_retval) = 1u;
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_cleanup_2e_dst) = 4u;
#line 1201 "UecDirectTransfer.cpp"
  goto llvm_cbe_cleanup;

llvm_cbe_if_2e_end169:
#line 1205 "UecDirectTransfer.cpp"
  llvm_cbe_tmp170 = *(&llvm_cbe_l_uecEvent_ptr);
#line 1205 "UecDirectTransfer.cpp"
  llvm_cbe_call172 = _ZN3Uec17UecDirectTransfer22sendS1ApUlNasTransportEPNS_8UecEventE(llvm_cbe_this1, llvm_cbe_tmp170);
#line 1205 "UecDirectTransfer.cpp"
  *(&llvm_cbe_l_success) = llvm_cbe_call172;
#line 1207 "UecDirectTransfer.cpp"
  goto llvm_cbe_sw_2e_epilog528;

llvm_cbe_invoke_2e_cont173:
#line 1213 "UecDirectTransfer.cpp"
  llvm_cbe_call183 = _ZN11DummyStreamlsIPKcEERS_T_(llvm_cbe_call116, ((&_OC_str87.array[((signed int )0u)])));
#line 1213 "UecDirectTransfer.cpp"
  llvm_cbe_call185 = _ZN11DummyStreamlsIPFRSt8ios_baseS2_EEERS_T_(llvm_cbe_call183, _ZSt3hexRSt8ios_base);
#line 1213 "UecDirectTransfer.cpp"
  llvm_cbe_tmp186 = *(&llvm_cbe_l_uecEvent_ptr);
#line 1213 "UecDirectTransfer.cpp"
  llvm_cbe_call188 = _ZNK6CEvent10GetEventIdEv((((struct l_class_OC_CEvent *)llvm_cbe_tmp186)));
#line 1213 "UecDirectTransfer.cpp"
  llvm_cbe_call190 = _ZN11DummyStreamlsIjEERS_T_(llvm_cbe_call185, llvm_cbe_call188);
#line 1213 "UecDirectTransfer.cpp"
  llvm_cbe_call192 = _ZN11DummyStreamlsIPKcEERS_T_(llvm_cbe_call190, ((&_OC_str82.array[((signed int )0u)])));
#line 1213 "UecDirectTransfer.cpp"
  llvm_cbe_call194 = _ZN11DummyStreamlsIPFRSt8ios_baseS2_EEERS_T_(llvm_cbe_call192, _ZSt3hexRSt8ios_base);
#line 1213 "UecDirectTransfer.cpp"
  llvm_cbe_tmp195 = *(&llvm_cbe_l_uecEvent_ptr);
#line 1213 "UecDirectTransfer.cpp"
  llvm_cbe_call197 = _ZNK3Uec8UecEvent16getAsn1PayLoadIdEv(llvm_cbe_tmp195);
#line 1213 "UecDirectTransfer.cpp"
  llvm_cbe_call199 = _ZN11DummyStreamlsIiEERS_T_(llvm_cbe_call194, llvm_cbe_call197);
#line 1217 "UecDirectTransfer.cpp"
  _ZN3Uec14UecServiceBase15serviceFinishedEiNS_13EUecServiceIdE((((struct l_class_OC_Uec_KD__KD_UecServiceBase *)llvm_cbe_this1)), 0u, 0u);
#line 1218 "UecDirectTransfer.cpp"
  *(&llvm_cbe_retval) = 1u;
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_cleanup_2e_dst) = 6u;
#line 1218 "UecDirectTransfer.cpp"
  goto llvm_cbe_cleanup;

llvm_cbe_sw_2e_bb201:
#line 1227 "UecDirectTransfer.cpp"
  llvm_cbe_tmp202 = *(&llvm_cbe_l_uecEvent_ptr);
#line 1227 "UecDirectTransfer.cpp"
  llvm_cbe_call204 = _ZNK3Uec8UecEvent14getAsn1PayLoadEv(llvm_cbe_tmp202);
#line 1227 "UecDirectTransfer.cpp"
  if ((llvm_cbe_call204 == ((unsigned char *)/*NULL*/0))) {    goto llvm_cbe_if_2e_then211;  } else {    goto llvm_cbe_lor_2e_lhs_2e_false206;  }


llvm_cbe_lor_2e_lhs_2e_false206:
#line 1227 "UecDirectTransfer.cpp"
  llvm_cbe_tmp207 = *(&llvm_cbe_l_uecEvent_ptr);
#line 1227 "UecDirectTransfer.cpp"
  llvm_cbe_call209 = _ZNK3Uec8UecEvent16getAsn1PayLoadIdEv(llvm_cbe_tmp207);
#line 1227 "UecDirectTransfer.cpp"
  if ((llvm_cbe_call209 == 0u)) {    goto llvm_cbe_if_2e_then211;  } else {    goto llvm_cbe_if_2e_end240;  }


llvm_cbe_if_2e_then211:
#line 1231 "UecDirectTransfer.cpp"
  llvm_cbe_call213 = _ZN11DummyStreamlsIPKcEERS_T_((&mout), ((&_OC_str81.array[((signed int )0u)])));
#line 1231 "UecDirectTransfer.cpp"
  llvm_cbe_call215 = _ZN11DummyStreamlsIPFRSt8ios_baseS2_EEERS_T_(llvm_cbe_call213, _ZSt3decRSt8ios_base);
#line 1231 "UecDirectTransfer.cpp"
  llvm_cbe_tmp216 = *(&llvm_cbe_l_uecEvent_ptr);
#line 1231 "UecDirectTransfer.cpp"
  llvm_cbe_call218 = _ZNK3Uec8UecEvent13getInstanceIdEv(llvm_cbe_tmp216);
#line 1231 "UecDirectTransfer.cpp"
  llvm_cbe_call220 = _ZN11DummyStreamlsIjEERS_T_(llvm_cbe_call215, llvm_cbe_call218);
#line 1231 "UecDirectTransfer.cpp"
  llvm_cbe_call222 = _ZN11DummyStreamlsIPKcEERS_T_(llvm_cbe_call220, ((&_OC_str83.array[((signed int )0u)])));
#line 1231 "UecDirectTransfer.cpp"
  llvm_cbe_call224 = _ZN11DummyStreamlsIPFRSt8ios_baseS2_EEERS_T_(llvm_cbe_call222, _ZSt3hexRSt8ios_base);
#line 1231 "UecDirectTransfer.cpp"
  llvm_cbe_tmp225 = *(&llvm_cbe_l_uecEvent_ptr);
#line 1231 "UecDirectTransfer.cpp"
  llvm_cbe_call227 = _ZNK3Uec8UecEvent14getAsn1PayLoadEv(llvm_cbe_tmp225);
#line 1231 "UecDirectTransfer.cpp"
  llvm_cbe_call229 = _ZN11DummyStreamlsIPvEERS_T_(llvm_cbe_call224, llvm_cbe_call227);
#line 1231 "UecDirectTransfer.cpp"
  llvm_cbe_call231 = _ZN11DummyStreamlsIPKcEERS_T_(llvm_cbe_call229, ((&_OC_str84.array[((signed int )0u)])));
#line 1231 "UecDirectTransfer.cpp"
  llvm_cbe_call233 = _ZN11DummyStreamlsIPFRSt8ios_baseS2_EEERS_T_(llvm_cbe_call231, _ZSt3hexRSt8ios_base);
#line 1231 "UecDirectTransfer.cpp"
  llvm_cbe_tmp234 = *(&llvm_cbe_l_uecEvent_ptr);
#line 1231 "UecDirectTransfer.cpp"
  llvm_cbe_call236 = _ZNK3Uec8UecEvent16getAsn1PayLoadIdEv(llvm_cbe_tmp234);
#line 1231 "UecDirectTransfer.cpp"
  llvm_cbe_call238 = _ZN11DummyStreamlsIiEERS_T_(llvm_cbe_call233, llvm_cbe_call236);
#line 1235 "UecDirectTransfer.cpp"
  _ZN3Uec14UecServiceBase15serviceFinishedEiNS_13EUecServiceIdE((((struct l_class_OC_Uec_KD__KD_UecServiceBase *)llvm_cbe_this1)), 0u, 0u);
#line 1236 "UecDirectTransfer.cpp"
  *(&llvm_cbe_retval) = 1u;
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_cleanup_2e_dst) = 8u;
#line 1236 "UecDirectTransfer.cpp"
  goto llvm_cbe_cleanup;

llvm_cbe_if_2e_end240:
#line 1239 "UecDirectTransfer.cpp"
  llvm_cbe_tmp241 = *(&llvm_cbe_l_uecEvent_ptr);
#line 1239 "UecDirectTransfer.cpp"
  llvm_cbe_call243 = _ZN3Uec17UecDirectTransfer28handleTupL3MessageIndicationEPNS_8UecEventE(llvm_cbe_this1, llvm_cbe_tmp241);
#line 1239 "UecDirectTransfer.cpp"
  *(&llvm_cbe_l_success) = llvm_cbe_call243;
#line 1239 "UecDirectTransfer.cpp"
  llvm_cbe_tmp244 = *(&llvm_cbe_l_success);
#line 1239 "UecDirectTransfer.cpp"
  if ((1u == llvm_cbe_tmp244)) {    goto llvm_cbe_if_2e_end249;  } else {    goto llvm_cbe_if_2e_then246;  }


llvm_cbe_if_2e_then246:
#line 1241 "UecDirectTransfer.cpp"
  llvm_cbe_tmp247 = *(&llvm_cbe_l_success);
#line 1241 "UecDirectTransfer.cpp"
  _ZN3Uec14UecServiceBase15serviceFinishedEiNS_13EUecServiceIdE((((struct l_class_OC_Uec_KD__KD_UecServiceBase *)llvm_cbe_this1)), llvm_cbe_tmp247, 0u);
#line 1242 "UecDirectTransfer.cpp"
  *(&llvm_cbe_retval) = 1u;
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_cleanup_2e_dst) = 9u;
#line 1242 "UecDirectTransfer.cpp"
  goto llvm_cbe_cleanup;

llvm_cbe_if_2e_end249:
#line 1245 "UecDirectTransfer.cpp"
  llvm_cbe_tmp250 = *(&llvm_cbe_l_uecEvent_ptr);
#line 1245 "UecDirectTransfer.cpp"
  llvm_cbe_call252 = _ZNK3Uec8UecEvent16getAsn1PayLoadIdEv(llvm_cbe_tmp250);
#line 1249 "UecDirectTransfer.cpp"
  llvm_cbe_call255 = _ZN11DummyStreamlsIPKcEERS_T_((&mout), ((&_OC_str81.array[((signed int )0u)])));
#line 1249 "UecDirectTransfer.cpp"
  llvm_cbe_call257 = _ZN11DummyStreamlsIPFRSt8ios_baseS2_EEERS_T_(llvm_cbe_call255, _ZSt3decRSt8ios_base);
#line 1249 "UecDirectTransfer.cpp"
  llvm_cbe_tmp258 = *(&llvm_cbe_l_uecEvent_ptr);
#line 1249 "UecDirectTransfer.cpp"
  llvm_cbe_call260 = _ZNK3Uec8UecEvent13getInstanceIdEv(llvm_cbe_tmp258);
#line 1249 "UecDirectTransfer.cpp"
  llvm_cbe_call262 = _ZN11DummyStreamlsIjEERS_T_(llvm_cbe_call257, llvm_cbe_call260);
#line 0 "LLVM INTERNAL"
  if ((llvm_cbe_call252 == 1u)) {    goto llvm_cbe_sw_2e_bb253;  } else {    goto llvm_cbe_invoke_2e_cont322;  }


llvm_cbe_sw_2e_bb253:
#line 1249 "UecDirectTransfer.cpp"
  llvm_cbe_call264 = _ZN11DummyStreamlsIPKcEERS_T_(llvm_cbe_call262, ((&_OC_str88.array[((signed int )0u)])));
#line 1249 "UecDirectTransfer.cpp"
  llvm_cbe_call266 = _ZN11DummyStreamlsIPFRSt8ios_baseS2_EEERS_T_(llvm_cbe_call264, _ZSt3decRSt8ios_base);
#line 1249 "UecDirectTransfer.cpp"
  llvm_cbe_tmp267 = *(&llvm_cbe_l_uecUeContextDataWrite_ptr);
#line 1249 "UecDirectTransfer.cpp"
  llvm_cbe_call269 = _ZNK3Uec16UecUeContextData14getNasDeliveryEv(llvm_cbe_tmp267);
#line 1249 "UecDirectTransfer.cpp"
  llvm_cbe_call271 = _ZN11DummyStreamlsIN3Uec15EUecNasDeliveryEEERS_T_(llvm_cbe_call266, llvm_cbe_call269);
#line 1249 "UecDirectTransfer.cpp"
  llvm_cbe_call273 = _ZN11DummyStreamlsIPKcEERS_T_(llvm_cbe_call271, ((&_OC_str20.array[((signed int )0u)])));
#line 1253 "UecDirectTransfer.cpp"
  llvm_cbe_tmp274 = *(&llvm_cbe_l_uecEvent_ptr);
#line 1253 "UecDirectTransfer.cpp"
  llvm_cbe_call276 = _ZN3Uec17UecDirectTransfer24handleS1ApDlNasTransportEPNS_8UecEventE(llvm_cbe_this1, llvm_cbe_tmp274);
#line 1253 "UecDirectTransfer.cpp"
  *(&llvm_cbe_l_success) = llvm_cbe_call276;
#line 1253 "UecDirectTransfer.cpp"
  llvm_cbe_tmp277 = *(&llvm_cbe_l_success);
#line 1253 "UecDirectTransfer.cpp"
  if ((1u == llvm_cbe_tmp277)) {    goto llvm_cbe_if_2e_end282;  } else {    goto llvm_cbe_if_2e_then279;  }


llvm_cbe_if_2e_then279:
#line 1255 "UecDirectTransfer.cpp"
  llvm_cbe_tmp280 = *(&llvm_cbe_l_success);
#line 1255 "UecDirectTransfer.cpp"
  _ZN3Uec14UecServiceBase15serviceFinishedEiNS_13EUecServiceIdE((((struct l_class_OC_Uec_KD__KD_UecServiceBase *)llvm_cbe_this1)), llvm_cbe_tmp280, 0u);
#line 1256 "UecDirectTransfer.cpp"
  *(&llvm_cbe_retval) = 1u;
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_cleanup_2e_dst) = 10u;
#line 1256 "UecDirectTransfer.cpp"
  goto llvm_cbe_cleanup;

llvm_cbe_if_2e_end282:
#line 1259 "UecDirectTransfer.cpp"
  llvm_cbe_tmp283 = *(&llvm_cbe_l_uecUeContextDataWrite_ptr);
#line 1259 "UecDirectTransfer.cpp"
  llvm_cbe_call285 = _ZNK3Uec16UecUeContextData14getNasDeliveryEv(llvm_cbe_tmp283);
#line 1259 "UecDirectTransfer.cpp"
  switch (llvm_cbe_call285) {
  default:
    goto llvm_cbe_sw_2e_default298;;
  case 0u:
    goto llvm_cbe_sw_2e_bb286;    break;
  case 1u:
    goto llvm_cbe_sw_2e_bb290;  case 2u:
    goto llvm_cbe_sw_2e_bb294;  }

llvm_cbe_sw_2e_bb286:
#line 1264 "UecDirectTransfer.cpp"
  llvm_cbe_tmp287 = *(&llvm_cbe_l_uecEvent_ptr);
#line 1264 "UecDirectTransfer.cpp"
  llvm_cbe_call289 = _ZN3Uec17UecDirectTransfer28sendRrcDlInformationTransferEPNS_8UecEventE(llvm_cbe_this1, llvm_cbe_tmp287);
#line 1264 "UecDirectTransfer.cpp"
  *(&llvm_cbe_l_success) = llvm_cbe_call289;
#line 1265 "UecDirectTransfer.cpp"
  goto llvm_cbe_sw_2e_epilog528;

llvm_cbe_sw_2e_bb290:
#line 1271 "UecDirectTransfer.cpp"
  llvm_cbe_tmp291 = *(&llvm_cbe_l_uecEvent_ptr);
#line 1271 "UecDirectTransfer.cpp"
  llvm_cbe_call293 = _ZN3Uec17UecDirectTransfer32sendS1ApNasNonDeliveryIndicationEPNS_8UecEventE(llvm_cbe_this1, llvm_cbe_tmp291);
#line 1271 "UecDirectTransfer.cpp"
  *(&llvm_cbe_l_success) = llvm_cbe_call293;
#line 1272 "UecDirectTransfer.cpp"
  goto llvm_cbe_sw_2e_epilog528;

llvm_cbe_sw_2e_bb294:
#line 1278 "UecDirectTransfer.cpp"
  llvm_cbe_tmp295 = *(&llvm_cbe_l_uecEvent_ptr);
#line 1278 "UecDirectTransfer.cpp"
  llvm_cbe_call297 = _ZN3Uec17UecDirectTransfer11storeNasPduEPNS_8UecEventE(llvm_cbe_this1, llvm_cbe_tmp295);
#line 1279 "UecDirectTransfer.cpp"
  *(&llvm_cbe_l_success) = 1u;
#line 1280 "UecDirectTransfer.cpp"
  goto llvm_cbe_sw_2e_epilog528;

llvm_cbe_sw_2e_default298:
#line 1286 "UecDirectTransfer.cpp"
  llvm_cbe_call300 = _ZN11DummyStreamlsIPKcEERS_T_((&mout), ((&_OC_str81.array[((signed int )0u)])));
#line 1286 "UecDirectTransfer.cpp"
  llvm_cbe_call302 = _ZN11DummyStreamlsIPFRSt8ios_baseS2_EEERS_T_(llvm_cbe_call300, _ZSt3decRSt8ios_base);
#line 1286 "UecDirectTransfer.cpp"
  llvm_cbe_tmp303 = *(&llvm_cbe_l_uecEvent_ptr);
#line 1286 "UecDirectTransfer.cpp"
  llvm_cbe_call305 = _ZNK3Uec8UecEvent13getInstanceIdEv(llvm_cbe_tmp303);
#line 1286 "UecDirectTransfer.cpp"
  llvm_cbe_call307 = _ZN11DummyStreamlsIjEERS_T_(llvm_cbe_call302, llvm_cbe_call305);
#line 1286 "UecDirectTransfer.cpp"
  llvm_cbe_call309 = _ZN11DummyStreamlsIPKcEERS_T_(llvm_cbe_call307, ((&_OC_str89.array[((signed int )0u)])));
#line 1286 "UecDirectTransfer.cpp"
  llvm_cbe_call311 = _ZN11DummyStreamlsIPFRSt8ios_baseS2_EEERS_T_(llvm_cbe_call309, _ZSt3decRSt8ios_base);
#line 1286 "UecDirectTransfer.cpp"
  llvm_cbe_tmp312 = *(&llvm_cbe_l_uecUeContextDataWrite_ptr);
#line 1286 "UecDirectTransfer.cpp"
  llvm_cbe_call314 = _ZNK3Uec16UecUeContextData14getNasDeliveryEv(llvm_cbe_tmp312);
#line 1286 "UecDirectTransfer.cpp"
  llvm_cbe_call316 = _ZN11DummyStreamlsIN3Uec15EUecNasDeliveryEEERS_T_(llvm_cbe_call311, llvm_cbe_call314);
#line 1286 "UecDirectTransfer.cpp"
  llvm_cbe_call318 = _ZN11DummyStreamlsIPKcEERS_T_(llvm_cbe_call316, ((&_OC_str20.array[((signed int )0u)])));
#line 1290 "UecDirectTransfer.cpp"
  _ZN3Uec14UecServiceBase15serviceFinishedEiNS_13EUecServiceIdE((((struct l_class_OC_Uec_KD__KD_UecServiceBase *)llvm_cbe_this1)), 0u, 0u);
#line 1291 "UecDirectTransfer.cpp"
  *(&llvm_cbe_retval) = 1u;
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_cleanup_2e_dst) = 14u;
#line 1291 "UecDirectTransfer.cpp"
  goto llvm_cbe_cleanup;

llvm_cbe_invoke_2e_cont322:
#line 1301 "UecDirectTransfer.cpp"
  llvm_cbe_call332 = _ZN11DummyStreamlsIPKcEERS_T_(llvm_cbe_call262, ((&_OC_str87.array[((signed int )0u)])));
#line 1301 "UecDirectTransfer.cpp"
  llvm_cbe_call334 = _ZN11DummyStreamlsIPFRSt8ios_baseS2_EEERS_T_(llvm_cbe_call332, _ZSt3hexRSt8ios_base);
#line 1301 "UecDirectTransfer.cpp"
  llvm_cbe_tmp335 = *(&llvm_cbe_l_uecEvent_ptr);
#line 1301 "UecDirectTransfer.cpp"
  llvm_cbe_call337 = _ZNK6CEvent10GetEventIdEv((((struct l_class_OC_CEvent *)llvm_cbe_tmp335)));
#line 1301 "UecDirectTransfer.cpp"
  llvm_cbe_call339 = _ZN11DummyStreamlsIjEERS_T_(llvm_cbe_call334, llvm_cbe_call337);
#line 1301 "UecDirectTransfer.cpp"
  llvm_cbe_call341 = _ZN11DummyStreamlsIPKcEERS_T_(llvm_cbe_call339, ((&_OC_str82.array[((signed int )0u)])));
#line 1301 "UecDirectTransfer.cpp"
  llvm_cbe_call343 = _ZN11DummyStreamlsIPFRSt8ios_baseS2_EEERS_T_(llvm_cbe_call341, _ZSt3hexRSt8ios_base);
#line 1301 "UecDirectTransfer.cpp"
  llvm_cbe_tmp344 = *(&llvm_cbe_l_uecEvent_ptr);
#line 1301 "UecDirectTransfer.cpp"
  llvm_cbe_call346 = _ZNK3Uec8UecEvent16getAsn1PayLoadIdEv(llvm_cbe_tmp344);
#line 1301 "UecDirectTransfer.cpp"
  llvm_cbe_call348 = _ZN11DummyStreamlsIiEERS_T_(llvm_cbe_call343, llvm_cbe_call346);
#line 1306 "UecDirectTransfer.cpp"
  _ZN3Uec14UecServiceBase15serviceFinishedEiNS_13EUecServiceIdE((((struct l_class_OC_Uec_KD__KD_UecServiceBase *)llvm_cbe_this1)), 0u, 0u);
#line 1307 "UecDirectTransfer.cpp"
  *(&llvm_cbe_retval) = 1u;
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_cleanup_2e_dst) = 16u;
#line 1307 "UecDirectTransfer.cpp"
  goto llvm_cbe_cleanup;

llvm_cbe_sw_2e_bb351:
#line 1316 "UecDirectTransfer.cpp"
  llvm_cbe_tmp352 = *(&llvm_cbe_l_uecEvent_ptr);
#line 1316 "UecDirectTransfer.cpp"
  llvm_cbe_call354 = _ZN3Uec17UecDirectTransfer20handleTupSrbSendRespEPNS_8UecEventE(llvm_cbe_this1, llvm_cbe_tmp352);
#line 1316 "UecDirectTransfer.cpp"
  *(&llvm_cbe_l_success) = llvm_cbe_call354;
#line 1316 "UecDirectTransfer.cpp"
  llvm_cbe_tmp355 = *(&llvm_cbe_l_success);
#line 1316 "UecDirectTransfer.cpp"
  if ((1u == llvm_cbe_tmp355)) {    goto llvm_cbe_if_2e_end360;  } else {    goto llvm_cbe_if_2e_then357;  }


llvm_cbe_if_2e_then357:
#line 1318 "UecDirectTransfer.cpp"
  llvm_cbe_tmp358 = *(&llvm_cbe_l_success);
#line 1318 "UecDirectTransfer.cpp"
  _ZN3Uec14UecServiceBase15serviceFinishedEiNS_13EUecServiceIdE((((struct l_class_OC_Uec_KD__KD_UecServiceBase *)llvm_cbe_this1)), llvm_cbe_tmp358, 0u);
#line 1319 "UecDirectTransfer.cpp"
  *(&llvm_cbe_retval) = 1u;
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_cleanup_2e_dst) = 15u;
#line 1319 "UecDirectTransfer.cpp"
  goto llvm_cbe_cleanup;

llvm_cbe_if_2e_end360:
#line 1322 "UecDirectTransfer.cpp"
  llvm_cbe_tmp361 = *(&llvm_cbe_l_uecEvent_ptr);
#line 1322 "UecDirectTransfer.cpp"
  llvm_cbe_call363 = _ZNK3Uec8UecEvent16getAsn1PayLoadIdEv(llvm_cbe_tmp361);
#line 1326 "UecDirectTransfer.cpp"
  llvm_cbe_call366 = _ZN11DummyStreamlsIPKcEERS_T_((&mout), ((&_OC_str81.array[((signed int )0u)])));
#line 1326 "UecDirectTransfer.cpp"
  llvm_cbe_call368 = _ZN11DummyStreamlsIPFRSt8ios_baseS2_EEERS_T_(llvm_cbe_call366, _ZSt3decRSt8ios_base);
#line 1326 "UecDirectTransfer.cpp"
  llvm_cbe_tmp369 = *(&llvm_cbe_l_uecEvent_ptr);
#line 1326 "UecDirectTransfer.cpp"
  llvm_cbe_call371 = _ZNK3Uec8UecEvent13getInstanceIdEv(llvm_cbe_tmp369);
#line 1326 "UecDirectTransfer.cpp"
  llvm_cbe_call373 = _ZN11DummyStreamlsIjEERS_T_(llvm_cbe_call368, llvm_cbe_call371);
#line 0 "LLVM INTERNAL"
  if ((llvm_cbe_call363 == 2u)) {    goto llvm_cbe_sw_2e_bb364;  } else {    goto llvm_cbe_invoke_2e_cont389;  }


llvm_cbe_sw_2e_bb364:
#line 1326 "UecDirectTransfer.cpp"
  llvm_cbe_call375 = _ZN11DummyStreamlsIPKcEERS_T_(llvm_cbe_call373, ((&_OC_str90.array[((signed int )0u)])));
#line 1329 "UecDirectTransfer.cpp"
  llvm_cbe_tmp376 = *(&llvm_cbe_l_uecEvent_ptr);
#line 1329 "UecDirectTransfer.cpp"
  llvm_cbe_call378 = _ZNK3Uec17UecDirectTransfer30handleRrcDlInformationTransferEPNS_8UecEventE(llvm_cbe_this1, llvm_cbe_tmp376);
#line 1329 "UecDirectTransfer.cpp"
  *(&llvm_cbe_l_success) = llvm_cbe_call378;
#line 1329 "UecDirectTransfer.cpp"
  llvm_cbe_tmp379 = *(&llvm_cbe_l_success);
#line 1329 "UecDirectTransfer.cpp"
  if ((1u == llvm_cbe_tmp379)) {    goto llvm_cbe_if_2e_end384;  } else {    goto llvm_cbe_if_2e_then381;  }


llvm_cbe_if_2e_then381:
#line 1331 "UecDirectTransfer.cpp"
  llvm_cbe_tmp382 = *(&llvm_cbe_l_success);
#line 1331 "UecDirectTransfer.cpp"
  _ZN3Uec14UecServiceBase15serviceFinishedEiNS_13EUecServiceIdE((((struct l_class_OC_Uec_KD__KD_UecServiceBase *)llvm_cbe_this1)), llvm_cbe_tmp382, 0u);
#line 1332 "UecDirectTransfer.cpp"
  *(&llvm_cbe_retval) = 1u;
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_cleanup_2e_dst) = 13u;
#line 1332 "UecDirectTransfer.cpp"
  goto llvm_cbe_cleanup;

llvm_cbe_if_2e_end384:
#line 1336 "UecDirectTransfer.cpp"
  llvm_cbe_tmp385 = *(&llvm_cbe_l_uecEvent_ptr);
#line 1336 "UecDirectTransfer.cpp"
  llvm_cbe_call387 = _ZN3Uec17UecDirectTransfer32sendS1ApNasNonDeliveryIndicationEPNS_8UecEventE(llvm_cbe_this1, llvm_cbe_tmp385);
#line 1336 "UecDirectTransfer.cpp"
  *(&llvm_cbe_l_success) = llvm_cbe_call387;
#line 1338 "UecDirectTransfer.cpp"
  goto llvm_cbe_sw_2e_epilog528;

llvm_cbe_invoke_2e_cont389:
#line 1344 "UecDirectTransfer.cpp"
  llvm_cbe_call399 = _ZN11DummyStreamlsIPKcEERS_T_(llvm_cbe_call373, ((&_OC_str87.array[((signed int )0u)])));
#line 1344 "UecDirectTransfer.cpp"
  llvm_cbe_call401 = _ZN11DummyStreamlsIPFRSt8ios_baseS2_EEERS_T_(llvm_cbe_call399, _ZSt3hexRSt8ios_base);
#line 1344 "UecDirectTransfer.cpp"
  llvm_cbe_tmp402 = *(&llvm_cbe_l_uecEvent_ptr);
#line 1344 "UecDirectTransfer.cpp"
  llvm_cbe_call404 = _ZNK6CEvent10GetEventIdEv((((struct l_class_OC_CEvent *)llvm_cbe_tmp402)));
#line 1344 "UecDirectTransfer.cpp"
  llvm_cbe_call406 = _ZN11DummyStreamlsIjEERS_T_(llvm_cbe_call401, llvm_cbe_call404);
#line 1344 "UecDirectTransfer.cpp"
  llvm_cbe_call408 = _ZN11DummyStreamlsIPKcEERS_T_(llvm_cbe_call406, ((&_OC_str82.array[((signed int )0u)])));
#line 1344 "UecDirectTransfer.cpp"
  llvm_cbe_call410 = _ZN11DummyStreamlsIPFRSt8ios_baseS2_EEERS_T_(llvm_cbe_call408, _ZSt3hexRSt8ios_base);
#line 1344 "UecDirectTransfer.cpp"
  llvm_cbe_tmp411 = *(&llvm_cbe_l_uecEvent_ptr);
#line 1344 "UecDirectTransfer.cpp"
  llvm_cbe_call413 = _ZNK3Uec8UecEvent16getAsn1PayLoadIdEv(llvm_cbe_tmp411);
#line 1344 "UecDirectTransfer.cpp"
  llvm_cbe_call415 = _ZN11DummyStreamlsIiEERS_T_(llvm_cbe_call410, llvm_cbe_call413);
#line 1348 "UecDirectTransfer.cpp"
  _ZN3Uec14UecServiceBase15serviceFinishedEiNS_13EUecServiceIdE((((struct l_class_OC_Uec_KD__KD_UecServiceBase *)llvm_cbe_this1)), 0u, 0u);
#line 1349 "UecDirectTransfer.cpp"
  *(&llvm_cbe_retval) = 1u;
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_cleanup_2e_dst) = 12u;
#line 1349 "UecDirectTransfer.cpp"
  goto llvm_cbe_cleanup;

llvm_cbe_sw_2e_bb418:
#line 1358 "UecDirectTransfer.cpp"
  llvm_cbe_tmp420 = *(&llvm_cbe_l_uecEvent_ptr);
#line 1358 "UecDirectTransfer.cpp"
  llvm_cbe_call422 = _ZNK3Uec8UecEvent10getPayLoadEv(llvm_cbe_tmp420);
#line 1358 "UecDirectTransfer.cpp"
  *(&llvm_cbe_l_payLoad_ptr) = (((struct l_struct_OC_Uec_KD__KD_UEC_DTF_DirectTransfer *)llvm_cbe_call422));
#line 1360 "UecDirectTransfer.cpp"
  llvm_cbe_call424 = _ZN11DummyStreamlsIPKcEERS_T_((&mout), ((&_OC_str81.array[((signed int )0u)])));
#line 1360 "UecDirectTransfer.cpp"
  llvm_cbe_call426 = _ZN11DummyStreamlsIPFRSt8ios_baseS2_EEERS_T_(llvm_cbe_call424, _ZSt3decRSt8ios_base);
#line 1360 "UecDirectTransfer.cpp"
  llvm_cbe_tmp427 = *(&llvm_cbe_l_uecEvent_ptr);
#line 1360 "UecDirectTransfer.cpp"
  llvm_cbe_call429 = _ZNK3Uec8UecEvent13getInstanceIdEv(llvm_cbe_tmp427);
#line 1360 "UecDirectTransfer.cpp"
  llvm_cbe_call431 = _ZN11DummyStreamlsIjEERS_T_(llvm_cbe_call426, llvm_cbe_call429);
#line 1360 "UecDirectTransfer.cpp"
  llvm_cbe_call433 = _ZN11DummyStreamlsIPKcEERS_T_(llvm_cbe_call431, ((&_OC_str91.array[((signed int )0u)])));
#line 1360 "UecDirectTransfer.cpp"
  llvm_cbe_call435 = _ZN11DummyStreamlsIPFRSt8ios_baseS2_EEERS_T_(llvm_cbe_call433, _ZSt3decRSt8ios_base);
#line 1360 "UecDirectTransfer.cpp"
  llvm_cbe_tmp436 = *(&llvm_cbe_l_payLoad_ptr);
#line 1360 "UecDirectTransfer.cpp"
  llvm_cbe_tmp438 = *((&llvm_cbe_tmp436->field0));
#line 1360 "UecDirectTransfer.cpp"
  llvm_cbe_call440 = _ZN11DummyStreamlsIN3Uec13EUecDirectionEEERS_T_(llvm_cbe_call435, llvm_cbe_tmp438);
#line 1360 "UecDirectTransfer.cpp"
  llvm_cbe_call442 = _ZN11DummyStreamlsIPKcEERS_T_(llvm_cbe_call440, ((&_OC_str92.array[((signed int )0u)])));
#line 1360 "UecDirectTransfer.cpp"
  llvm_cbe_call444 = _ZN11DummyStreamlsIPFRSt8ios_baseS2_EEERS_T_(llvm_cbe_call442, _ZSt3decRSt8ios_base);
#line 1360 "UecDirectTransfer.cpp"
  llvm_cbe_tmp445 = *(&llvm_cbe_l_payLoad_ptr);
#line 1360 "UecDirectTransfer.cpp"
  llvm_cbe_tmp448 = *((&((&llvm_cbe_tmp445->field2))->field0));
#line 1360 "UecDirectTransfer.cpp"
  llvm_cbe_call450 = _ZN11DummyStreamlsIjEERS_T_(llvm_cbe_call444, llvm_cbe_tmp448);
#line 1364 "UecDirectTransfer.cpp"
  llvm_cbe_tmp451 = *(&llvm_cbe_l_uecEvent_ptr);
#line 1364 "UecDirectTransfer.cpp"
  llvm_cbe_call453 = _ZNK3Uec17UecDirectTransfer26handleUecDtfDirectTransferEPNS_8UecEventE(llvm_cbe_this1, llvm_cbe_tmp451);
#line 1364 "UecDirectTransfer.cpp"
  *(&llvm_cbe_l_success) = llvm_cbe_call453;
#line 1366 "UecDirectTransfer.cpp"
  llvm_cbe_tmp454 = *(&llvm_cbe_l_success);
#line 1366 "UecDirectTransfer.cpp"
  if ((1u == llvm_cbe_tmp454)) {    goto llvm_cbe_if_2e_then456;  } else {    goto llvm_cbe_if_2e_end488;  }


llvm_cbe_if_2e_then456:
#line 1368 "UecDirectTransfer.cpp"
  llvm_cbe_tmp457 = *(&llvm_cbe_l_payLoad_ptr);
#line 1368 "UecDirectTransfer.cpp"
  llvm_cbe_tmp459 = *((&llvm_cbe_tmp457->field0));
#line 1368 "UecDirectTransfer.cpp"
  switch (llvm_cbe_tmp459) {
  default:
    goto llvm_cbe_sw_2e_default468;;
  case 0u:
    goto llvm_cbe_sw_2e_bb460;    break;
  case 2u:
    goto llvm_cbe_sw_2e_bb464;  }

llvm_cbe_sw_2e_bb460:
#line 1373 "UecDirectTransfer.cpp"
  llvm_cbe_tmp461 = *(&llvm_cbe_l_uecEvent_ptr);
#line 1373 "UecDirectTransfer.cpp"
  llvm_cbe_call463 = _ZN3Uec17UecDirectTransfer28sendRrcDlInformationTransferEPNS_8UecEventE(llvm_cbe_this1, llvm_cbe_tmp461);
#line 1373 "UecDirectTransfer.cpp"
  *(&llvm_cbe_l_success) = llvm_cbe_call463;
#line 1374 "UecDirectTransfer.cpp"
  goto llvm_cbe_if_2e_end488;

llvm_cbe_sw_2e_bb464:
#line 1380 "UecDirectTransfer.cpp"
  llvm_cbe_tmp465 = *(&llvm_cbe_l_uecEvent_ptr);
#line 1380 "UecDirectTransfer.cpp"
  llvm_cbe_call467 = _ZN3Uec17UecDirectTransfer24sendS1ApInitialUeMessageEPNS_8UecEventE(llvm_cbe_this1, llvm_cbe_tmp465);
#line 1380 "UecDirectTransfer.cpp"
  *(&llvm_cbe_l_success) = llvm_cbe_call467;
#line 1381 "UecDirectTransfer.cpp"
  goto llvm_cbe_if_2e_end488;

llvm_cbe_sw_2e_default468:
#line 1387 "UecDirectTransfer.cpp"
  llvm_cbe_call470 = _ZN11DummyStreamlsIPKcEERS_T_((&mout), ((&_OC_str81.array[((signed int )0u)])));
#line 1387 "UecDirectTransfer.cpp"
  llvm_cbe_call472 = _ZN11DummyStreamlsIPFRSt8ios_baseS2_EEERS_T_(llvm_cbe_call470, _ZSt3decRSt8ios_base);
#line 1387 "UecDirectTransfer.cpp"
  llvm_cbe_tmp473 = *(&llvm_cbe_l_uecEvent_ptr);
#line 1387 "UecDirectTransfer.cpp"
  llvm_cbe_call475 = _ZNK3Uec8UecEvent13getInstanceIdEv(llvm_cbe_tmp473);
#line 1387 "UecDirectTransfer.cpp"
  llvm_cbe_call477 = _ZN11DummyStreamlsIjEERS_T_(llvm_cbe_call472, llvm_cbe_call475);
#line 1387 "UecDirectTransfer.cpp"
  llvm_cbe_call479 = _ZN11DummyStreamlsIPKcEERS_T_(llvm_cbe_call477, ((&_OC_str93.array[((signed int )0u)])));
#line 1387 "UecDirectTransfer.cpp"
  llvm_cbe_call481 = _ZN11DummyStreamlsIPFRSt8ios_baseS2_EEERS_T_(llvm_cbe_call479, _ZSt3decRSt8ios_base);
#line 1387 "UecDirectTransfer.cpp"
  llvm_cbe_tmp482 = *(&llvm_cbe_l_payLoad_ptr);
#line 1387 "UecDirectTransfer.cpp"
  llvm_cbe_tmp484 = *((&llvm_cbe_tmp482->field0));
#line 1387 "UecDirectTransfer.cpp"
  llvm_cbe_call486 = _ZN11DummyStreamlsIN3Uec13EUecDirectionEEERS_T_(llvm_cbe_call481, llvm_cbe_tmp484);
#line 1390 "UecDirectTransfer.cpp"
  *(&llvm_cbe_l_success) = 0u;
#line 1392 "UecDirectTransfer.cpp"
  goto llvm_cbe_if_2e_end488;

llvm_cbe_if_2e_end488:
#line 1395 "UecDirectTransfer.cpp"
  llvm_cbe_tmp489 = *(&llvm_cbe_l_payLoad_ptr);
#line 1395 "UecDirectTransfer.cpp"
  llvm_cbe_tmp491 = *((&llvm_cbe_tmp489->field1));
#line 1395 "UecDirectTransfer.cpp"
  if (((((((bool )llvm_cbe_tmp491&1u))&1)) != 0)) {    goto llvm_cbe_land_2e_lhs_2e_true;  } else {    goto llvm_cbe_if_2e_end505;  }


llvm_cbe_land_2e_lhs_2e_true:
#line 1395 "UecDirectTransfer.cpp"
  llvm_cbe_tmp494 = *(&llvm_cbe_l_payLoad_ptr);
#line 1395 "UecDirectTransfer.cpp"
  llvm_cbe_tmp497 = *((&((&llvm_cbe_tmp494->field2))->field1));
#line 1395 "UecDirectTransfer.cpp"
  if ((((unsigned char *)/*NULL*/0) == llvm_cbe_tmp497)) {    goto llvm_cbe_if_2e_end505;  } else {    goto llvm_cbe_if_2e_then499;  }


llvm_cbe_if_2e_then499:
#line 1399 "UecDirectTransfer.cpp"
  llvm_cbe_tmp500 = *(&llvm_cbe_l_payLoad_ptr);
#line 1399 "UecDirectTransfer.cpp"
  llvm_cbe_tmp503 = *((&((&llvm_cbe_tmp500->field2))->field1));
#line 1399 "UecDirectTransfer.cpp"
  if ((llvm_cbe_tmp503 == ((unsigned char *)/*NULL*/0))) {    goto llvm_cbe_if_2e_end505;  } else {    goto llvm_cbe_delete_2e_notnull;  }


llvm_cbe_delete_2e_notnull:
#line 1399 "UecDirectTransfer.cpp"
  _ZdlPv(llvm_cbe_tmp503);
#line 0 "LLVM INTERNAL"
  goto llvm_cbe_if_2e_end505;

llvm_cbe_if_2e_end505:
#line 1402 "UecDirectTransfer.cpp"
  llvm_cbe_tmp506 = *(&llvm_cbe_l_success);
#line 1402 "UecDirectTransfer.cpp"
  _ZN3Uec14UecServiceBase15serviceFinishedEiNS_13EUecServiceIdE((((struct l_class_OC_Uec_KD__KD_UecServiceBase *)llvm_cbe_this1)), llvm_cbe_tmp506, 0u);
#line 1403 "UecDirectTransfer.cpp"
  *(&llvm_cbe_retval) = 1u;
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_cleanup_2e_dst) = 11u;
#line 1403 "UecDirectTransfer.cpp"
  goto llvm_cbe_cleanup;

llvm_cbe_sw_2e_default508:
#line 1411 "UecDirectTransfer.cpp"
  llvm_cbe_call510 = _ZN11DummyStreamlsIPKcEERS_T_((&mout), ((&_OC_str81.array[((signed int )0u)])));
#line 1411 "UecDirectTransfer.cpp"
  llvm_cbe_call512 = _ZN11DummyStreamlsIPFRSt8ios_baseS2_EEERS_T_(llvm_cbe_call510, _ZSt3decRSt8ios_base);
#line 1411 "UecDirectTransfer.cpp"
  llvm_cbe_tmp513 = *(&llvm_cbe_l_uecEvent_ptr);
#line 1411 "UecDirectTransfer.cpp"
  llvm_cbe_call515 = _ZNK3Uec8UecEvent13getInstanceIdEv(llvm_cbe_tmp513);
#line 1411 "UecDirectTransfer.cpp"
  llvm_cbe_call517 = _ZN11DummyStreamlsIjEERS_T_(llvm_cbe_call512, llvm_cbe_call515);
#line 1411 "UecDirectTransfer.cpp"
  llvm_cbe_call519 = _ZN11DummyStreamlsIPKcEERS_T_(llvm_cbe_call517, ((&_OC_str94.array[((signed int )0u)])));
#line 1411 "UecDirectTransfer.cpp"
  llvm_cbe_call521 = _ZN11DummyStreamlsIPFRSt8ios_baseS2_EEERS_T_(llvm_cbe_call519, _ZSt3hexRSt8ios_base);
#line 1411 "UecDirectTransfer.cpp"
  llvm_cbe_tmp522 = *(&llvm_cbe_l_uecEvent_ptr);
#line 1411 "UecDirectTransfer.cpp"
  llvm_cbe_call524 = _ZNK6CEvent10GetEventIdEv((((struct l_class_OC_CEvent *)llvm_cbe_tmp522)));
#line 1411 "UecDirectTransfer.cpp"
  llvm_cbe_call526 = _ZN11DummyStreamlsIjEERS_T_(llvm_cbe_call521, llvm_cbe_call524);
#line 1414 "UecDirectTransfer.cpp"
  _ZN3Uec14UecServiceBase15serviceFinishedEiNS_13EUecServiceIdE((((struct l_class_OC_Uec_KD__KD_UecServiceBase *)llvm_cbe_this1)), 0u, 0u);
#line 1415 "UecDirectTransfer.cpp"
  *(&llvm_cbe_retval) = 1u;
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_cleanup_2e_dst) = 7u;
#line 1415 "UecDirectTransfer.cpp"
  goto llvm_cbe_cleanup;

llvm_cbe_sw_2e_epilog528:
#line 1420 "UecDirectTransfer.cpp"
  llvm_cbe_tmp531 = *((&((&llvm_cbe_this1->field2))->field1));
#line 1420 "UecDirectTransfer.cpp"
  if ((llvm_cbe_tmp531 != ((unsigned char *)/*NULL*/0))) {    goto llvm_cbe_if_2e_then533;  } else {    goto llvm_cbe_if_2e_end543;  }


llvm_cbe_if_2e_then533:
#line 1422 "UecDirectTransfer.cpp"
  llvm_cbe_tmp536 = *((&((&llvm_cbe_this1->field2))->field1));
#line 1422 "UecDirectTransfer.cpp"
  if ((llvm_cbe_tmp536 == ((unsigned char *)/*NULL*/0))) {    goto llvm_cbe_delete_2e_end540;  } else {    goto llvm_cbe_delete_2e_notnull538;  }


llvm_cbe_delete_2e_notnull538:
#line 1422 "UecDirectTransfer.cpp"
  _ZdaPv(llvm_cbe_tmp536);
#line 0 "LLVM INTERNAL"
  goto llvm_cbe_delete_2e_end540;

llvm_cbe_delete_2e_end540:
#line 0 "LLVM INTERNAL"
  llvm_cbe_tmp__14 = memset((((unsigned char *)((&llvm_cbe_this1->field2)))), 0u, 16ull);
#line 1424 "UecDirectTransfer.cpp"
  goto llvm_cbe_if_2e_end543;

llvm_cbe_if_2e_end543:
#line 1426 "UecDirectTransfer.cpp"
  llvm_cbe_tmp544 = *(&llvm_cbe_l_success);
#line 1426 "UecDirectTransfer.cpp"
  _ZN3Uec14UecServiceBase15serviceFinishedEiNS_13EUecServiceIdE((((struct l_class_OC_Uec_KD__KD_UecServiceBase *)llvm_cbe_this1)), llvm_cbe_tmp544, 0u);
#line 1427 "UecDirectTransfer.cpp"
  *(&llvm_cbe_retval) = 1u;
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_cleanup_2e_dst) = 5u;
#line 1427 "UecDirectTransfer.cpp"
  goto llvm_cbe_cleanup;

llvm_cbe_cleanup_2e_pad:
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_cleanup_2e_dst563) = 1u;
#line 0 "LLVM INTERNAL"
  goto llvm_cbe_cleanup564;

llvm_cbe_cleanup_2e_pad546:
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_cleanup_2e_dst563) = 1u;
#line 0 "LLVM INTERNAL"
  goto llvm_cbe_cleanup564;

llvm_cbe_cleanup_2e_pad547:
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_cleanup_2e_dst563) = 1u;
#line 0 "LLVM INTERNAL"
  goto llvm_cbe_cleanup564;

llvm_cbe_cleanup_2e_pad548:
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_cleanup_2e_dst563) = 1u;
#line 0 "LLVM INTERNAL"
  goto llvm_cbe_cleanup564;

llvm_cbe_cleanup_2e_pad549:
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_cleanup_2e_dst563) = 1u;
#line 0 "LLVM INTERNAL"
  goto llvm_cbe_cleanup564;

llvm_cbe_cleanup_2e_pad550:
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_cleanup_2e_dst563) = 1u;
#line 0 "LLVM INTERNAL"
  goto llvm_cbe_cleanup564;

llvm_cbe_cleanup_2e_pad551:
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_cleanup_2e_dst563) = 1u;
#line 0 "LLVM INTERNAL"
  goto llvm_cbe_cleanup564;

llvm_cbe_cleanup_2e_pad552:
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_cleanup_2e_dst563) = 1u;
#line 0 "LLVM INTERNAL"
  goto llvm_cbe_cleanup564;

llvm_cbe_cleanup_2e_pad553:
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_cleanup_2e_dst563) = 1u;
#line 0 "LLVM INTERNAL"
  goto llvm_cbe_cleanup564;

llvm_cbe_cleanup_2e_pad554:
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_cleanup_2e_dst563) = 1u;
#line 0 "LLVM INTERNAL"
  goto llvm_cbe_cleanup564;

llvm_cbe_cleanup_2e_pad555:
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_cleanup_2e_dst563) = 1u;
#line 0 "LLVM INTERNAL"
  goto llvm_cbe_cleanup564;

llvm_cbe_cleanup_2e_pad556:
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_cleanup_2e_dst563) = 1u;
#line 0 "LLVM INTERNAL"
  goto llvm_cbe_cleanup564;

llvm_cbe_cleanup_2e_pad557:
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_cleanup_2e_dst563) = 1u;
#line 0 "LLVM INTERNAL"
  goto llvm_cbe_cleanup564;

llvm_cbe_cleanup_2e_pad558:
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_cleanup_2e_dst563) = 1u;
#line 0 "LLVM INTERNAL"
  goto llvm_cbe_cleanup564;

llvm_cbe_cleanup_2e_pad559:
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_cleanup_2e_dst563) = 1u;
#line 0 "LLVM INTERNAL"
  goto llvm_cbe_cleanup564;

llvm_cbe_cleanup_2e_pad560:
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_cleanup_2e_dst563) = 1u;
#line 0 "LLVM INTERNAL"
  goto llvm_cbe_cleanup564;

llvm_cbe_cleanup:
#line 1428 "UecDirectTransfer.cpp"
  _ZN3Uec17UecDebugCallstackD1Ev((&llvm_cbe__callstackTmp_));
#line 1428 "UecDirectTransfer.cpp"
  llvm_cbe_tmp561 = *(&llvm_cbe_cleanup_2e_dst);
#line 1428 "UecDirectTransfer.cpp"
  switch (llvm_cbe_tmp561) {
  default:
    goto llvm_cbe_cleanup_2e_end;;
  case 1u:
    goto llvm_cbe_cleanup_2e_pad;  case 2u:
    goto llvm_cbe_cleanup_2e_pad546;  case 3u:
    goto llvm_cbe_cleanup_2e_pad547;  case 4u:
    goto llvm_cbe_cleanup_2e_pad548;  case 5u:
    goto llvm_cbe_cleanup_2e_pad549;  case 6u:
    goto llvm_cbe_cleanup_2e_pad550;  case 7u:
    goto llvm_cbe_cleanup_2e_pad551;  case 8u:
    goto llvm_cbe_cleanup_2e_pad552;  case 9u:
    goto llvm_cbe_cleanup_2e_pad553;  case 10u:
    goto llvm_cbe_cleanup_2e_pad554;  case 11u:
    goto llvm_cbe_cleanup_2e_pad555;  case 12u:
    goto llvm_cbe_cleanup_2e_pad556;  case 13u:
    goto llvm_cbe_cleanup_2e_pad557;  case 14u:
    goto llvm_cbe_cleanup_2e_pad558;  case 15u:
    goto llvm_cbe_cleanup_2e_pad559;  case 16u:
    goto llvm_cbe_cleanup_2e_pad560;  }

llvm_cbe_cleanup_2e_end:
#line 1428 "UecDirectTransfer.cpp"
  *(&llvm_cbe_cleanup_2e_dst563) = 0u;
#line 1428 "UecDirectTransfer.cpp"
  goto llvm_cbe_cleanup564;

llvm_cbe_cleanup564:
#line 1428 "UecDirectTransfer.cpp"
  _ZNSsD1Ev((&llvm_cbe__currentFunction_));
#line 1428 "UecDirectTransfer.cpp"
  llvm_cbe_tmp565 = *(&llvm_cbe_cleanup_2e_dst563);
#line 1428 "UecDirectTransfer.cpp"
  llvm_cbe_tmp__15 = *(&llvm_cbe_retval);
#line 1428 "UecDirectTransfer.cpp"
  return llvm_cbe_tmp__15;
}


#line 0 "LLVM INTERNAL"
struct l_class_OC_Uec_KD__KD_UecManagerBase *_ZNK3Uec14UecServiceBase13getUecManagerEv(struct l_class_OC_Uec_KD__KD_UecServiceBase *llvm_cbe_this) {
  struct l_class_OC_Uec_KD__KD_UecManagerBase *llvm_cbe_retval;    /* Address-exposed local */
  struct l_class_OC_Uec_KD__KD_UecServiceBase *llvm_cbe_this_2e_addr;    /* Address-exposed local */
  struct l_class_OC_Uec_KD__KD_UecServiceBase *llvm_cbe_this1;
  struct l_class_OC_Uec_KD__KD_UecManagerBase *llvm_cbe_tmp2;
  struct l_class_OC_Uec_KD__KD_UecManagerBase *llvm_cbe_tmp__16;

#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_this_2e_addr) = llvm_cbe_this;
#line 0 "LLVM INTERNAL"
  llvm_cbe_this1 = *(&llvm_cbe_this_2e_addr);
#line 97 "UecServiceBase.hpp"
  llvm_cbe_tmp2 = *((&llvm_cbe_this1->field4));
#line 97 "UecServiceBase.hpp"
  *(&llvm_cbe_retval) = llvm_cbe_tmp2;
#line 97 "UecServiceBase.hpp"
  llvm_cbe_tmp__16 = *(&llvm_cbe_retval);
#line 97 "UecServiceBase.hpp"
  return llvm_cbe_tmp__16;
}


#line 0 "LLVM INTERNAL"
void _ZThn8_N3Uec17UecDirectTransferD0Ev(struct l_class_OC_Uec_KD__KD_UecDirectTransfer *llvm_cbe_this) {
  struct l_class_OC_Uec_KD__KD_UecDirectTransfer *llvm_cbe_this_2e_addr;    /* Address-exposed local */
  struct l_class_OC_Uec_KD__KD_UecDirectTransfer *llvm_cbe_this1;

#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_this_2e_addr) = llvm_cbe_this;
#line 0 "LLVM INTERNAL"
  llvm_cbe_this1 = *(&llvm_cbe_this_2e_addr);
#line 0 "LLVM INTERNAL"
  _ZN3Uec17UecDirectTransferD0Ev((((struct l_class_OC_Uec_KD__KD_UecDirectTransfer *)((&(((unsigned char *)llvm_cbe_this1))[((signed long long )18446744073709551608ull)])))));
#line 0 "LLVM INTERNAL"
  return;
}


#line 0 "LLVM INTERNAL"
void _ZN3Uec17UecDirectTransferD0Ev(struct l_class_OC_Uec_KD__KD_UecDirectTransfer *llvm_cbe_this) {
  struct l_class_OC_Uec_KD__KD_UecDirectTransfer *llvm_cbe_this_2e_addr;    /* Address-exposed local */
  struct l_class_OC_Uec_KD__KD_UecDirectTransfer *llvm_cbe_this1;

#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_this_2e_addr) = llvm_cbe_this;
#line 0 "LLVM INTERNAL"
  llvm_cbe_this1 = *(&llvm_cbe_this_2e_addr);
#line 0 "LLVM INTERNAL"
  _ZN3Uec17UecDirectTransferD2Ev(llvm_cbe_this1);
#line 0 "LLVM INTERNAL"
  _ZdlPv((((unsigned char *)llvm_cbe_this1)));
#line 96 "UecDirectTransfer.cpp"
  return;
}


#line 0 "LLVM INTERNAL"
void _ZThn8_N3Uec17UecDirectTransferD1Ev(struct l_class_OC_Uec_KD__KD_UecDirectTransfer *llvm_cbe_this) {
  struct l_class_OC_Uec_KD__KD_UecDirectTransfer *llvm_cbe_this_2e_addr;    /* Address-exposed local */
  struct l_class_OC_Uec_KD__KD_UecDirectTransfer *llvm_cbe_this1;

#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_this_2e_addr) = llvm_cbe_this;
#line 0 "LLVM INTERNAL"
  llvm_cbe_this1 = *(&llvm_cbe_this_2e_addr);
#line 0 "LLVM INTERNAL"
  _ZN3Uec17UecDirectTransferD2Ev((((struct l_class_OC_Uec_KD__KD_UecDirectTransfer *)((&(((unsigned char *)llvm_cbe_this1))[((signed long long )18446744073709551608ull)])))));
#line 0 "LLVM INTERNAL"
  return;
}


#line 0 "LLVM INTERNAL"
void _ZN3Uec17UecDirectTransferD2Ev(struct l_class_OC_Uec_KD__KD_UecDirectTransfer *llvm_cbe_this) {
  struct l_class_OC_Uec_KD__KD_UecDirectTransfer *llvm_cbe_this_2e_addr;    /* Address-exposed local */
  struct l_class_OC_Uec_KD__KD_UecDirectTransfer *llvm_cbe_this1;
  struct l_class_OC_CStateT *llvm_cbe_tmp2;
  void  (**llvm_cbe_tmp__17) (struct l_class_OC_CStateT *);
  void  (*llvm_cbe_tmp__18) (struct l_class_OC_CStateT *);

#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_this_2e_addr) = llvm_cbe_this;
#line 0 "LLVM INTERNAL"
  llvm_cbe_this1 = *(&llvm_cbe_this_2e_addr);
#line 0 "LLVM INTERNAL"
  *(((unsigned char ***)llvm_cbe_this1)) = ((&_ZTVN3Uec17UecDirectTransferE.array[((signed long long )2ull)]));
#line 0 "LLVM INTERNAL"
  *(((unsigned char ***)((&(((unsigned char *)llvm_cbe_this1))[((signed long long )8ull)])))) = ((&_ZTVN3Uec17UecDirectTransferE.array[((signed long long )13ull)]));
#line 95 "UecDirectTransfer.cpp"
  llvm_cbe_tmp2 = *((&llvm_cbe_this1->field1));
#line 95 "UecDirectTransfer.cpp"
  if ((llvm_cbe_tmp2 == ((struct l_class_OC_CStateT *)/*NULL*/0))) {    goto llvm_cbe_dtor_2e_epilogue;  } else {    goto llvm_cbe_delete_2e_notnull;  }


llvm_cbe_delete_2e_notnull:
#line 95 "UecDirectTransfer.cpp"
  llvm_cbe_tmp__17 = *(((void  (***) (struct l_class_OC_CStateT *))llvm_cbe_tmp2));
#line 95 "UecDirectTransfer.cpp"
  llvm_cbe_tmp__18 = *((&llvm_cbe_tmp__17[((signed long long )1ull)]));
#line 95 "UecDirectTransfer.cpp"
  llvm_cbe_tmp__18(llvm_cbe_tmp2);
#line 95 "UecDirectTransfer.cpp"
  goto llvm_cbe_dtor_2e_epilogue;

llvm_cbe_dtor_2e_epilogue:
#line 96 "UecDirectTransfer.cpp"
  _ZN3Uec14UecServiceBaseD2Ev((((struct l_class_OC_Uec_KD__KD_UecServiceBase *)llvm_cbe_this1)));
#line 96 "UecDirectTransfer.cpp"
  return;
}


#line 0 "LLVM INTERNAL"
unsigned int _ZN3Uec17UecDirectTransfer7doResetEv(struct l_class_OC_Uec_KD__KD_UecDirectTransfer *llvm_cbe_this) {
  unsigned int llvm_cbe_retval;    /* Address-exposed local */
  struct l_class_OC_Uec_KD__KD_UecDirectTransfer *llvm_cbe_this_2e_addr;    /* Address-exposed local */
  struct l_class_OC_Uec_KD__KD_UecDirectTransfer *llvm_cbe_this1;
  unsigned char *llvm_cbe_tmp3;
  unsigned char *llvm_cbe_tmp6;
  unsigned char *llvm_cbe_tmp__19;
  struct l_class_OC_CStateT *llvm_cbe_tmp9;
  unsigned int llvm_cbe_tmp__20;

#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_this_2e_addr) = llvm_cbe_this;
#line 0 "LLVM INTERNAL"
  llvm_cbe_this1 = *(&llvm_cbe_this_2e_addr);
#line 105 "UecDirectTransfer.cpp"
  llvm_cbe_tmp3 = *((&((&llvm_cbe_this1->field2))->field1));
#line 105 "UecDirectTransfer.cpp"
  if ((llvm_cbe_tmp3 != ((unsigned char *)/*NULL*/0))) {    goto llvm_cbe_if_2e_then;  } else {    goto llvm_cbe_if_2e_end;  }


llvm_cbe_if_2e_then:
#line 107 "UecDirectTransfer.cpp"
  llvm_cbe_tmp6 = *((&((&llvm_cbe_this1->field2))->field1));
#line 107 "UecDirectTransfer.cpp"
  if ((llvm_cbe_tmp6 == ((unsigned char *)/*NULL*/0))) {    goto llvm_cbe_if_2e_end;  } else {    goto llvm_cbe_delete_2e_notnull;  }


llvm_cbe_delete_2e_notnull:
#line 107 "UecDirectTransfer.cpp"
  _ZdaPv(llvm_cbe_tmp6);
#line 107 "UecDirectTransfer.cpp"
  goto llvm_cbe_if_2e_end;

llvm_cbe_if_2e_end:
#line 0 "LLVM INTERNAL"
  llvm_cbe_tmp__19 = memset((((unsigned char *)((&llvm_cbe_this1->field2)))), 0u, 16ull);
#line 112 "UecDirectTransfer.cpp"
  _ZN3Uec14UecServiceBase19setProcessingStatusEib((((struct l_class_OC_Uec_KD__KD_UecServiceBase *)llvm_cbe_this1)), 1u, 1);
#line 113 "UecDirectTransfer.cpp"
  llvm_cbe_tmp9 = *((&llvm_cbe_this1->field1));
#line 113 "UecDirectTransfer.cpp"
  _ZN6Common8CFsmBase15SetInitialStateEP10CStateBase((((struct l_class_OC_Common_KD__KD_CFsmBase *)((&(((unsigned char *)llvm_cbe_this1))[((signed long long )8ull)])))), (((struct l_class_OC_CStateBase *)llvm_cbe_tmp9)));
#line 114 "UecDirectTransfer.cpp"
  *(&llvm_cbe_retval) = 1u;
#line 115 "UecDirectTransfer.cpp"
  llvm_cbe_tmp__20 = *(&llvm_cbe_retval);
#line 115 "UecDirectTransfer.cpp"
  return llvm_cbe_tmp__20;
}


#line 0 "LLVM INTERNAL"
void _ZN3Uec14UecServiceBase19setProcessingStatusEib(struct l_class_OC_Uec_KD__KD_UecServiceBase *llvm_cbe_this, unsigned int llvm_cbe_p_state, bool llvm_cbe_isForcedMode) {
  struct l_class_OC_Uec_KD__KD_UecServiceBase *llvm_cbe_this_2e_addr;    /* Address-exposed local */
  unsigned int llvm_cbe_p_state_2e_addr;    /* Address-exposed local */
  unsigned char llvm_cbe_isForcedMode_2e_addr;    /* Address-exposed local */
  struct l_class_OC_Uec_KD__KD_UecServiceBase *llvm_cbe_this1;
  unsigned char llvm_cbe_tmp;
  unsigned int llvm_cbe_tmp3;
  unsigned int llvm_cbe_tmp4;

#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_this_2e_addr) = llvm_cbe_this;
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_p_state_2e_addr) = llvm_cbe_p_state;
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_isForcedMode_2e_addr) = (((unsigned char )(bool )llvm_cbe_isForcedMode));
#line 0 "LLVM INTERNAL"
  llvm_cbe_this1 = *(&llvm_cbe_this_2e_addr);
#line 114 "UecServiceBase.hpp"
  llvm_cbe_tmp = *(&llvm_cbe_isForcedMode_2e_addr);
#line 114 "UecServiceBase.hpp"
  if ((((((bool )llvm_cbe_tmp&1u))&1))) {    goto llvm_cbe_if_2e_then;  } else {    goto llvm_cbe_lor_2e_lhs_2e_false;  }


llvm_cbe_lor_2e_lhs_2e_false:
#line 114 "UecServiceBase.hpp"
  llvm_cbe_tmp3 = *((&llvm_cbe_this1->field10));
#line 114 "UecServiceBase.hpp"
  if ((llvm_cbe_tmp3 == 1u)) {    goto llvm_cbe_if_2e_then;  } else {    goto llvm_cbe_if_2e_end;  }


llvm_cbe_if_2e_then:
#line 115 "UecServiceBase.hpp"
  llvm_cbe_tmp4 = *(&llvm_cbe_p_state_2e_addr);
#line 115 "UecServiceBase.hpp"
  *((&llvm_cbe_this1->field10)) = llvm_cbe_tmp4;
#line 115 "UecServiceBase.hpp"
  return;

llvm_cbe_if_2e_end:
#line 115 "UecServiceBase.hpp"
  return;
}


#line 0 "LLVM INTERNAL"
unsigned int _ZNK3Uec17UecDirectTransfer30handleRrcDlInformationTransferEPNS_8UecEventE(struct l_class_OC_Uec_KD__KD_UecDirectTransfer *llvm_cbe_this, struct l_class_OC_Uec_KD__KD_UecEvent *llvm_cbe_p_uecEvent_ptr) {
  unsigned int llvm_cbe_retval;    /* Address-exposed local */
  struct l_class_OC_Uec_KD__KD_UecDirectTransfer *llvm_cbe_this_2e_addr;    /* Address-exposed local */
  struct l_class_OC_Uec_KD__KD_UecEvent *llvm_cbe_p_uecEvent_ptr_2e_addr;    /* Address-exposed local */
  struct l_class_OC_std_KD__KD_basic_string llvm_cbe__currentFunction_;    /* Address-exposed local */
  struct l_class_OC_BaseEvent llvm_cbe_agg_2e_tmp;    /* Address-exposed local */
  struct l_class_OC_Uec_KD__KD_UecDebugCallstack llvm_cbe__callstackTmp_;    /* Address-exposed local */
  struct l_struct_OC_SErrcDLInformationTransfer *llvm_cbe_l_asn1PayLoad_ptr;    /* Address-exposed local */
  unsigned int llvm_cbe_cleanup_2e_dst;    /* Address-exposed local */
  unsigned int llvm_cbe_cleanup_2e_dst186;    /* Address-exposed local */
  struct l_class_OC_Uec_KD__KD_UecDirectTransfer *llvm_cbe_this1;
  struct l_class_OC_Uec_KD__KD_UecEvent *llvm_cbe_tmp;
  unsigned char *llvm_cbe_call;
  struct l_struct_OC_SErrcDLInformationTransfer *llvm_cbe_tmp14;
  unsigned int llvm_cbe_tmp17;
  struct l_class_OC_BaseEvent *llvm_cbe_call19;
  struct l_class_OC_BaseEvent *llvm_cbe_call21;
  struct l_class_OC_Uec_KD__KD_UecEvent *llvm_cbe_tmp22;
  unsigned int llvm_cbe_call24;
  struct l_class_OC_BaseEvent *llvm_cbe_call26;
  struct l_class_OC_BaseEvent *llvm_cbe_call28;
  struct l_class_OC_BaseEvent *llvm_cbe_call30;
  struct l_struct_OC_SErrcDLInformationTransfer *llvm_cbe_tmp31;
  unsigned int llvm_cbe_tmp34;
  struct l_class_OC_BaseEvent *llvm_cbe_call36;
  struct l_struct_OC_SErrcDLInformationTransfer *llvm_cbe_tmp37;
  unsigned int llvm_cbe_tmp42;
  struct l_class_OC_BaseEvent *llvm_cbe_call46;
  struct l_class_OC_BaseEvent *llvm_cbe_call48;
  struct l_class_OC_Uec_KD__KD_UecEvent *llvm_cbe_tmp49;
  unsigned int llvm_cbe_call51;
  struct l_class_OC_BaseEvent *llvm_cbe_call53;
  struct l_class_OC_BaseEvent *llvm_cbe_call55;
  struct l_class_OC_BaseEvent *llvm_cbe_call57;
  struct l_struct_OC_SErrcDLInformationTransfer *llvm_cbe_tmp58;
  unsigned int llvm_cbe_tmp63;
  struct l_class_OC_BaseEvent *llvm_cbe_call65;
  struct l_struct_OC_SErrcDLInformationTransfer *llvm_cbe_tmp67;
  unsigned int llvm_cbe_tmp75;
  struct l_class_OC_BaseEvent *llvm_cbe_call79;
  struct l_class_OC_BaseEvent *llvm_cbe_call81;
  struct l_class_OC_Uec_KD__KD_UecEvent *llvm_cbe_tmp82;
  unsigned int llvm_cbe_call84;
  struct l_class_OC_BaseEvent *llvm_cbe_call86;
  struct l_class_OC_BaseEvent *llvm_cbe_call88;
  struct l_class_OC_BaseEvent *llvm_cbe_call90;
  struct l_struct_OC_SErrcDLInformationTransfer *llvm_cbe_tmp91;
  unsigned int llvm_cbe_tmp99;
  struct l_class_OC_BaseEvent *llvm_cbe_call101;
  struct l_struct_OC_SErrcDLInformationTransfer *llvm_cbe_tmp103;
  unsigned char *llvm_cbe_tmp113;
  struct l_struct_OC_SErrcDLInformationTransfer *llvm_cbe_tmp115;
  unsigned int llvm_cbe_tmp125;
  struct l_class_OC_BaseEvent *llvm_cbe_call129;
  struct l_class_OC_BaseEvent *llvm_cbe_call131;
  struct l_class_OC_Uec_KD__KD_UecEvent *llvm_cbe_tmp132;
  unsigned int llvm_cbe_call134;
  struct l_class_OC_BaseEvent *llvm_cbe_call136;
  struct l_class_OC_BaseEvent *llvm_cbe_call138;
  struct l_class_OC_BaseEvent *llvm_cbe_call140;
  struct l_class_OC_Uec_KD__KD_UecEvent *llvm_cbe_tmp141;
  unsigned int llvm_cbe_call143;
  struct l_class_OC_BaseEvent *llvm_cbe_call145;
  struct l_class_OC_BaseEvent *llvm_cbe_call147;
  struct l_class_OC_BaseEvent *llvm_cbe_call149;
  struct l_struct_OC_SErrcDLInformationTransfer *llvm_cbe_tmp150;
  unsigned int llvm_cbe_tmp160;
  struct l_class_OC_BaseEvent *llvm_cbe_call162;
  struct l_class_OC_BaseEvent *llvm_cbe_call164;
  struct l_class_OC_BaseEvent *llvm_cbe_call166;
  struct l_struct_OC_SErrcDLInformationTransfer *llvm_cbe_tmp167;
  unsigned char *llvm_cbe_tmp177;
  struct l_class_OC_BaseEvent *llvm_cbe_call179;
  unsigned int llvm_cbe_tmp185;
  unsigned int llvm_cbe_tmp188;
  unsigned int llvm_cbe_tmp__21;

#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_this_2e_addr) = llvm_cbe_this;
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_p_uecEvent_ptr_2e_addr) = llvm_cbe_p_uecEvent_ptr;
#line 0 "LLVM INTERNAL"
  llvm_cbe_this1 = *(&llvm_cbe_this_2e_addr);
#line 123 "UecDirectTransfer.cpp"
  _ZNSaIcEC1Ev((&llvm_cbe_agg_2e_tmp));
#line 123 "UecDirectTransfer.cpp"
  _ZNSsC1EPKcRKSaIcE((&llvm_cbe__currentFunction_), ((&__PRETTY_FUNCTION___OC__ZNK3Uec17UecDirectTransfer30handleRrcDlInformationTransferEPNS_8UecEventE.array[((signed int )0u)])), (&llvm_cbe_agg_2e_tmp));
#line 123 "UecDirectTransfer.cpp"
  _ZNSaIcED1Ev((&llvm_cbe_agg_2e_tmp));
#line 123 "UecDirectTransfer.cpp"
  _ZN3Uec17UecDebugCallstackC1ERKSs((&llvm_cbe__callstackTmp_), (&llvm_cbe__currentFunction_));
#line 125 "UecDirectTransfer.cpp"
  llvm_cbe_tmp = *(&llvm_cbe_p_uecEvent_ptr_2e_addr);
#line 125 "UecDirectTransfer.cpp"
  llvm_cbe_call = _ZNK3Uec8UecEvent14getAsn1PayLoadEv(llvm_cbe_tmp);
#line 125 "UecDirectTransfer.cpp"
  *(&llvm_cbe_l_asn1PayLoad_ptr) = (((struct l_struct_OC_SErrcDLInformationTransfer *)llvm_cbe_call));
#line 127 "UecDirectTransfer.cpp"
  llvm_cbe_tmp14 = *(&llvm_cbe_l_asn1PayLoad_ptr);
#line 127 "UecDirectTransfer.cpp"
  llvm_cbe_tmp17 = *((&((&llvm_cbe_tmp14->field4))->field0));
#line 127 "UecDirectTransfer.cpp"
  if ((0u == llvm_cbe_tmp17)) {    goto llvm_cbe_if_2e_end;  } else {    goto llvm_cbe_if_2e_then;  }


llvm_cbe_if_2e_then:
#line 129 "UecDirectTransfer.cpp"
  llvm_cbe_call19 = _ZN11DummyStreamlsIPKcEERS_T_((&mout), ((&_OC_str3.array[((signed int )0u)])));
#line 129 "UecDirectTransfer.cpp"
  llvm_cbe_call21 = _ZN11DummyStreamlsIPFRSt8ios_baseS2_EEERS_T_(llvm_cbe_call19, _ZSt3decRSt8ios_base);
#line 129 "UecDirectTransfer.cpp"
  llvm_cbe_tmp22 = *(&llvm_cbe_p_uecEvent_ptr_2e_addr);
#line 129 "UecDirectTransfer.cpp"
  llvm_cbe_call24 = _ZNK3Uec8UecEvent13getInstanceIdEv(llvm_cbe_tmp22);
#line 129 "UecDirectTransfer.cpp"
  llvm_cbe_call26 = _ZN11DummyStreamlsIjEERS_T_(llvm_cbe_call21, llvm_cbe_call24);
#line 129 "UecDirectTransfer.cpp"
  llvm_cbe_call28 = _ZN11DummyStreamlsIPKcEERS_T_(llvm_cbe_call26, ((&_OC_str4.array[((signed int )0u)])));
#line 129 "UecDirectTransfer.cpp"
  llvm_cbe_call30 = _ZN11DummyStreamlsIPFRSt8ios_baseS2_EEERS_T_(llvm_cbe_call28, _ZSt3decRSt8ios_base);
#line 129 "UecDirectTransfer.cpp"
  llvm_cbe_tmp31 = *(&llvm_cbe_l_asn1PayLoad_ptr);
#line 129 "UecDirectTransfer.cpp"
  llvm_cbe_tmp34 = *((&((&llvm_cbe_tmp31->field4))->field0));
#line 129 "UecDirectTransfer.cpp"
  llvm_cbe_call36 = _ZN11DummyStreamlsI49EDiscUErrcCriticalExtensionsDLInformationTransferEERS_T_(llvm_cbe_call30, llvm_cbe_tmp34);
#line 132 "UecDirectTransfer.cpp"
  *(&llvm_cbe_retval) = 4294967295u;
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_cleanup_2e_dst) = 1u;
#line 132 "UecDirectTransfer.cpp"
  goto llvm_cbe_cleanup;

llvm_cbe_if_2e_end:
#line 135 "UecDirectTransfer.cpp"
  llvm_cbe_tmp37 = *(&llvm_cbe_l_asn1PayLoad_ptr);
#line 135 "UecDirectTransfer.cpp"
  llvm_cbe_tmp42 = *((&((&((&((&llvm_cbe_tmp37->field4))->field1))->field0))->field0));
#line 135 "UecDirectTransfer.cpp"
  if ((0u == llvm_cbe_tmp42)) {    goto llvm_cbe_if_2e_end66;  } else {    goto llvm_cbe_if_2e_then44;  }


llvm_cbe_if_2e_then44:
#line 137 "UecDirectTransfer.cpp"
  llvm_cbe_call46 = _ZN11DummyStreamlsIPKcEERS_T_((&mout), ((&_OC_str3.array[((signed int )0u)])));
#line 137 "UecDirectTransfer.cpp"
  llvm_cbe_call48 = _ZN11DummyStreamlsIPFRSt8ios_baseS2_EEERS_T_(llvm_cbe_call46, _ZSt3decRSt8ios_base);
#line 137 "UecDirectTransfer.cpp"
  llvm_cbe_tmp49 = *(&llvm_cbe_p_uecEvent_ptr_2e_addr);
#line 137 "UecDirectTransfer.cpp"
  llvm_cbe_call51 = _ZNK3Uec8UecEvent13getInstanceIdEv(llvm_cbe_tmp49);
#line 137 "UecDirectTransfer.cpp"
  llvm_cbe_call53 = _ZN11DummyStreamlsIjEERS_T_(llvm_cbe_call48, llvm_cbe_call51);
#line 137 "UecDirectTransfer.cpp"
  llvm_cbe_call55 = _ZN11DummyStreamlsIPKcEERS_T_(llvm_cbe_call53, ((&_OC_str5.array[((signed int )0u)])));
#line 137 "UecDirectTransfer.cpp"
  llvm_cbe_call57 = _ZN11DummyStreamlsIPFRSt8ios_baseS2_EEERS_T_(llvm_cbe_call55, _ZSt3decRSt8ios_base);
#line 137 "UecDirectTransfer.cpp"
  llvm_cbe_tmp58 = *(&llvm_cbe_l_asn1PayLoad_ptr);
#line 137 "UecDirectTransfer.cpp"
  llvm_cbe_tmp63 = *((&((&((&((&llvm_cbe_tmp58->field4))->field1))->field0))->field0));
#line 137 "UecDirectTransfer.cpp"
  llvm_cbe_call65 = _ZN11DummyStreamlsI51EDiscUErrcC1CriticalExtensionsDLInformationTransferEERS_T_(llvm_cbe_call57, llvm_cbe_tmp63);
#line 142 "UecDirectTransfer.cpp"
  *(&llvm_cbe_retval) = 4294967295u;
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_cleanup_2e_dst) = 2u;
#line 142 "UecDirectTransfer.cpp"
  goto llvm_cbe_cleanup;

llvm_cbe_if_2e_end66:
#line 145 "UecDirectTransfer.cpp"
  llvm_cbe_tmp67 = *(&llvm_cbe_l_asn1PayLoad_ptr);
#line 145 "UecDirectTransfer.cpp"
  llvm_cbe_tmp75 = *((&((&((&((&((&((&((&llvm_cbe_tmp67->field4))->field1))->field0))->field1))->field0))->field0))->field0));
#line 145 "UecDirectTransfer.cpp"
  if ((0u == llvm_cbe_tmp75)) {    goto llvm_cbe_if_2e_end102;  } else {    goto llvm_cbe_if_2e_then77;  }


llvm_cbe_if_2e_then77:
#line 147 "UecDirectTransfer.cpp"
  llvm_cbe_call79 = _ZN11DummyStreamlsIPKcEERS_T_((&mout), ((&_OC_str3.array[((signed int )0u)])));
#line 147 "UecDirectTransfer.cpp"
  llvm_cbe_call81 = _ZN11DummyStreamlsIPFRSt8ios_baseS2_EEERS_T_(llvm_cbe_call79, _ZSt3decRSt8ios_base);
#line 147 "UecDirectTransfer.cpp"
  llvm_cbe_tmp82 = *(&llvm_cbe_p_uecEvent_ptr_2e_addr);
#line 147 "UecDirectTransfer.cpp"
  llvm_cbe_call84 = _ZNK3Uec8UecEvent13getInstanceIdEv(llvm_cbe_tmp82);
#line 147 "UecDirectTransfer.cpp"
  llvm_cbe_call86 = _ZN11DummyStreamlsIjEERS_T_(llvm_cbe_call81, llvm_cbe_call84);
#line 147 "UecDirectTransfer.cpp"
  llvm_cbe_call88 = _ZN11DummyStreamlsIPKcEERS_T_(llvm_cbe_call86, ((&_OC_str4.array[((signed int )0u)])));
#line 147 "UecDirectTransfer.cpp"
  llvm_cbe_call90 = _ZN11DummyStreamlsIPFRSt8ios_baseS2_EEERS_T_(llvm_cbe_call88, _ZSt3decRSt8ios_base);
#line 147 "UecDirectTransfer.cpp"
  llvm_cbe_tmp91 = *(&llvm_cbe_l_asn1PayLoad_ptr);
#line 147 "UecDirectTransfer.cpp"
  llvm_cbe_tmp99 = *((&((&((&((&((&((&((&llvm_cbe_tmp91->field4))->field1))->field0))->field1))->field0))->field0))->field0));
#line 147 "UecDirectTransfer.cpp"
  llvm_cbe_call101 = _ZN11DummyStreamlsI27EDiscUErrcDedicatedInfoTypeEERS_T_(llvm_cbe_call90, llvm_cbe_tmp99);
#line 152 "UecDirectTransfer.cpp"
  *(&llvm_cbe_retval) = 4294967295u;
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_cleanup_2e_dst) = 3u;
#line 152 "UecDirectTransfer.cpp"
  goto llvm_cbe_cleanup;

llvm_cbe_if_2e_end102:
#line 155 "UecDirectTransfer.cpp"
  llvm_cbe_tmp103 = *(&llvm_cbe_l_asn1PayLoad_ptr);
#line 155 "UecDirectTransfer.cpp"
  llvm_cbe_tmp113 = *((&((&((&((&((&((&((&((&((&llvm_cbe_tmp103->field4))->field1))->field0))->field1))->field0))->field0))->field1))->field0))->field1));
#line 155 "UecDirectTransfer.cpp"
  if ((((unsigned char *)/*NULL*/0) == llvm_cbe_tmp113)) {    goto llvm_cbe_if_2e_then127;  } else {    goto llvm_cbe_lor_2e_lhs_2e_false;  }


llvm_cbe_lor_2e_lhs_2e_false:
#line 155 "UecDirectTransfer.cpp"
  llvm_cbe_tmp115 = *(&llvm_cbe_l_asn1PayLoad_ptr);
#line 155 "UecDirectTransfer.cpp"
  llvm_cbe_tmp125 = *((&((&((&((&((&((&((&((&((&llvm_cbe_tmp115->field4))->field1))->field0))->field1))->field0))->field0))->field1))->field0))->field0));
#line 155 "UecDirectTransfer.cpp"
  if ((0u == llvm_cbe_tmp125)) {    goto llvm_cbe_if_2e_then127;  } else {    goto llvm_cbe_if_2e_end180;  }


llvm_cbe_if_2e_then127:
#line 159 "UecDirectTransfer.cpp"
  llvm_cbe_call129 = _ZN11DummyStreamlsIPKcEERS_T_((&mout), ((&_OC_str3.array[((signed int )0u)])));
#line 159 "UecDirectTransfer.cpp"
  llvm_cbe_call131 = _ZN11DummyStreamlsIPFRSt8ios_baseS2_EEERS_T_(llvm_cbe_call129, _ZSt3decRSt8ios_base);
#line 159 "UecDirectTransfer.cpp"
  llvm_cbe_tmp132 = *(&llvm_cbe_p_uecEvent_ptr_2e_addr);
#line 159 "UecDirectTransfer.cpp"
  llvm_cbe_call134 = _ZNK3Uec8UecEvent13getInstanceIdEv(llvm_cbe_tmp132);
#line 159 "UecDirectTransfer.cpp"
  llvm_cbe_call136 = _ZN11DummyStreamlsIjEERS_T_(llvm_cbe_call131, llvm_cbe_call134);
#line 159 "UecDirectTransfer.cpp"
  llvm_cbe_call138 = _ZN11DummyStreamlsIPKcEERS_T_(llvm_cbe_call136, ((&_OC_str6.array[((signed int )0u)])));
#line 159 "UecDirectTransfer.cpp"
  llvm_cbe_call140 = _ZN11DummyStreamlsIPFRSt8ios_baseS2_EEERS_T_(llvm_cbe_call138, _ZSt3hexRSt8ios_base);
#line 159 "UecDirectTransfer.cpp"
  llvm_cbe_tmp141 = *(&llvm_cbe_p_uecEvent_ptr_2e_addr);
#line 159 "UecDirectTransfer.cpp"
  llvm_cbe_call143 = _ZNK6CEvent10GetEventIdEv((((struct l_class_OC_CEvent *)llvm_cbe_tmp141)));
#line 159 "UecDirectTransfer.cpp"
  llvm_cbe_call145 = _ZN11DummyStreamlsIjEERS_T_(llvm_cbe_call140, llvm_cbe_call143);
#line 159 "UecDirectTransfer.cpp"
  llvm_cbe_call147 = _ZN11DummyStreamlsIPKcEERS_T_(llvm_cbe_call145, ((&_OC_str7.array[((signed int )0u)])));
#line 159 "UecDirectTransfer.cpp"
  llvm_cbe_call149 = _ZN11DummyStreamlsIPFRSt8ios_baseS2_EEERS_T_(llvm_cbe_call147, _ZSt3decRSt8ios_base);
#line 159 "UecDirectTransfer.cpp"
  llvm_cbe_tmp150 = *(&llvm_cbe_l_asn1PayLoad_ptr);
#line 159 "UecDirectTransfer.cpp"
  llvm_cbe_tmp160 = *((&((&((&((&((&((&((&((&((&llvm_cbe_tmp150->field4))->field1))->field0))->field1))->field0))->field0))->field1))->field0))->field0));
#line 159 "UecDirectTransfer.cpp"
  llvm_cbe_call162 = _ZN11DummyStreamlsIjEERS_T_(llvm_cbe_call149, llvm_cbe_tmp160);
#line 159 "UecDirectTransfer.cpp"
  llvm_cbe_call164 = _ZN11DummyStreamlsIPKcEERS_T_(llvm_cbe_call162, ((&_OC_str8.array[((signed int )0u)])));
#line 159 "UecDirectTransfer.cpp"
  llvm_cbe_call166 = _ZN11DummyStreamlsIPFRSt8ios_baseS2_EEERS_T_(llvm_cbe_call164, _ZSt3hexRSt8ios_base);
#line 159 "UecDirectTransfer.cpp"
  llvm_cbe_tmp167 = *(&llvm_cbe_l_asn1PayLoad_ptr);
#line 159 "UecDirectTransfer.cpp"
  llvm_cbe_tmp177 = *((&((&((&((&((&((&((&((&((&llvm_cbe_tmp167->field4))->field1))->field0))->field1))->field0))->field0))->field1))->field0))->field1));
#line 159 "UecDirectTransfer.cpp"
  llvm_cbe_call179 = _ZN11DummyStreamlsIPhEERS_T_(llvm_cbe_call166, llvm_cbe_tmp177);
#line 167 "UecDirectTransfer.cpp"
  *(&llvm_cbe_retval) = 4294967295u;
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_cleanup_2e_dst) = 4u;
#line 167 "UecDirectTransfer.cpp"
  goto llvm_cbe_cleanup;

llvm_cbe_if_2e_end180:
#line 170 "UecDirectTransfer.cpp"
  *(&llvm_cbe_retval) = 1u;
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_cleanup_2e_dst) = 5u;
#line 170 "UecDirectTransfer.cpp"
  goto llvm_cbe_cleanup;

llvm_cbe_cleanup_2e_pad:
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_cleanup_2e_dst186) = 1u;
#line 0 "LLVM INTERNAL"
  goto llvm_cbe_cleanup187;

llvm_cbe_cleanup_2e_pad181:
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_cleanup_2e_dst186) = 1u;
#line 0 "LLVM INTERNAL"
  goto llvm_cbe_cleanup187;

llvm_cbe_cleanup_2e_pad182:
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_cleanup_2e_dst186) = 1u;
#line 0 "LLVM INTERNAL"
  goto llvm_cbe_cleanup187;

llvm_cbe_cleanup_2e_pad183:
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_cleanup_2e_dst186) = 1u;
#line 0 "LLVM INTERNAL"
  goto llvm_cbe_cleanup187;

llvm_cbe_cleanup_2e_pad184:
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_cleanup_2e_dst186) = 1u;
#line 0 "LLVM INTERNAL"
  goto llvm_cbe_cleanup187;

llvm_cbe_cleanup:
#line 171 "UecDirectTransfer.cpp"
  _ZN3Uec17UecDebugCallstackD1Ev((&llvm_cbe__callstackTmp_));
#line 171 "UecDirectTransfer.cpp"
  llvm_cbe_tmp185 = *(&llvm_cbe_cleanup_2e_dst);
#line 171 "UecDirectTransfer.cpp"
  switch (llvm_cbe_tmp185) {
  default:
    goto llvm_cbe_cleanup_2e_end;;
  case 1u:
    goto llvm_cbe_cleanup_2e_pad;  case 2u:
    goto llvm_cbe_cleanup_2e_pad181;  case 3u:
    goto llvm_cbe_cleanup_2e_pad182;  case 4u:
    goto llvm_cbe_cleanup_2e_pad183;  case 5u:
    goto llvm_cbe_cleanup_2e_pad184;  }

llvm_cbe_cleanup_2e_end:
#line 171 "UecDirectTransfer.cpp"
  *(&llvm_cbe_cleanup_2e_dst186) = 0u;
#line 171 "UecDirectTransfer.cpp"
  goto llvm_cbe_cleanup187;

llvm_cbe_cleanup187:
#line 171 "UecDirectTransfer.cpp"
  _ZNSsD1Ev((&llvm_cbe__currentFunction_));
#line 171 "UecDirectTransfer.cpp"
  llvm_cbe_tmp188 = *(&llvm_cbe_cleanup_2e_dst186);
#line 171 "UecDirectTransfer.cpp"
  llvm_cbe_tmp__21 = *(&llvm_cbe_retval);
#line 171 "UecDirectTransfer.cpp"
  return llvm_cbe_tmp__21;
}


#line 0 "LLVM INTERNAL"
unsigned char *_ZNK3Uec8UecEvent14getAsn1PayLoadEv(struct l_class_OC_Uec_KD__KD_UecEvent *llvm_cbe_this) {
  unsigned char *llvm_cbe_retval;    /* Address-exposed local */
  struct l_class_OC_Uec_KD__KD_UecEvent *llvm_cbe_this_2e_addr;    /* Address-exposed local */
  struct l_class_OC_Uec_KD__KD_UecEvent *llvm_cbe_this1;
  unsigned char *llvm_cbe_tmp2;
  unsigned char *llvm_cbe_tmp__22;

#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_this_2e_addr) = llvm_cbe_this;
#line 0 "LLVM INTERNAL"
  llvm_cbe_this1 = *(&llvm_cbe_this_2e_addr);
#line 319 "UecEvent.hpp"
  llvm_cbe_tmp2 = *((&llvm_cbe_this1->field7));
#line 319 "UecEvent.hpp"
  *(&llvm_cbe_retval) = llvm_cbe_tmp2;
#line 319 "UecEvent.hpp"
  llvm_cbe_tmp__22 = *(&llvm_cbe_retval);
#line 319 "UecEvent.hpp"
  return llvm_cbe_tmp__22;
}


#line 0 "LLVM INTERNAL"
struct l_class_OC_BaseEvent *_ZN11DummyStreamlsIPKcEERS_T_(struct l_class_OC_BaseEvent *llvm_cbe_this, unsigned char *llvm_cbe_arg) {
  struct l_class_OC_BaseEvent *llvm_cbe_retval;    /* Address-exposed local */
  struct l_class_OC_BaseEvent *llvm_cbe_this_2e_addr;    /* Address-exposed local */
  unsigned char *llvm_cbe_arg_2e_addr;    /* Address-exposed local */
  struct l_class_OC_BaseEvent *llvm_cbe_this1;
  struct l_class_OC_BaseEvent *llvm_cbe_tmp__23;

#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_this_2e_addr) = llvm_cbe_this;
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_arg_2e_addr) = llvm_cbe_arg;
#line 0 "LLVM INTERNAL"
  llvm_cbe_this1 = *(&llvm_cbe_this_2e_addr);
#line 30 "BaseTypes.hpp"
  *(&llvm_cbe_retval) = llvm_cbe_this1;
#line 31 "BaseTypes.hpp"
  llvm_cbe_tmp__23 = *(&llvm_cbe_retval);
#line 31 "BaseTypes.hpp"
  return llvm_cbe_tmp__23;
}


#line 0 "LLVM INTERNAL"
struct l_class_OC_BaseEvent *_ZN11DummyStreamlsIPFRSt8ios_baseS2_EEERS_T_(struct l_class_OC_BaseEvent *llvm_cbe_this, struct l_class_OC_std_KD__KD_ios_base * (*llvm_cbe_arg) (struct l_class_OC_std_KD__KD_ios_base *)) {
  struct l_class_OC_BaseEvent *llvm_cbe_retval;    /* Address-exposed local */
  struct l_class_OC_BaseEvent *llvm_cbe_this_2e_addr;    /* Address-exposed local */
  struct l_class_OC_std_KD__KD_ios_base * (*llvm_cbe_arg_2e_addr) (struct l_class_OC_std_KD__KD_ios_base *);    /* Address-exposed local */
  struct l_class_OC_BaseEvent *llvm_cbe_this1;
  struct l_class_OC_BaseEvent *llvm_cbe_tmp__24;

#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_this_2e_addr) = llvm_cbe_this;
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_arg_2e_addr) = llvm_cbe_arg;
#line 0 "LLVM INTERNAL"
  llvm_cbe_this1 = *(&llvm_cbe_this_2e_addr);
#line 30 "BaseTypes.hpp"
  *(&llvm_cbe_retval) = llvm_cbe_this1;
#line 31 "BaseTypes.hpp"
  llvm_cbe_tmp__24 = *(&llvm_cbe_retval);
#line 31 "BaseTypes.hpp"
  return llvm_cbe_tmp__24;
}


#line 0 "LLVM INTERNAL"
struct l_class_OC_std_KD__KD_ios_base *_ZSt3decRSt8ios_base(struct l_class_OC_std_KD__KD_ios_base *llvm_cbe___base) {
  struct l_class_OC_std_KD__KD_ios_base *llvm_cbe_retval;    /* Address-exposed local */
  struct l_class_OC_std_KD__KD_ios_base *llvm_cbe___base_2e_addr;    /* Address-exposed local */
  struct l_class_OC_std_KD__KD_ios_base *llvm_cbe_tmp;
  unsigned int llvm_cbe_call;
  struct l_class_OC_std_KD__KD_ios_base *llvm_cbe_tmp1;
  struct l_class_OC_std_KD__KD_ios_base *llvm_cbe_tmp__25;

#line 0 "LLVM INTERNAL"
  *(&llvm_cbe___base_2e_addr) = llvm_cbe___base;
#line 938 "ios_base.h"
  llvm_cbe_tmp = *(&llvm_cbe___base_2e_addr);
#line 938 "ios_base.h"
  llvm_cbe_call = _ZNSt8ios_base4setfESt13_Ios_FmtflagsS0_(llvm_cbe_tmp, 2u, 74u);
#line 939 "ios_base.h"
  llvm_cbe_tmp1 = *(&llvm_cbe___base_2e_addr);
#line 939 "ios_base.h"
  *(&llvm_cbe_retval) = llvm_cbe_tmp1;
#line 940 "ios_base.h"
  llvm_cbe_tmp__25 = *(&llvm_cbe_retval);
#line 940 "ios_base.h"
  return llvm_cbe_tmp__25;
}


#line 0 "LLVM INTERNAL"
struct l_class_OC_BaseEvent *_ZN11DummyStreamlsIjEERS_T_(struct l_class_OC_BaseEvent *llvm_cbe_this, unsigned int llvm_cbe_arg) {
  struct l_class_OC_BaseEvent *llvm_cbe_retval;    /* Address-exposed local */
  struct l_class_OC_BaseEvent *llvm_cbe_this_2e_addr;    /* Address-exposed local */
  unsigned int llvm_cbe_arg_2e_addr;    /* Address-exposed local */
  struct l_class_OC_BaseEvent *llvm_cbe_this1;
  struct l_class_OC_BaseEvent *llvm_cbe_tmp__26;

#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_this_2e_addr) = llvm_cbe_this;
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_arg_2e_addr) = llvm_cbe_arg;
#line 0 "LLVM INTERNAL"
  llvm_cbe_this1 = *(&llvm_cbe_this_2e_addr);
#line 30 "BaseTypes.hpp"
  *(&llvm_cbe_retval) = llvm_cbe_this1;
#line 31 "BaseTypes.hpp"
  llvm_cbe_tmp__26 = *(&llvm_cbe_retval);
#line 31 "BaseTypes.hpp"
  return llvm_cbe_tmp__26;
}


#line 0 "LLVM INTERNAL"
unsigned int _ZNK3Uec8UecEvent13getInstanceIdEv(struct l_class_OC_Uec_KD__KD_UecEvent *llvm_cbe_this) {
  unsigned int llvm_cbe_retval;    /* Address-exposed local */
  struct l_class_OC_Uec_KD__KD_UecEvent *llvm_cbe_this_2e_addr;    /* Address-exposed local */
  struct l_class_OC_Uec_KD__KD_UecEvent *llvm_cbe_this1;
  unsigned int llvm_cbe_tmp2;
  unsigned int llvm_cbe_tmp__27;

#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_this_2e_addr) = llvm_cbe_this;
#line 0 "LLVM INTERNAL"
  llvm_cbe_this1 = *(&llvm_cbe_this_2e_addr);
#line 340 "UecEvent.hpp"
  llvm_cbe_tmp2 = *((&llvm_cbe_this1->field2));
#line 340 "UecEvent.hpp"
  *(&llvm_cbe_retval) = llvm_cbe_tmp2;
#line 340 "UecEvent.hpp"
  llvm_cbe_tmp__27 = *(&llvm_cbe_retval);
#line 340 "UecEvent.hpp"
  return llvm_cbe_tmp__27;
}


#line 0 "LLVM INTERNAL"
struct l_class_OC_BaseEvent *_ZN11DummyStreamlsI49EDiscUErrcCriticalExtensionsDLInformationTransferEERS_T_(struct l_class_OC_BaseEvent *llvm_cbe_this, unsigned int llvm_cbe_arg) {
  struct l_class_OC_BaseEvent *llvm_cbe_retval;    /* Address-exposed local */
  struct l_class_OC_BaseEvent *llvm_cbe_this_2e_addr;    /* Address-exposed local */
  unsigned int llvm_cbe_arg_2e_addr;    /* Address-exposed local */
  struct l_class_OC_BaseEvent *llvm_cbe_this1;
  struct l_class_OC_BaseEvent *llvm_cbe_tmp__28;

#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_this_2e_addr) = llvm_cbe_this;
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_arg_2e_addr) = llvm_cbe_arg;
#line 0 "LLVM INTERNAL"
  llvm_cbe_this1 = *(&llvm_cbe_this_2e_addr);
#line 30 "BaseTypes.hpp"
  *(&llvm_cbe_retval) = llvm_cbe_this1;
#line 31 "BaseTypes.hpp"
  llvm_cbe_tmp__28 = *(&llvm_cbe_retval);
#line 31 "BaseTypes.hpp"
  return llvm_cbe_tmp__28;
}


#line 0 "LLVM INTERNAL"
struct l_class_OC_BaseEvent *_ZN11DummyStreamlsI51EDiscUErrcC1CriticalExtensionsDLInformationTransferEERS_T_(struct l_class_OC_BaseEvent *llvm_cbe_this, unsigned int llvm_cbe_arg) {
  struct l_class_OC_BaseEvent *llvm_cbe_retval;    /* Address-exposed local */
  struct l_class_OC_BaseEvent *llvm_cbe_this_2e_addr;    /* Address-exposed local */
  unsigned int llvm_cbe_arg_2e_addr;    /* Address-exposed local */
  struct l_class_OC_BaseEvent *llvm_cbe_this1;
  struct l_class_OC_BaseEvent *llvm_cbe_tmp__29;

#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_this_2e_addr) = llvm_cbe_this;
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_arg_2e_addr) = llvm_cbe_arg;
#line 0 "LLVM INTERNAL"
  llvm_cbe_this1 = *(&llvm_cbe_this_2e_addr);
#line 30 "BaseTypes.hpp"
  *(&llvm_cbe_retval) = llvm_cbe_this1;
#line 31 "BaseTypes.hpp"
  llvm_cbe_tmp__29 = *(&llvm_cbe_retval);
#line 31 "BaseTypes.hpp"
  return llvm_cbe_tmp__29;
}


#line 0 "LLVM INTERNAL"
struct l_class_OC_BaseEvent *_ZN11DummyStreamlsI27EDiscUErrcDedicatedInfoTypeEERS_T_(struct l_class_OC_BaseEvent *llvm_cbe_this, unsigned int llvm_cbe_arg) {
  struct l_class_OC_BaseEvent *llvm_cbe_retval;    /* Address-exposed local */
  struct l_class_OC_BaseEvent *llvm_cbe_this_2e_addr;    /* Address-exposed local */
  unsigned int llvm_cbe_arg_2e_addr;    /* Address-exposed local */
  struct l_class_OC_BaseEvent *llvm_cbe_this1;
  struct l_class_OC_BaseEvent *llvm_cbe_tmp__30;

#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_this_2e_addr) = llvm_cbe_this;
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_arg_2e_addr) = llvm_cbe_arg;
#line 0 "LLVM INTERNAL"
  llvm_cbe_this1 = *(&llvm_cbe_this_2e_addr);
#line 30 "BaseTypes.hpp"
  *(&llvm_cbe_retval) = llvm_cbe_this1;
#line 31 "BaseTypes.hpp"
  llvm_cbe_tmp__30 = *(&llvm_cbe_retval);
#line 31 "BaseTypes.hpp"
  return llvm_cbe_tmp__30;
}


#line 0 "LLVM INTERNAL"
struct l_class_OC_std_KD__KD_ios_base *_ZSt3hexRSt8ios_base(struct l_class_OC_std_KD__KD_ios_base *llvm_cbe___base) {
  struct l_class_OC_std_KD__KD_ios_base *llvm_cbe_retval;    /* Address-exposed local */
  struct l_class_OC_std_KD__KD_ios_base *llvm_cbe___base_2e_addr;    /* Address-exposed local */
  struct l_class_OC_std_KD__KD_ios_base *llvm_cbe_tmp;
  unsigned int llvm_cbe_call;
  struct l_class_OC_std_KD__KD_ios_base *llvm_cbe_tmp1;
  struct l_class_OC_std_KD__KD_ios_base *llvm_cbe_tmp__31;

#line 0 "LLVM INTERNAL"
  *(&llvm_cbe___base_2e_addr) = llvm_cbe___base;
#line 946 "ios_base.h"
  llvm_cbe_tmp = *(&llvm_cbe___base_2e_addr);
#line 946 "ios_base.h"
  llvm_cbe_call = _ZNSt8ios_base4setfESt13_Ios_FmtflagsS0_(llvm_cbe_tmp, 8u, 74u);
#line 947 "ios_base.h"
  llvm_cbe_tmp1 = *(&llvm_cbe___base_2e_addr);
#line 947 "ios_base.h"
  *(&llvm_cbe_retval) = llvm_cbe_tmp1;
#line 948 "ios_base.h"
  llvm_cbe_tmp__31 = *(&llvm_cbe_retval);
#line 948 "ios_base.h"
  return llvm_cbe_tmp__31;
}


#line 0 "LLVM INTERNAL"
unsigned int _ZNK6CEvent10GetEventIdEv(struct l_class_OC_CEvent *llvm_cbe_this) {
  unsigned int llvm_cbe_retval;    /* Address-exposed local */
  struct l_class_OC_CEvent *llvm_cbe_this_2e_addr;    /* Address-exposed local */
  struct l_class_OC_CEvent *llvm_cbe_this1;
  unsigned int llvm_cbe_tmp2;
  unsigned int llvm_cbe_tmp__32;

#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_this_2e_addr) = llvm_cbe_this;
#line 0 "LLVM INTERNAL"
  llvm_cbe_this1 = *(&llvm_cbe_this_2e_addr);
#line 104 "CEvent.hpp"
  llvm_cbe_tmp2 = *((&llvm_cbe_this1->field2));
#line 104 "CEvent.hpp"
  *(&llvm_cbe_retval) = llvm_cbe_tmp2;
#line 104 "CEvent.hpp"
  llvm_cbe_tmp__32 = *(&llvm_cbe_retval);
#line 104 "CEvent.hpp"
  return llvm_cbe_tmp__32;
}


#line 0 "LLVM INTERNAL"
struct l_class_OC_BaseEvent *_ZN11DummyStreamlsIPhEERS_T_(struct l_class_OC_BaseEvent *llvm_cbe_this, unsigned char *llvm_cbe_arg) {
  struct l_class_OC_BaseEvent *llvm_cbe_retval;    /* Address-exposed local */
  struct l_class_OC_BaseEvent *llvm_cbe_this_2e_addr;    /* Address-exposed local */
  unsigned char *llvm_cbe_arg_2e_addr;    /* Address-exposed local */
  struct l_class_OC_BaseEvent *llvm_cbe_this1;
  struct l_class_OC_BaseEvent *llvm_cbe_tmp__33;

#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_this_2e_addr) = llvm_cbe_this;
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_arg_2e_addr) = llvm_cbe_arg;
#line 0 "LLVM INTERNAL"
  llvm_cbe_this1 = *(&llvm_cbe_this_2e_addr);
#line 30 "BaseTypes.hpp"
  *(&llvm_cbe_retval) = llvm_cbe_this1;
#line 31 "BaseTypes.hpp"
  llvm_cbe_tmp__33 = *(&llvm_cbe_retval);
#line 31 "BaseTypes.hpp"
  return llvm_cbe_tmp__33;
}


#line 0 "LLVM INTERNAL"
unsigned int _ZN3Uec17UecDirectTransfer30handleRrcUlInformationTransferEPNS_8UecEventE(struct l_class_OC_Uec_KD__KD_UecDirectTransfer *llvm_cbe_this, struct l_class_OC_Uec_KD__KD_UecEvent *llvm_cbe_p_uecEvent_ptr) {
  unsigned int llvm_cbe_retval;    /* Address-exposed local */
  struct l_class_OC_Uec_KD__KD_UecDirectTransfer *llvm_cbe_this_2e_addr;    /* Address-exposed local */
  struct l_class_OC_Uec_KD__KD_UecEvent *llvm_cbe_p_uecEvent_ptr_2e_addr;    /* Address-exposed local */
  struct l_class_OC_std_KD__KD_basic_string llvm_cbe__currentFunction_;    /* Address-exposed local */
  struct l_class_OC_BaseEvent llvm_cbe_agg_2e_tmp;    /* Address-exposed local */
  struct l_class_OC_Uec_KD__KD_UecDebugCallstack llvm_cbe__callstackTmp_;    /* Address-exposed local */
  struct l_struct_OC_SErrcULInformationTransfer *llvm_cbe_l_asn1PayLoad_ptr;    /* Address-exposed local */
  struct l_struct_OC_SAsnDynstr *llvm_cbe_l_rrcDedicatedInfoNAS_ptr;    /* Address-exposed local */
  unsigned int llvm_cbe_cleanup_2e_dst;    /* Address-exposed local */
  unsigned int llvm_cbe_cleanup_2e_dst150;    /* Address-exposed local */
  struct l_class_OC_Uec_KD__KD_UecDirectTransfer *llvm_cbe_this1;
  struct l_class_OC_Uec_KD__KD_UecEvent *llvm_cbe_tmp;
  unsigned char *llvm_cbe_call;
  struct l_struct_OC_SErrcULInformationTransfer *llvm_cbe_tmp14;
  unsigned int llvm_cbe_tmp17;
  struct l_struct_OC_SErrcULInformationTransfer *llvm_cbe_tmp18;
  unsigned int llvm_cbe_tmp23;
  struct l_class_OC_BaseEvent *llvm_cbe_call26;
  struct l_class_OC_BaseEvent *llvm_cbe_call28;
  struct l_class_OC_Uec_KD__KD_UecEvent *llvm_cbe_tmp29;
  unsigned int llvm_cbe_call31;
  struct l_class_OC_BaseEvent *llvm_cbe_call33;
  struct l_class_OC_BaseEvent *llvm_cbe_call35;
  struct l_class_OC_BaseEvent *llvm_cbe_call37;
  struct l_struct_OC_SErrcULInformationTransfer *llvm_cbe_tmp38;
  unsigned int llvm_cbe_tmp41;
  struct l_class_OC_BaseEvent *llvm_cbe_call43;
  struct l_class_OC_BaseEvent *llvm_cbe_call45;
  struct l_class_OC_BaseEvent *llvm_cbe_call47;
  struct l_struct_OC_SErrcULInformationTransfer *llvm_cbe_tmp48;
  unsigned int llvm_cbe_tmp53;
  struct l_class_OC_BaseEvent *llvm_cbe_call55;
  struct l_struct_OC_SErrcULInformationTransfer *llvm_cbe_tmp56;
  unsigned int llvm_cbe_tmp64;
  struct l_struct_OC_SErrcULInformationTransfer *llvm_cbe_tmp66;
  struct l_struct_OC_SAsnDynstr *llvm_cbe_tmp75;
  unsigned char *llvm_cbe_tmp77;
  struct l_struct_OC_SAsnDynstr *llvm_cbe_tmp79;
  unsigned int llvm_cbe_tmp81;
  struct l_class_OC_BaseEvent *llvm_cbe_call85;
  struct l_class_OC_BaseEvent *llvm_cbe_call87;
  struct l_class_OC_Uec_KD__KD_UecEvent *llvm_cbe_tmp88;
  unsigned int llvm_cbe_call90;
  struct l_class_OC_BaseEvent *llvm_cbe_call92;
  struct l_class_OC_BaseEvent *llvm_cbe_call94;
  struct l_class_OC_BaseEvent *llvm_cbe_call96;
  struct l_class_OC_Uec_KD__KD_UecEvent *llvm_cbe_tmp97;
  unsigned int llvm_cbe_call99;
  struct l_class_OC_BaseEvent *llvm_cbe_call101;
  struct l_class_OC_BaseEvent *llvm_cbe_call103;
  struct l_class_OC_BaseEvent *llvm_cbe_call105;
  struct l_struct_OC_SAsnDynstr *llvm_cbe_tmp106;
  unsigned int llvm_cbe_tmp108;
  struct l_class_OC_BaseEvent *llvm_cbe_call110;
  struct l_class_OC_BaseEvent *llvm_cbe_call112;
  struct l_class_OC_BaseEvent *llvm_cbe_call114;
  struct l_struct_OC_SAsnDynstr *llvm_cbe_tmp115;
  unsigned char *llvm_cbe_tmp117;
  struct l_class_OC_BaseEvent *llvm_cbe_call119;
  struct l_class_OC_BaseEvent *llvm_cbe_call122;
  struct l_class_OC_BaseEvent *llvm_cbe_call124;
  struct l_class_OC_Uec_KD__KD_UecManagerBase *llvm_cbe_call126;
  unsigned int llvm_cbe_call128;
  struct l_class_OC_BaseEvent *llvm_cbe_call130;
  struct l_class_OC_BaseEvent *llvm_cbe_call132;
  struct l_class_OC_BaseEvent *llvm_cbe_call134;
  struct l_struct_OC_SErrcULInformationTransfer *llvm_cbe_tmp135;
  unsigned int llvm_cbe_tmp143;
  struct l_class_OC_BaseEvent *llvm_cbe_call145;
  unsigned int llvm_cbe_tmp149;
  unsigned int llvm_cbe_tmp152;
  unsigned int llvm_cbe_tmp__34;

#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_this_2e_addr) = llvm_cbe_this;
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_p_uecEvent_ptr_2e_addr) = llvm_cbe_p_uecEvent_ptr;
#line 0 "LLVM INTERNAL"
  llvm_cbe_this1 = *(&llvm_cbe_this_2e_addr);
#line 179 "UecDirectTransfer.cpp"
  _ZNSaIcEC1Ev((&llvm_cbe_agg_2e_tmp));
#line 179 "UecDirectTransfer.cpp"
  _ZNSsC1EPKcRKSaIcE((&llvm_cbe__currentFunction_), ((&__PRETTY_FUNCTION___OC__ZN3Uec17UecDirectTransfer30handleRrcUlInformationTransferEPNS_8UecEventE.array[((signed int )0u)])), (&llvm_cbe_agg_2e_tmp));
#line 179 "UecDirectTransfer.cpp"
  _ZNSaIcED1Ev((&llvm_cbe_agg_2e_tmp));
#line 179 "UecDirectTransfer.cpp"
  _ZN3Uec17UecDebugCallstackC1ERKSs((&llvm_cbe__callstackTmp_), (&llvm_cbe__currentFunction_));
#line 182 "UecDirectTransfer.cpp"
  llvm_cbe_tmp = *(&llvm_cbe_p_uecEvent_ptr_2e_addr);
#line 182 "UecDirectTransfer.cpp"
  llvm_cbe_call = _ZNK3Uec8UecEvent14getAsn1PayLoadEv(llvm_cbe_tmp);
#line 182 "UecDirectTransfer.cpp"
  *(&llvm_cbe_l_asn1PayLoad_ptr) = (((struct l_struct_OC_SErrcULInformationTransfer *)llvm_cbe_call));
#line 184 "UecDirectTransfer.cpp"
  llvm_cbe_tmp14 = *(&llvm_cbe_l_asn1PayLoad_ptr);
#line 184 "UecDirectTransfer.cpp"
  llvm_cbe_tmp17 = *((&((&llvm_cbe_tmp14->field0))->field0));
#line 184 "UecDirectTransfer.cpp"
  if ((0u == llvm_cbe_tmp17)) {    goto llvm_cbe_land_2e_lhs_2e_true;  } else {    goto llvm_cbe_if_2e_then;  }


llvm_cbe_land_2e_lhs_2e_true:
#line 184 "UecDirectTransfer.cpp"
  llvm_cbe_tmp18 = *(&llvm_cbe_l_asn1PayLoad_ptr);
#line 184 "UecDirectTransfer.cpp"
  llvm_cbe_tmp23 = *((&((&((&((&llvm_cbe_tmp18->field0))->field1))->field0))->field0));
#line 184 "UecDirectTransfer.cpp"
  if ((0u == llvm_cbe_tmp23)) {    goto llvm_cbe_if_2e_end;  } else {    goto llvm_cbe_if_2e_then;  }


llvm_cbe_if_2e_then:
#line 187 "UecDirectTransfer.cpp"
  llvm_cbe_call26 = _ZN11DummyStreamlsIPKcEERS_T_((&mout), ((&_OC_str9.array[((signed int )0u)])));
#line 187 "UecDirectTransfer.cpp"
  llvm_cbe_call28 = _ZN11DummyStreamlsIPFRSt8ios_baseS2_EEERS_T_(llvm_cbe_call26, _ZSt3decRSt8ios_base);
#line 187 "UecDirectTransfer.cpp"
  llvm_cbe_tmp29 = *(&llvm_cbe_p_uecEvent_ptr_2e_addr);
#line 187 "UecDirectTransfer.cpp"
  llvm_cbe_call31 = _ZNK3Uec8UecEvent13getInstanceIdEv(llvm_cbe_tmp29);
#line 187 "UecDirectTransfer.cpp"
  llvm_cbe_call33 = _ZN11DummyStreamlsIjEERS_T_(llvm_cbe_call28, llvm_cbe_call31);
#line 187 "UecDirectTransfer.cpp"
  llvm_cbe_call35 = _ZN11DummyStreamlsIPKcEERS_T_(llvm_cbe_call33, ((&_OC_str10.array[((signed int )0u)])));
#line 187 "UecDirectTransfer.cpp"
  llvm_cbe_call37 = _ZN11DummyStreamlsIPFRSt8ios_baseS2_EEERS_T_(llvm_cbe_call35, _ZSt3decRSt8ios_base);
#line 187 "UecDirectTransfer.cpp"
  llvm_cbe_tmp38 = *(&llvm_cbe_l_asn1PayLoad_ptr);
#line 187 "UecDirectTransfer.cpp"
  llvm_cbe_tmp41 = *((&((&llvm_cbe_tmp38->field0))->field0));
#line 187 "UecDirectTransfer.cpp"
  llvm_cbe_call43 = _ZN11DummyStreamlsI49EDiscUErrcCriticalExtensionsULInformationTransferEERS_T_(llvm_cbe_call37, llvm_cbe_tmp41);
#line 187 "UecDirectTransfer.cpp"
  llvm_cbe_call45 = _ZN11DummyStreamlsIPKcEERS_T_(llvm_cbe_call43, ((&_OC_str11.array[((signed int )0u)])));
#line 187 "UecDirectTransfer.cpp"
  llvm_cbe_call47 = _ZN11DummyStreamlsIPFRSt8ios_baseS2_EEERS_T_(llvm_cbe_call45, _ZSt3decRSt8ios_base);
#line 187 "UecDirectTransfer.cpp"
  llvm_cbe_tmp48 = *(&llvm_cbe_l_asn1PayLoad_ptr);
#line 187 "UecDirectTransfer.cpp"
  llvm_cbe_tmp53 = *((&((&((&((&llvm_cbe_tmp48->field0))->field1))->field0))->field0));
#line 187 "UecDirectTransfer.cpp"
  llvm_cbe_call55 = _ZN11DummyStreamlsI51EDiscUErrcC1CriticalExtensionsULInformationTransferEERS_T_(llvm_cbe_call47, llvm_cbe_tmp53);
#line 192 "UecDirectTransfer.cpp"
  *(&llvm_cbe_retval) = 4294967295u;
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_cleanup_2e_dst) = 1u;
#line 192 "UecDirectTransfer.cpp"
  goto llvm_cbe_cleanup;

llvm_cbe_if_2e_end:
#line 195 "UecDirectTransfer.cpp"
  llvm_cbe_tmp56 = *(&llvm_cbe_l_asn1PayLoad_ptr);
#line 195 "UecDirectTransfer.cpp"
  llvm_cbe_tmp64 = *((&((&((&((&((&((&((&llvm_cbe_tmp56->field0))->field1))->field0))->field1))->field0))->field0))->field0));
#line 0 "LLVM INTERNAL"
  if ((llvm_cbe_tmp64 == 0u)) {    goto llvm_cbe_sw_2e_bb;  } else {    goto llvm_cbe_sw_2e_default;  }


llvm_cbe_sw_2e_bb:
#line 199 "UecDirectTransfer.cpp"
  llvm_cbe_tmp66 = *(&llvm_cbe_l_asn1PayLoad_ptr);
#line 199 "UecDirectTransfer.cpp"
  *(&llvm_cbe_l_rrcDedicatedInfoNAS_ptr) = ((&((&((&((&((&((&((&((&llvm_cbe_tmp66->field0))->field1))->field0))->field1))->field0))->field0))->field1))->field0));
#line 201 "UecDirectTransfer.cpp"
  llvm_cbe_tmp75 = *(&llvm_cbe_l_rrcDedicatedInfoNAS_ptr);
#line 201 "UecDirectTransfer.cpp"
  llvm_cbe_tmp77 = *((&llvm_cbe_tmp75->field1));
#line 201 "UecDirectTransfer.cpp"
  if ((((unsigned char *)/*NULL*/0) == llvm_cbe_tmp77)) {    goto llvm_cbe_if_2e_then83;  } else {    goto llvm_cbe_lor_2e_lhs_2e_false;  }


llvm_cbe_lor_2e_lhs_2e_false:
#line 201 "UecDirectTransfer.cpp"
  llvm_cbe_tmp79 = *(&llvm_cbe_l_rrcDedicatedInfoNAS_ptr);
#line 201 "UecDirectTransfer.cpp"
  llvm_cbe_tmp81 = *((&llvm_cbe_tmp79->field0));
#line 201 "UecDirectTransfer.cpp"
  if ((0u == llvm_cbe_tmp81)) {    goto llvm_cbe_if_2e_then83;  } else {    goto llvm_cbe_sw_2e_epilog;  }


llvm_cbe_if_2e_then83:
#line 205 "UecDirectTransfer.cpp"
  llvm_cbe_call85 = _ZN11DummyStreamlsIPKcEERS_T_((&mout), ((&_OC_str9.array[((signed int )0u)])));
#line 205 "UecDirectTransfer.cpp"
  llvm_cbe_call87 = _ZN11DummyStreamlsIPFRSt8ios_baseS2_EEERS_T_(llvm_cbe_call85, _ZSt3decRSt8ios_base);
#line 205 "UecDirectTransfer.cpp"
  llvm_cbe_tmp88 = *(&llvm_cbe_p_uecEvent_ptr_2e_addr);
#line 205 "UecDirectTransfer.cpp"
  llvm_cbe_call90 = _ZNK3Uec8UecEvent13getInstanceIdEv(llvm_cbe_tmp88);
#line 205 "UecDirectTransfer.cpp"
  llvm_cbe_call92 = _ZN11DummyStreamlsIjEERS_T_(llvm_cbe_call87, llvm_cbe_call90);
#line 205 "UecDirectTransfer.cpp"
  llvm_cbe_call94 = _ZN11DummyStreamlsIPKcEERS_T_(llvm_cbe_call92, ((&_OC_str6.array[((signed int )0u)])));
#line 205 "UecDirectTransfer.cpp"
  llvm_cbe_call96 = _ZN11DummyStreamlsIPFRSt8ios_baseS2_EEERS_T_(llvm_cbe_call94, _ZSt3hexRSt8ios_base);
#line 205 "UecDirectTransfer.cpp"
  llvm_cbe_tmp97 = *(&llvm_cbe_p_uecEvent_ptr_2e_addr);
#line 205 "UecDirectTransfer.cpp"
  llvm_cbe_call99 = _ZNK6CEvent10GetEventIdEv((((struct l_class_OC_CEvent *)llvm_cbe_tmp97)));
#line 205 "UecDirectTransfer.cpp"
  llvm_cbe_call101 = _ZN11DummyStreamlsIjEERS_T_(llvm_cbe_call96, llvm_cbe_call99);
#line 205 "UecDirectTransfer.cpp"
  llvm_cbe_call103 = _ZN11DummyStreamlsIPKcEERS_T_(llvm_cbe_call101, ((&_OC_str7.array[((signed int )0u)])));
#line 205 "UecDirectTransfer.cpp"
  llvm_cbe_call105 = _ZN11DummyStreamlsIPFRSt8ios_baseS2_EEERS_T_(llvm_cbe_call103, _ZSt3decRSt8ios_base);
#line 205 "UecDirectTransfer.cpp"
  llvm_cbe_tmp106 = *(&llvm_cbe_l_rrcDedicatedInfoNAS_ptr);
#line 205 "UecDirectTransfer.cpp"
  llvm_cbe_tmp108 = *((&llvm_cbe_tmp106->field0));
#line 205 "UecDirectTransfer.cpp"
  llvm_cbe_call110 = _ZN11DummyStreamlsIjEERS_T_(llvm_cbe_call105, llvm_cbe_tmp108);
#line 205 "UecDirectTransfer.cpp"
  llvm_cbe_call112 = _ZN11DummyStreamlsIPKcEERS_T_(llvm_cbe_call110, ((&_OC_str8.array[((signed int )0u)])));
#line 205 "UecDirectTransfer.cpp"
  llvm_cbe_call114 = _ZN11DummyStreamlsIPFRSt8ios_baseS2_EEERS_T_(llvm_cbe_call112, _ZSt3hexRSt8ios_base);
#line 205 "UecDirectTransfer.cpp"
  llvm_cbe_tmp115 = *(&llvm_cbe_l_rrcDedicatedInfoNAS_ptr);
#line 205 "UecDirectTransfer.cpp"
  llvm_cbe_tmp117 = *((&llvm_cbe_tmp115->field1));
#line 205 "UecDirectTransfer.cpp"
  llvm_cbe_call119 = _ZN11DummyStreamlsIPhEERS_T_(llvm_cbe_call114, llvm_cbe_tmp117);
#line 210 "UecDirectTransfer.cpp"
  *(&llvm_cbe_retval) = 4294967295u;
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_cleanup_2e_dst) = 2u;
#line 210 "UecDirectTransfer.cpp"
  goto llvm_cbe_cleanup;

llvm_cbe_sw_2e_default:
#line 218 "UecDirectTransfer.cpp"
  llvm_cbe_call122 = _ZN11DummyStreamlsIPKcEERS_T_((&mout), ((&_OC_str9.array[((signed int )0u)])));
#line 218 "UecDirectTransfer.cpp"
  llvm_cbe_call124 = _ZN11DummyStreamlsIPFRSt8ios_baseS2_EEERS_T_(llvm_cbe_call122, _ZSt3decRSt8ios_base);
#line 218 "UecDirectTransfer.cpp"
  llvm_cbe_call126 = _ZNK3Uec14UecServiceBase13getUecManagerEv((((struct l_class_OC_Uec_KD__KD_UecServiceBase *)llvm_cbe_this1)));
#line 218 "UecDirectTransfer.cpp"
  llvm_cbe_call128 = _ZNK3Uec14UecManagerBase13getInstanceIdEv(llvm_cbe_call126);
#line 218 "UecDirectTransfer.cpp"
  llvm_cbe_call130 = _ZN11DummyStreamlsIjEERS_T_(llvm_cbe_call124, llvm_cbe_call128);
#line 218 "UecDirectTransfer.cpp"
  llvm_cbe_call132 = _ZN11DummyStreamlsIPKcEERS_T_(llvm_cbe_call130, ((&_OC_str4.array[((signed int )0u)])));
#line 218 "UecDirectTransfer.cpp"
  llvm_cbe_call134 = _ZN11DummyStreamlsIPFRSt8ios_baseS2_EEERS_T_(llvm_cbe_call132, _ZSt3decRSt8ios_base);
#line 218 "UecDirectTransfer.cpp"
  llvm_cbe_tmp135 = *(&llvm_cbe_l_asn1PayLoad_ptr);
#line 218 "UecDirectTransfer.cpp"
  llvm_cbe_tmp143 = *((&((&((&((&((&((&((&llvm_cbe_tmp135->field0))->field1))->field0))->field1))->field0))->field0))->field0));
#line 218 "UecDirectTransfer.cpp"
  llvm_cbe_call145 = _ZN11DummyStreamlsI34EDiscUErrcDedicatedInformationTypeEERS_T_(llvm_cbe_call134, llvm_cbe_tmp143);
#line 221 "UecDirectTransfer.cpp"
  *(&llvm_cbe_retval) = 4294967295u;
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_cleanup_2e_dst) = 4u;
#line 221 "UecDirectTransfer.cpp"
  goto llvm_cbe_cleanup;

llvm_cbe_sw_2e_epilog:
#line 225 "UecDirectTransfer.cpp"
  *(&llvm_cbe_retval) = 1u;
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_cleanup_2e_dst) = 3u;
#line 225 "UecDirectTransfer.cpp"
  goto llvm_cbe_cleanup;

llvm_cbe_cleanup_2e_pad:
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_cleanup_2e_dst150) = 1u;
#line 0 "LLVM INTERNAL"
  goto llvm_cbe_cleanup151;

llvm_cbe_cleanup_2e_pad146:
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_cleanup_2e_dst150) = 1u;
#line 0 "LLVM INTERNAL"
  goto llvm_cbe_cleanup151;

llvm_cbe_cleanup_2e_pad147:
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_cleanup_2e_dst150) = 1u;
#line 0 "LLVM INTERNAL"
  goto llvm_cbe_cleanup151;

llvm_cbe_cleanup_2e_pad148:
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_cleanup_2e_dst150) = 1u;
#line 0 "LLVM INTERNAL"
  goto llvm_cbe_cleanup151;

llvm_cbe_cleanup:
#line 226 "UecDirectTransfer.cpp"
  _ZN3Uec17UecDebugCallstackD1Ev((&llvm_cbe__callstackTmp_));
#line 226 "UecDirectTransfer.cpp"
  llvm_cbe_tmp149 = *(&llvm_cbe_cleanup_2e_dst);
#line 226 "UecDirectTransfer.cpp"
  switch (llvm_cbe_tmp149) {
  default:
    goto llvm_cbe_cleanup_2e_end;;
  case 1u:
    goto llvm_cbe_cleanup_2e_pad;  case 2u:
    goto llvm_cbe_cleanup_2e_pad146;  case 3u:
    goto llvm_cbe_cleanup_2e_pad147;  case 4u:
    goto llvm_cbe_cleanup_2e_pad148;  }

llvm_cbe_cleanup_2e_end:
#line 226 "UecDirectTransfer.cpp"
  *(&llvm_cbe_cleanup_2e_dst150) = 0u;
#line 226 "UecDirectTransfer.cpp"
  goto llvm_cbe_cleanup151;

llvm_cbe_cleanup151:
#line 226 "UecDirectTransfer.cpp"
  _ZNSsD1Ev((&llvm_cbe__currentFunction_));
#line 226 "UecDirectTransfer.cpp"
  llvm_cbe_tmp152 = *(&llvm_cbe_cleanup_2e_dst150);
#line 226 "UecDirectTransfer.cpp"
  llvm_cbe_tmp__34 = *(&llvm_cbe_retval);
#line 226 "UecDirectTransfer.cpp"
  return llvm_cbe_tmp__34;
}


#line 0 "LLVM INTERNAL"
struct l_class_OC_BaseEvent *_ZN11DummyStreamlsI49EDiscUErrcCriticalExtensionsULInformationTransferEERS_T_(struct l_class_OC_BaseEvent *llvm_cbe_this, unsigned int llvm_cbe_arg) {
  struct l_class_OC_BaseEvent *llvm_cbe_retval;    /* Address-exposed local */
  struct l_class_OC_BaseEvent *llvm_cbe_this_2e_addr;    /* Address-exposed local */
  unsigned int llvm_cbe_arg_2e_addr;    /* Address-exposed local */
  struct l_class_OC_BaseEvent *llvm_cbe_this1;
  struct l_class_OC_BaseEvent *llvm_cbe_tmp__35;

#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_this_2e_addr) = llvm_cbe_this;
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_arg_2e_addr) = llvm_cbe_arg;
#line 0 "LLVM INTERNAL"
  llvm_cbe_this1 = *(&llvm_cbe_this_2e_addr);
#line 30 "BaseTypes.hpp"
  *(&llvm_cbe_retval) = llvm_cbe_this1;
#line 31 "BaseTypes.hpp"
  llvm_cbe_tmp__35 = *(&llvm_cbe_retval);
#line 31 "BaseTypes.hpp"
  return llvm_cbe_tmp__35;
}


#line 0 "LLVM INTERNAL"
struct l_class_OC_BaseEvent *_ZN11DummyStreamlsI51EDiscUErrcC1CriticalExtensionsULInformationTransferEERS_T_(struct l_class_OC_BaseEvent *llvm_cbe_this, unsigned int llvm_cbe_arg) {
  struct l_class_OC_BaseEvent *llvm_cbe_retval;    /* Address-exposed local */
  struct l_class_OC_BaseEvent *llvm_cbe_this_2e_addr;    /* Address-exposed local */
  unsigned int llvm_cbe_arg_2e_addr;    /* Address-exposed local */
  struct l_class_OC_BaseEvent *llvm_cbe_this1;
  struct l_class_OC_BaseEvent *llvm_cbe_tmp__36;

#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_this_2e_addr) = llvm_cbe_this;
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_arg_2e_addr) = llvm_cbe_arg;
#line 0 "LLVM INTERNAL"
  llvm_cbe_this1 = *(&llvm_cbe_this_2e_addr);
#line 30 "BaseTypes.hpp"
  *(&llvm_cbe_retval) = llvm_cbe_this1;
#line 31 "BaseTypes.hpp"
  llvm_cbe_tmp__36 = *(&llvm_cbe_retval);
#line 31 "BaseTypes.hpp"
  return llvm_cbe_tmp__36;
}


#line 0 "LLVM INTERNAL"
unsigned int _ZNK3Uec14UecManagerBase13getInstanceIdEv(struct l_class_OC_Uec_KD__KD_UecManagerBase *llvm_cbe_this) {
  unsigned int llvm_cbe_retval;    /* Address-exposed local */
  struct l_class_OC_Uec_KD__KD_UecManagerBase *llvm_cbe_this_2e_addr;    /* Address-exposed local */
  struct l_class_OC_Uec_KD__KD_UecManagerBase *llvm_cbe_this1;
  unsigned int llvm_cbe_tmp2;
  unsigned int llvm_cbe_tmp__37;

#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_this_2e_addr) = llvm_cbe_this;
#line 0 "LLVM INTERNAL"
  llvm_cbe_this1 = *(&llvm_cbe_this_2e_addr);
#line 250 "UecManagerBase.hpp"
  llvm_cbe_tmp2 = *((&llvm_cbe_this1->field5));
#line 250 "UecManagerBase.hpp"
  *(&llvm_cbe_retval) = llvm_cbe_tmp2;
#line 251 "UecManagerBase.hpp"
  llvm_cbe_tmp__37 = *(&llvm_cbe_retval);
#line 251 "UecManagerBase.hpp"
  return llvm_cbe_tmp__37;
}


#line 0 "LLVM INTERNAL"
struct l_class_OC_BaseEvent *_ZN11DummyStreamlsI34EDiscUErrcDedicatedInformationTypeEERS_T_(struct l_class_OC_BaseEvent *llvm_cbe_this, unsigned int llvm_cbe_arg) {
  struct l_class_OC_BaseEvent *llvm_cbe_retval;    /* Address-exposed local */
  struct l_class_OC_BaseEvent *llvm_cbe_this_2e_addr;    /* Address-exposed local */
  unsigned int llvm_cbe_arg_2e_addr;    /* Address-exposed local */
  struct l_class_OC_BaseEvent *llvm_cbe_this1;
  struct l_class_OC_BaseEvent *llvm_cbe_tmp__38;

#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_this_2e_addr) = llvm_cbe_this;
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_arg_2e_addr) = llvm_cbe_arg;
#line 0 "LLVM INTERNAL"
  llvm_cbe_this1 = *(&llvm_cbe_this_2e_addr);
#line 30 "BaseTypes.hpp"
  *(&llvm_cbe_retval) = llvm_cbe_this1;
#line 31 "BaseTypes.hpp"
  llvm_cbe_tmp__38 = *(&llvm_cbe_retval);
#line 31 "BaseTypes.hpp"
  return llvm_cbe_tmp__38;
}


#line 0 "LLVM INTERNAL"
unsigned int _ZN3Uec17UecDirectTransfer24handleS1ApDlNasTransportEPNS_8UecEventE(struct l_class_OC_Uec_KD__KD_UecDirectTransfer *llvm_cbe_this, struct l_class_OC_Uec_KD__KD_UecEvent *llvm_cbe_p_uecEvent_ptr) {
  unsigned int llvm_cbe_retval;    /* Address-exposed local */
  struct l_class_OC_Uec_KD__KD_UecDirectTransfer *llvm_cbe_this_2e_addr;    /* Address-exposed local */
  struct l_class_OC_Uec_KD__KD_UecEvent *llvm_cbe_p_uecEvent_ptr_2e_addr;    /* Address-exposed local */
  struct l_class_OC_std_KD__KD_basic_string llvm_cbe__currentFunction_;    /* Address-exposed local */
  struct l_class_OC_BaseEvent llvm_cbe_agg_2e_tmp;    /* Address-exposed local */
  struct l_class_OC_Uec_KD__KD_UecDebugCallstack llvm_cbe__callstackTmp_;    /* Address-exposed local */
  struct l_struct_OC_SS1apUFDownlinkNASTransport *llvm_cbe_l_asn1PayLoad_ptr;    /* Address-exposed local */
  struct l_class_OC_Uec_KD__KD_UecUeContextData *llvm_cbe_l_uecUeContextDataWrite_ptr;    /* Address-exposed local */
  unsigned int llvm_cbe_cleanup_2e_dst;    /* Address-exposed local */
  unsigned int llvm_cbe_cleanup_2e_dst226;    /* Address-exposed local */
  struct l_class_OC_Uec_KD__KD_UecDirectTransfer *llvm_cbe_this1;
  struct l_class_OC_Uec_KD__KD_UecEvent *llvm_cbe_tmp;
  unsigned char *llvm_cbe_call;
  struct l_class_OC_Uec_KD__KD_UecBaseData *llvm_cbe_call16;
  struct l_class_OC_Uec_KD__KD_UecBaseData * (**llvm_cbe_tmp__39) (struct l_class_OC_Uec_KD__KD_UecBaseData *, unsigned int );
  struct l_class_OC_Uec_KD__KD_UecBaseData * (*llvm_cbe_tmp__40) (struct l_class_OC_Uec_KD__KD_UecBaseData *, unsigned int );
  struct l_class_OC_Uec_KD__KD_UecBaseData *llvm_cbe_call18;
  struct l_class_OC_Uec_KD__KD_UecUeContextData *llvm_cbe_tmp19;
  bool llvm_cbe_call21;
  struct l_class_OC_Uec_KD__KD_UecUeContextData *llvm_cbe_tmp22;
  struct l_struct_OC_SS1apUFDownlinkNASTransport *llvm_cbe_tmp23;
  unsigned long long llvm_cbe_tmp25;
  struct l_class_OC_BaseEvent *llvm_cbe_call28;
  struct l_class_OC_BaseEvent *llvm_cbe_call30;
  struct l_class_OC_Uec_KD__KD_UecEvent *llvm_cbe_tmp31;
  unsigned int llvm_cbe_call33;
  struct l_class_OC_BaseEvent *llvm_cbe_call35;
  struct l_class_OC_BaseEvent *llvm_cbe_call37;
  struct l_class_OC_BaseEvent *llvm_cbe_call39;
  struct l_struct_OC_SS1apUFDownlinkNASTransport *llvm_cbe_tmp40;
  unsigned int llvm_cbe_tmp42;
  struct l_class_OC_BaseEvent *llvm_cbe_call44;
  struct l_class_OC_BaseEvent *llvm_cbe_call46;
  struct l_class_OC_BaseEvent *llvm_cbe_call48;
  struct l_struct_OC_SS1apUFDownlinkNASTransport *llvm_cbe_tmp49;
  unsigned long long llvm_cbe_tmp51;
  struct l_class_OC_BaseEvent *llvm_cbe_call53;
  struct l_class_OC_Uec_KD__KD_UecUeContextData *llvm_cbe_tmp54;
  unsigned int llvm_cbe_call56;
  struct l_class_OC_Uec_KD__KD_UecBaseData *llvm_cbe_call59;
  struct l_class_OC_Uec_KD__KD_UecTimePort * (**llvm_cbe_tmp__41) (struct l_class_OC_Uec_KD__KD_UecBaseData *);
  struct l_class_OC_Uec_KD__KD_UecTimePort * (*llvm_cbe_tmp__42) (struct l_class_OC_Uec_KD__KD_UecBaseData *);
  struct l_class_OC_Uec_KD__KD_UecTimePort *llvm_cbe_call62;
  struct l_class_OC_Uec_KD__KD_UecUeContextData *llvm_cbe_tmp63;
  unsigned int llvm_cbe_call65;
  unsigned int llvm_cbe_call67;
  struct l_class_OC_BaseEvent *llvm_cbe_call69;
  struct l_class_OC_BaseEvent *llvm_cbe_call71;
  struct l_class_OC_Uec_KD__KD_UecManagerBase *llvm_cbe_call73;
  unsigned int llvm_cbe_call75;
  struct l_class_OC_BaseEvent *llvm_cbe_call77;
  struct l_class_OC_BaseEvent *llvm_cbe_call79;
  struct l_class_OC_BaseEvent *llvm_cbe_call81;
  struct l_class_OC_Uec_KD__KD_UecUeContextData *llvm_cbe_tmp82;
  unsigned int llvm_cbe_call84;
  struct l_class_OC_BaseEvent *llvm_cbe_call86;
  struct l_class_OC_Uec_KD__KD_UecUeContextData *llvm_cbe_tmp87;
  struct l_struct_OC_SS1apUFDownlinkNASTransport *llvm_cbe_tmp90;
  unsigned int llvm_cbe_tmp92;
  struct l_class_OC_Uec_KD__KD_UecUeContextData *llvm_cbe_tmp93;
  unsigned int llvm_cbe_call95;
  struct l_struct_OC_SS1apUFDownlinkNASTransport *llvm_cbe_tmp97;
  unsigned long long llvm_cbe_tmp99;
  struct l_class_OC_Uec_KD__KD_UecUeContextData *llvm_cbe_tmp100;
  unsigned int llvm_cbe_call102;
  struct l_class_OC_BaseEvent *llvm_cbe_call107;
  struct l_class_OC_BaseEvent *llvm_cbe_call109;
  struct l_class_OC_Uec_KD__KD_UecEvent *llvm_cbe_tmp110;
  unsigned int llvm_cbe_call112;
  struct l_class_OC_BaseEvent *llvm_cbe_call114;
  struct l_class_OC_BaseEvent *llvm_cbe_call116;
  struct l_class_OC_BaseEvent *llvm_cbe_call118;
  struct l_struct_OC_SS1apUFDownlinkNASTransport *llvm_cbe_tmp119;
  unsigned int llvm_cbe_tmp121;
  struct l_class_OC_BaseEvent *llvm_cbe_call123;
  struct l_class_OC_BaseEvent *llvm_cbe_call125;
  struct l_class_OC_BaseEvent *llvm_cbe_call127;
  struct l_class_OC_Uec_KD__KD_UecUeContextData *llvm_cbe_tmp128;
  unsigned int llvm_cbe_call130;
  struct l_class_OC_BaseEvent *llvm_cbe_call132;
  struct l_class_OC_BaseEvent *llvm_cbe_call134;
  struct l_class_OC_BaseEvent *llvm_cbe_call136;
  struct l_struct_OC_SS1apUFDownlinkNASTransport *llvm_cbe_tmp137;
  unsigned long long llvm_cbe_tmp139;
  struct l_class_OC_BaseEvent *llvm_cbe_call141;
  struct l_class_OC_BaseEvent *llvm_cbe_call143;
  struct l_class_OC_BaseEvent *llvm_cbe_call145;
  struct l_class_OC_Uec_KD__KD_UecUeContextData *llvm_cbe_tmp146;
  unsigned int llvm_cbe_call148;
  struct l_class_OC_BaseEvent *llvm_cbe_call150;
  struct l_class_OC_BaseEvent *llvm_cbe_call152;
  struct l_struct_OC_SS1apUFDownlinkNASTransport *llvm_cbe_tmp154;
  unsigned char *llvm_cbe_tmp157;
  struct l_struct_OC_SS1apUFDownlinkNASTransport *llvm_cbe_tmp160;
  unsigned int llvm_cbe_tmp163;
  struct l_class_OC_BaseEvent *llvm_cbe_call167;
  struct l_class_OC_BaseEvent *llvm_cbe_call169;
  struct l_class_OC_Uec_KD__KD_UecEvent *llvm_cbe_tmp170;
  unsigned int llvm_cbe_call172;
  struct l_class_OC_BaseEvent *llvm_cbe_call174;
  struct l_class_OC_BaseEvent *llvm_cbe_call176;
  struct l_class_OC_BaseEvent *llvm_cbe_call178;
  struct l_class_OC_Uec_KD__KD_UecEvent *llvm_cbe_tmp179;
  unsigned int llvm_cbe_call181;
  struct l_class_OC_BaseEvent *llvm_cbe_call183;
  struct l_class_OC_BaseEvent *llvm_cbe_call185;
  struct l_class_OC_BaseEvent *llvm_cbe_call187;
  struct l_struct_OC_SS1apUFDownlinkNASTransport *llvm_cbe_tmp188;
  unsigned int llvm_cbe_tmp191;
  struct l_class_OC_BaseEvent *llvm_cbe_call193;
  struct l_class_OC_BaseEvent *llvm_cbe_call195;
  struct l_class_OC_BaseEvent *llvm_cbe_call197;
  struct l_struct_OC_SS1apUFDownlinkNASTransport *llvm_cbe_tmp198;
  unsigned char *llvm_cbe_tmp201;
  struct l_class_OC_BaseEvent *llvm_cbe_call203;
  struct l_struct_OC_SS1apUFDownlinkNASTransport *llvm_cbe_tmp205;
  struct l_class_OC_Uec_KD__KD_UecUeContextData *llvm_cbe_tmp206;
  unsigned int llvm_cbe_call208;
  struct l_class_OC_BaseEvent *llvm_cbe_call212;
  struct l_class_OC_BaseEvent *llvm_cbe_call214;
  struct l_class_OC_Uec_KD__KD_UecEvent *llvm_cbe_tmp215;
  unsigned int llvm_cbe_call217;
  struct l_class_OC_BaseEvent *llvm_cbe_call219;
  struct l_class_OC_BaseEvent *llvm_cbe_call221;
  unsigned int llvm_cbe_tmp225;
  unsigned int llvm_cbe_tmp228;
  unsigned int llvm_cbe_tmp__43;

#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_this_2e_addr) = llvm_cbe_this;
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_p_uecEvent_ptr_2e_addr) = llvm_cbe_p_uecEvent_ptr;
#line 0 "LLVM INTERNAL"
  llvm_cbe_this1 = *(&llvm_cbe_this_2e_addr);
#line 234 "UecDirectTransfer.cpp"
  _ZNSaIcEC1Ev((&llvm_cbe_agg_2e_tmp));
#line 234 "UecDirectTransfer.cpp"
  _ZNSsC1EPKcRKSaIcE((&llvm_cbe__currentFunction_), ((&__PRETTY_FUNCTION___OC__ZN3Uec17UecDirectTransfer24handleS1ApDlNasTransportEPNS_8UecEventE.array[((signed int )0u)])), (&llvm_cbe_agg_2e_tmp));
#line 234 "UecDirectTransfer.cpp"
  _ZNSaIcED1Ev((&llvm_cbe_agg_2e_tmp));
#line 234 "UecDirectTransfer.cpp"
  _ZN3Uec17UecDebugCallstackC1ERKSs((&llvm_cbe__callstackTmp_), (&llvm_cbe__currentFunction_));
#line 237 "UecDirectTransfer.cpp"
  llvm_cbe_tmp = *(&llvm_cbe_p_uecEvent_ptr_2e_addr);
#line 237 "UecDirectTransfer.cpp"
  llvm_cbe_call = _ZNK3Uec8UecEvent14getAsn1PayLoadEv(llvm_cbe_tmp);
#line 237 "UecDirectTransfer.cpp"
  *(&llvm_cbe_l_asn1PayLoad_ptr) = (((struct l_struct_OC_SS1apUFDownlinkNASTransport *)llvm_cbe_call));
#line 240 "UecDirectTransfer.cpp"
  llvm_cbe_call16 = _ZNK3Uec14UecServiceBase16getUecDataAccessEv((((struct l_class_OC_Uec_KD__KD_UecServiceBase *)llvm_cbe_this1)));
#line 240 "UecDirectTransfer.cpp"
  llvm_cbe_tmp__39 = *(((struct l_class_OC_Uec_KD__KD_UecBaseData * (***) (struct l_class_OC_Uec_KD__KD_UecBaseData *, unsigned int ))llvm_cbe_call16));
#line 240 "UecDirectTransfer.cpp"
  llvm_cbe_tmp__40 = *((&llvm_cbe_tmp__39[((signed long long )2ull)]));
#line 240 "UecDirectTransfer.cpp"
  llvm_cbe_call18 = llvm_cbe_tmp__40(llvm_cbe_call16, 1u);
#line 240 "UecDirectTransfer.cpp"
  *(&llvm_cbe_l_uecUeContextDataWrite_ptr) = (((struct l_class_OC_Uec_KD__KD_UecUeContextData *)llvm_cbe_call18));
#line 242 "UecDirectTransfer.cpp"
  llvm_cbe_tmp19 = *(&llvm_cbe_l_uecUeContextDataWrite_ptr);
#line 242 "UecDirectTransfer.cpp"
  llvm_cbe_call21 = ((_ZNK3Uec16UecUeContextData16isValidMmeUeS1IdEv(llvm_cbe_tmp19))&1);
#line 242 "UecDirectTransfer.cpp"
  if (llvm_cbe_call21) {    goto llvm_cbe_if_2e_end89;  } else {    goto llvm_cbe_if_2e_then;  }


llvm_cbe_if_2e_then:
#line 245 "UecDirectTransfer.cpp"
  llvm_cbe_tmp22 = *(&llvm_cbe_l_uecUeContextDataWrite_ptr);
#line 245 "UecDirectTransfer.cpp"
  llvm_cbe_tmp23 = *(&llvm_cbe_l_asn1PayLoad_ptr);
#line 245 "UecDirectTransfer.cpp"
  llvm_cbe_tmp25 = *((&llvm_cbe_tmp23->field0));
#line 245 "UecDirectTransfer.cpp"
  _ZN3Uec16UecUeContextData12setMmeUeS1IdEj(llvm_cbe_tmp22, (((unsigned int )llvm_cbe_tmp25)));
#line 247 "UecDirectTransfer.cpp"
  llvm_cbe_call28 = _ZN11DummyStreamlsIPKcEERS_T_((&mout), ((&_OC_str12.array[((signed int )0u)])));
#line 247 "UecDirectTransfer.cpp"
  llvm_cbe_call30 = _ZN11DummyStreamlsIPFRSt8ios_baseS2_EEERS_T_(llvm_cbe_call28, _ZSt3decRSt8ios_base);
#line 247 "UecDirectTransfer.cpp"
  llvm_cbe_tmp31 = *(&llvm_cbe_p_uecEvent_ptr_2e_addr);
#line 247 "UecDirectTransfer.cpp"
  llvm_cbe_call33 = _ZNK3Uec8UecEvent13getInstanceIdEv(llvm_cbe_tmp31);
#line 247 "UecDirectTransfer.cpp"
  llvm_cbe_call35 = _ZN11DummyStreamlsIjEERS_T_(llvm_cbe_call30, llvm_cbe_call33);
#line 247 "UecDirectTransfer.cpp"
  llvm_cbe_call37 = _ZN11DummyStreamlsIPKcEERS_T_(llvm_cbe_call35, ((&_OC_str13.array[((signed int )0u)])));
#line 247 "UecDirectTransfer.cpp"
  llvm_cbe_call39 = _ZN11DummyStreamlsIPFRSt8ios_baseS2_EEERS_T_(llvm_cbe_call37, _ZSt3decRSt8ios_base);
#line 247 "UecDirectTransfer.cpp"
  llvm_cbe_tmp40 = *(&llvm_cbe_l_asn1PayLoad_ptr);
#line 247 "UecDirectTransfer.cpp"
  llvm_cbe_tmp42 = *((&llvm_cbe_tmp40->field1));
#line 247 "UecDirectTransfer.cpp"
  llvm_cbe_call44 = _ZN11DummyStreamlsIiEERS_T_(llvm_cbe_call39, llvm_cbe_tmp42);
#line 247 "UecDirectTransfer.cpp"
  llvm_cbe_call46 = _ZN11DummyStreamlsIPKcEERS_T_(llvm_cbe_call44, ((&_OC_str14.array[((signed int )0u)])));
#line 247 "UecDirectTransfer.cpp"
  llvm_cbe_call48 = _ZN11DummyStreamlsIPFRSt8ios_baseS2_EEERS_T_(llvm_cbe_call46, _ZSt3decRSt8ios_base);
#line 247 "UecDirectTransfer.cpp"
  llvm_cbe_tmp49 = *(&llvm_cbe_l_asn1PayLoad_ptr);
#line 247 "UecDirectTransfer.cpp"
  llvm_cbe_tmp51 = *((&llvm_cbe_tmp49->field0));
#line 247 "UecDirectTransfer.cpp"
  llvm_cbe_call53 = _ZN11DummyStreamlsIxEERS_T_(llvm_cbe_call48, llvm_cbe_tmp51);
#line 252 "UecDirectTransfer.cpp"
  llvm_cbe_tmp54 = *(&llvm_cbe_l_uecUeContextDataWrite_ptr);
#line 252 "UecDirectTransfer.cpp"
  llvm_cbe_call56 = _ZNK3Uec16UecUeContextData24getUeAssS1SigConnTimerIdEv(llvm_cbe_tmp54);
#line 252 "UecDirectTransfer.cpp"
  if ((4294967295u == llvm_cbe_call56)) {    goto llvm_cbe_if_2e_end89;  } else {    goto llvm_cbe_if_2e_then57;  }


llvm_cbe_if_2e_then57:
#line 254 "UecDirectTransfer.cpp"
  llvm_cbe_call59 = _ZNK3Uec14UecServiceBase23getUecServiceControllerEv((((struct l_class_OC_Uec_KD__KD_UecServiceBase *)llvm_cbe_this1)));
#line 254 "UecDirectTransfer.cpp"
  llvm_cbe_tmp__41 = *(((struct l_class_OC_Uec_KD__KD_UecTimePort * (***) (struct l_class_OC_Uec_KD__KD_UecBaseData *))llvm_cbe_call59));
#line 254 "UecDirectTransfer.cpp"
  llvm_cbe_tmp__42 = *((&llvm_cbe_tmp__41[((signed long long )2ull)]));
#line 254 "UecDirectTransfer.cpp"
  llvm_cbe_call62 = llvm_cbe_tmp__42(llvm_cbe_call59);
#line 254 "UecDirectTransfer.cpp"
  llvm_cbe_tmp63 = *(&llvm_cbe_l_uecUeContextDataWrite_ptr);
#line 254 "UecDirectTransfer.cpp"
  llvm_cbe_call65 = _ZNK3Uec16UecUeContextData24getUeAssS1SigConnTimerIdEv(llvm_cbe_tmp63);
#line 254 "UecDirectTransfer.cpp"
  llvm_cbe_call67 = _ZN3Uec11UecTimePort9stopTimerEj(llvm_cbe_call62, llvm_cbe_call65);
#line 256 "UecDirectTransfer.cpp"
  llvm_cbe_call69 = _ZN11DummyStreamlsIPKcEERS_T_((&mout), ((&_OC_str12.array[((signed int )0u)])));
#line 256 "UecDirectTransfer.cpp"
  llvm_cbe_call71 = _ZN11DummyStreamlsIPFRSt8ios_baseS2_EEERS_T_(llvm_cbe_call69, _ZSt3decRSt8ios_base);
#line 256 "UecDirectTransfer.cpp"
  llvm_cbe_call73 = _ZNK3Uec14UecServiceBase13getUecManagerEv((((struct l_class_OC_Uec_KD__KD_UecServiceBase *)llvm_cbe_this1)));
#line 256 "UecDirectTransfer.cpp"
  llvm_cbe_call75 = _ZNK3Uec14UecManagerBase13getInstanceIdEv(llvm_cbe_call73);
#line 256 "UecDirectTransfer.cpp"
  llvm_cbe_call77 = _ZN11DummyStreamlsIjEERS_T_(llvm_cbe_call71, llvm_cbe_call75);
#line 256 "UecDirectTransfer.cpp"
  llvm_cbe_call79 = _ZN11DummyStreamlsIPKcEERS_T_(llvm_cbe_call77, ((&_OC_str15.array[((signed int )0u)])));
#line 256 "UecDirectTransfer.cpp"
  llvm_cbe_call81 = _ZN11DummyStreamlsIPFRSt8ios_baseS2_EEERS_T_(llvm_cbe_call79, _ZSt3decRSt8ios_base);
#line 256 "UecDirectTransfer.cpp"
  llvm_cbe_tmp82 = *(&llvm_cbe_l_uecUeContextDataWrite_ptr);
#line 256 "UecDirectTransfer.cpp"
  llvm_cbe_call84 = _ZNK3Uec16UecUeContextData24getUeAssS1SigConnTimerIdEv(llvm_cbe_tmp82);
#line 256 "UecDirectTransfer.cpp"
  llvm_cbe_call86 = _ZN11DummyStreamlsIjEERS_T_(llvm_cbe_call81, llvm_cbe_call84);
#line 259 "UecDirectTransfer.cpp"
  llvm_cbe_tmp87 = *(&llvm_cbe_l_uecUeContextDataWrite_ptr);
#line 259 "UecDirectTransfer.cpp"
  _ZN3Uec16UecUeContextData24setUeAssS1SigConnTimerIdEj(llvm_cbe_tmp87, 4294967295u);
#line 0 "LLVM INTERNAL"
  goto llvm_cbe_if_2e_end89;

llvm_cbe_if_2e_end89:
#line 263 "UecDirectTransfer.cpp"
  llvm_cbe_tmp90 = *(&llvm_cbe_l_asn1PayLoad_ptr);
#line 263 "UecDirectTransfer.cpp"
  llvm_cbe_tmp92 = *((&llvm_cbe_tmp90->field1));
#line 263 "UecDirectTransfer.cpp"
  llvm_cbe_tmp93 = *(&llvm_cbe_l_uecUeContextDataWrite_ptr);
#line 263 "UecDirectTransfer.cpp"
  llvm_cbe_call95 = _ZNK3Uec16UecUeContextData12getEnbUeS1IdEv(llvm_cbe_tmp93);
#line 263 "UecDirectTransfer.cpp"
  if ((llvm_cbe_tmp92 != llvm_cbe_call95)) {    goto llvm_cbe_if_2e_then105;  } else {    goto llvm_cbe_lor_2e_lhs_2e_false;  }


llvm_cbe_lor_2e_lhs_2e_false:
#line 263 "UecDirectTransfer.cpp"
  llvm_cbe_tmp97 = *(&llvm_cbe_l_asn1PayLoad_ptr);
#line 263 "UecDirectTransfer.cpp"
  llvm_cbe_tmp99 = *((&llvm_cbe_tmp97->field0));
#line 263 "UecDirectTransfer.cpp"
  llvm_cbe_tmp100 = *(&llvm_cbe_l_uecUeContextDataWrite_ptr);
#line 263 "UecDirectTransfer.cpp"
  llvm_cbe_call102 = _ZNK3Uec16UecUeContextData12getMmeUeS1IdEv(llvm_cbe_tmp100);
#line 263 "UecDirectTransfer.cpp"
  if ((llvm_cbe_tmp99 != (((unsigned long long )(unsigned int )llvm_cbe_call102)))) {    goto llvm_cbe_if_2e_then105;  } else {    goto llvm_cbe_if_2e_end153;  }


llvm_cbe_if_2e_then105:
#line 268 "UecDirectTransfer.cpp"
  llvm_cbe_call107 = _ZN11DummyStreamlsIPKcEERS_T_((&mout), ((&_OC_str12.array[((signed int )0u)])));
#line 268 "UecDirectTransfer.cpp"
  llvm_cbe_call109 = _ZN11DummyStreamlsIPFRSt8ios_baseS2_EEERS_T_(llvm_cbe_call107, _ZSt3decRSt8ios_base);
#line 268 "UecDirectTransfer.cpp"
  llvm_cbe_tmp110 = *(&llvm_cbe_p_uecEvent_ptr_2e_addr);
#line 268 "UecDirectTransfer.cpp"
  llvm_cbe_call112 = _ZNK3Uec8UecEvent13getInstanceIdEv(llvm_cbe_tmp110);
#line 268 "UecDirectTransfer.cpp"
  llvm_cbe_call114 = _ZN11DummyStreamlsIjEERS_T_(llvm_cbe_call109, llvm_cbe_call112);
#line 268 "UecDirectTransfer.cpp"
  llvm_cbe_call116 = _ZN11DummyStreamlsIPKcEERS_T_(llvm_cbe_call114, ((&_OC_str16.array[((signed int )0u)])));
#line 268 "UecDirectTransfer.cpp"
  llvm_cbe_call118 = _ZN11DummyStreamlsIPFRSt8ios_baseS2_EEERS_T_(llvm_cbe_call116, _ZSt3decRSt8ios_base);
#line 268 "UecDirectTransfer.cpp"
  llvm_cbe_tmp119 = *(&llvm_cbe_l_asn1PayLoad_ptr);
#line 268 "UecDirectTransfer.cpp"
  llvm_cbe_tmp121 = *((&llvm_cbe_tmp119->field1));
#line 268 "UecDirectTransfer.cpp"
  llvm_cbe_call123 = _ZN11DummyStreamlsIiEERS_T_(llvm_cbe_call118, llvm_cbe_tmp121);
#line 268 "UecDirectTransfer.cpp"
  llvm_cbe_call125 = _ZN11DummyStreamlsIPKcEERS_T_(llvm_cbe_call123, ((&_OC_str17.array[((signed int )0u)])));
#line 268 "UecDirectTransfer.cpp"
  llvm_cbe_call127 = _ZN11DummyStreamlsIPFRSt8ios_baseS2_EEERS_T_(llvm_cbe_call125, _ZSt3decRSt8ios_base);
#line 268 "UecDirectTransfer.cpp"
  llvm_cbe_tmp128 = *(&llvm_cbe_l_uecUeContextDataWrite_ptr);
#line 268 "UecDirectTransfer.cpp"
  llvm_cbe_call130 = _ZNK3Uec16UecUeContextData12getEnbUeS1IdEv(llvm_cbe_tmp128);
#line 268 "UecDirectTransfer.cpp"
  llvm_cbe_call132 = _ZN11DummyStreamlsIjEERS_T_(llvm_cbe_call127, llvm_cbe_call130);
#line 268 "UecDirectTransfer.cpp"
  llvm_cbe_call134 = _ZN11DummyStreamlsIPKcEERS_T_(llvm_cbe_call132, ((&_OC_str18.array[((signed int )0u)])));
#line 268 "UecDirectTransfer.cpp"
  llvm_cbe_call136 = _ZN11DummyStreamlsIPFRSt8ios_baseS2_EEERS_T_(llvm_cbe_call134, _ZSt3decRSt8ios_base);
#line 268 "UecDirectTransfer.cpp"
  llvm_cbe_tmp137 = *(&llvm_cbe_l_asn1PayLoad_ptr);
#line 268 "UecDirectTransfer.cpp"
  llvm_cbe_tmp139 = *((&llvm_cbe_tmp137->field0));
#line 268 "UecDirectTransfer.cpp"
  llvm_cbe_call141 = _ZN11DummyStreamlsIxEERS_T_(llvm_cbe_call136, llvm_cbe_tmp139);
#line 268 "UecDirectTransfer.cpp"
  llvm_cbe_call143 = _ZN11DummyStreamlsIPKcEERS_T_(llvm_cbe_call141, ((&_OC_str19.array[((signed int )0u)])));
#line 268 "UecDirectTransfer.cpp"
  llvm_cbe_call145 = _ZN11DummyStreamlsIPFRSt8ios_baseS2_EEERS_T_(llvm_cbe_call143, _ZSt3decRSt8ios_base);
#line 268 "UecDirectTransfer.cpp"
  llvm_cbe_tmp146 = *(&llvm_cbe_l_uecUeContextDataWrite_ptr);
#line 268 "UecDirectTransfer.cpp"
  llvm_cbe_call148 = _ZNK3Uec16UecUeContextData12getMmeUeS1IdEv(llvm_cbe_tmp146);
#line 268 "UecDirectTransfer.cpp"
  llvm_cbe_call150 = _ZN11DummyStreamlsIjEERS_T_(llvm_cbe_call145, llvm_cbe_call148);
#line 268 "UecDirectTransfer.cpp"
  llvm_cbe_call152 = _ZN11DummyStreamlsIPKcEERS_T_(llvm_cbe_call150, ((&_OC_str20.array[((signed int )0u)])));
#line 275 "UecDirectTransfer.cpp"
  *(&llvm_cbe_retval) = 4294967295u;
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_cleanup_2e_dst) = 1u;
#line 275 "UecDirectTransfer.cpp"
  goto llvm_cbe_cleanup;

llvm_cbe_if_2e_end153:
#line 278 "UecDirectTransfer.cpp"
  llvm_cbe_tmp154 = *(&llvm_cbe_l_asn1PayLoad_ptr);
#line 278 "UecDirectTransfer.cpp"
  llvm_cbe_tmp157 = *((&((&llvm_cbe_tmp154->field2))->field1));
#line 278 "UecDirectTransfer.cpp"
  if ((((unsigned char *)/*NULL*/0) == llvm_cbe_tmp157)) {    goto llvm_cbe_if_2e_then165;  } else {    goto llvm_cbe_lor_2e_lhs_2e_false159;  }


llvm_cbe_lor_2e_lhs_2e_false159:
#line 278 "UecDirectTransfer.cpp"
  llvm_cbe_tmp160 = *(&llvm_cbe_l_asn1PayLoad_ptr);
#line 278 "UecDirectTransfer.cpp"
  llvm_cbe_tmp163 = *((&((&llvm_cbe_tmp160->field2))->field0));
#line 278 "UecDirectTransfer.cpp"
  if ((0u == llvm_cbe_tmp163)) {    goto llvm_cbe_if_2e_then165;  } else {    goto llvm_cbe_if_2e_end204;  }


llvm_cbe_if_2e_then165:
#line 282 "UecDirectTransfer.cpp"
  llvm_cbe_call167 = _ZN11DummyStreamlsIPKcEERS_T_((&mout), ((&_OC_str12.array[((signed int )0u)])));
#line 282 "UecDirectTransfer.cpp"
  llvm_cbe_call169 = _ZN11DummyStreamlsIPFRSt8ios_baseS2_EEERS_T_(llvm_cbe_call167, _ZSt3decRSt8ios_base);
#line 282 "UecDirectTransfer.cpp"
  llvm_cbe_tmp170 = *(&llvm_cbe_p_uecEvent_ptr_2e_addr);
#line 282 "UecDirectTransfer.cpp"
  llvm_cbe_call172 = _ZNK3Uec8UecEvent13getInstanceIdEv(llvm_cbe_tmp170);
#line 282 "UecDirectTransfer.cpp"
  llvm_cbe_call174 = _ZN11DummyStreamlsIjEERS_T_(llvm_cbe_call169, llvm_cbe_call172);
#line 282 "UecDirectTransfer.cpp"
  llvm_cbe_call176 = _ZN11DummyStreamlsIPKcEERS_T_(llvm_cbe_call174, ((&_OC_str6.array[((signed int )0u)])));
#line 282 "UecDirectTransfer.cpp"
  llvm_cbe_call178 = _ZN11DummyStreamlsIPFRSt8ios_baseS2_EEERS_T_(llvm_cbe_call176, _ZSt3hexRSt8ios_base);
#line 282 "UecDirectTransfer.cpp"
  llvm_cbe_tmp179 = *(&llvm_cbe_p_uecEvent_ptr_2e_addr);
#line 282 "UecDirectTransfer.cpp"
  llvm_cbe_call181 = _ZNK6CEvent10GetEventIdEv((((struct l_class_OC_CEvent *)llvm_cbe_tmp179)));
#line 282 "UecDirectTransfer.cpp"
  llvm_cbe_call183 = _ZN11DummyStreamlsIjEERS_T_(llvm_cbe_call178, llvm_cbe_call181);
#line 282 "UecDirectTransfer.cpp"
  llvm_cbe_call185 = _ZN11DummyStreamlsIPKcEERS_T_(llvm_cbe_call183, ((&_OC_str7.array[((signed int )0u)])));
#line 282 "UecDirectTransfer.cpp"
  llvm_cbe_call187 = _ZN11DummyStreamlsIPFRSt8ios_baseS2_EEERS_T_(llvm_cbe_call185, _ZSt3decRSt8ios_base);
#line 282 "UecDirectTransfer.cpp"
  llvm_cbe_tmp188 = *(&llvm_cbe_l_asn1PayLoad_ptr);
#line 282 "UecDirectTransfer.cpp"
  llvm_cbe_tmp191 = *((&((&llvm_cbe_tmp188->field2))->field0));
#line 282 "UecDirectTransfer.cpp"
  llvm_cbe_call193 = _ZN11DummyStreamlsIjEERS_T_(llvm_cbe_call187, llvm_cbe_tmp191);
#line 282 "UecDirectTransfer.cpp"
  llvm_cbe_call195 = _ZN11DummyStreamlsIPKcEERS_T_(llvm_cbe_call193, ((&_OC_str8.array[((signed int )0u)])));
#line 282 "UecDirectTransfer.cpp"
  llvm_cbe_call197 = _ZN11DummyStreamlsIPFRSt8ios_baseS2_EEERS_T_(llvm_cbe_call195, _ZSt3hexRSt8ios_base);
#line 282 "UecDirectTransfer.cpp"
  llvm_cbe_tmp198 = *(&llvm_cbe_l_asn1PayLoad_ptr);
#line 282 "UecDirectTransfer.cpp"
  llvm_cbe_tmp201 = *((&((&llvm_cbe_tmp198->field2))->field1));
#line 282 "UecDirectTransfer.cpp"
  llvm_cbe_call203 = _ZN11DummyStreamlsIPhEERS_T_(llvm_cbe_call197, llvm_cbe_tmp201);
#line 287 "UecDirectTransfer.cpp"
  *(&llvm_cbe_retval) = 4294967295u;
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_cleanup_2e_dst) = 2u;
#line 287 "UecDirectTransfer.cpp"
  goto llvm_cbe_cleanup;

llvm_cbe_if_2e_end204:
#line 294 "UecDirectTransfer.cpp"
  llvm_cbe_tmp205 = *(&llvm_cbe_l_asn1PayLoad_ptr);
#line 294 "UecDirectTransfer.cpp"
  llvm_cbe_tmp206 = *(&llvm_cbe_l_uecUeContextDataWrite_ptr);
#line 294 "UecDirectTransfer.cpp"
  llvm_cbe_call208 = _ZN3Uec17UecDirectTransfer31handleEmbeddedHoRestrictionListEPK27SS1apUFDownlinkNASTransportPNS_16UecUeContextDataE(llvm_cbe_this1, llvm_cbe_tmp205, llvm_cbe_tmp206);
#line 294 "UecDirectTransfer.cpp"
  if ((llvm_cbe_call208 != 1u)) {    goto llvm_cbe_if_2e_then210;  } else {    goto llvm_cbe_if_2e_end222;  }


llvm_cbe_if_2e_then210:
#line 296 "UecDirectTransfer.cpp"
  llvm_cbe_call212 = _ZN11DummyStreamlsIPKcEERS_T_((&mout), ((&_OC_str12.array[((signed int )0u)])));
#line 296 "UecDirectTransfer.cpp"
  llvm_cbe_call214 = _ZN11DummyStreamlsIPFRSt8ios_baseS2_EEERS_T_(llvm_cbe_call212, _ZSt3decRSt8ios_base);
#line 296 "UecDirectTransfer.cpp"
  llvm_cbe_tmp215 = *(&llvm_cbe_p_uecEvent_ptr_2e_addr);
#line 296 "UecDirectTransfer.cpp"
  llvm_cbe_call217 = _ZNK3Uec8UecEvent13getInstanceIdEv(llvm_cbe_tmp215);
#line 296 "UecDirectTransfer.cpp"
  llvm_cbe_call219 = _ZN11DummyStreamlsIjEERS_T_(llvm_cbe_call214, llvm_cbe_call217);
#line 296 "UecDirectTransfer.cpp"
  llvm_cbe_call221 = _ZN11DummyStreamlsIPKcEERS_T_(llvm_cbe_call219, ((&_OC_str21.array[((signed int )0u)])));
#line 0 "LLVM INTERNAL"
  goto llvm_cbe_if_2e_end222;

llvm_cbe_if_2e_end222:
#line 302 "UecDirectTransfer.cpp"
  *(&llvm_cbe_retval) = 1u;
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_cleanup_2e_dst) = 3u;
#line 302 "UecDirectTransfer.cpp"
  goto llvm_cbe_cleanup;

llvm_cbe_cleanup_2e_pad:
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_cleanup_2e_dst226) = 1u;
#line 0 "LLVM INTERNAL"
  goto llvm_cbe_cleanup227;

llvm_cbe_cleanup_2e_pad223:
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_cleanup_2e_dst226) = 1u;
#line 0 "LLVM INTERNAL"
  goto llvm_cbe_cleanup227;

llvm_cbe_cleanup_2e_pad224:
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_cleanup_2e_dst226) = 1u;
#line 0 "LLVM INTERNAL"
  goto llvm_cbe_cleanup227;

llvm_cbe_cleanup:
#line 303 "UecDirectTransfer.cpp"
  _ZN3Uec17UecDebugCallstackD1Ev((&llvm_cbe__callstackTmp_));
#line 303 "UecDirectTransfer.cpp"
  llvm_cbe_tmp225 = *(&llvm_cbe_cleanup_2e_dst);
#line 303 "UecDirectTransfer.cpp"
  switch (llvm_cbe_tmp225) {
  default:
    goto llvm_cbe_cleanup_2e_end;;
  case 1u:
    goto llvm_cbe_cleanup_2e_pad;  case 2u:
    goto llvm_cbe_cleanup_2e_pad223;  case 3u:
    goto llvm_cbe_cleanup_2e_pad224;  }

llvm_cbe_cleanup_2e_end:
#line 303 "UecDirectTransfer.cpp"
  *(&llvm_cbe_cleanup_2e_dst226) = 0u;
#line 303 "UecDirectTransfer.cpp"
  goto llvm_cbe_cleanup227;

llvm_cbe_cleanup227:
#line 303 "UecDirectTransfer.cpp"
  _ZNSsD1Ev((&llvm_cbe__currentFunction_));
#line 303 "UecDirectTransfer.cpp"
  llvm_cbe_tmp228 = *(&llvm_cbe_cleanup_2e_dst226);
#line 303 "UecDirectTransfer.cpp"
  llvm_cbe_tmp__43 = *(&llvm_cbe_retval);
#line 303 "UecDirectTransfer.cpp"
  return llvm_cbe_tmp__43;
}


#line 0 "LLVM INTERNAL"
struct l_class_OC_Uec_KD__KD_UecBaseData *_ZNK3Uec14UecServiceBase16getUecDataAccessEv(struct l_class_OC_Uec_KD__KD_UecServiceBase *llvm_cbe_this) {
  struct l_class_OC_Uec_KD__KD_UecBaseData *llvm_cbe_retval;    /* Address-exposed local */
  struct l_class_OC_Uec_KD__KD_UecServiceBase *llvm_cbe_this_2e_addr;    /* Address-exposed local */
  struct l_class_OC_Uec_KD__KD_UecServiceBase *llvm_cbe_this1;
  struct l_class_OC_Uec_KD__KD_UecBaseData *llvm_cbe_tmp2;
  struct l_class_OC_Uec_KD__KD_UecBaseData *llvm_cbe_tmp__44;

#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_this_2e_addr) = llvm_cbe_this;
#line 0 "LLVM INTERNAL"
  llvm_cbe_this1 = *(&llvm_cbe_this_2e_addr);
#line 96 "UecServiceBase.hpp"
  llvm_cbe_tmp2 = *((&llvm_cbe_this1->field3));
#line 96 "UecServiceBase.hpp"
  *(&llvm_cbe_retval) = llvm_cbe_tmp2;
#line 96 "UecServiceBase.hpp"
  llvm_cbe_tmp__44 = *(&llvm_cbe_retval);
#line 96 "UecServiceBase.hpp"
  return llvm_cbe_tmp__44;
}


#line 0 "LLVM INTERNAL"
bool _ZNK3Uec16UecUeContextData16isValidMmeUeS1IdEv(struct l_class_OC_Uec_KD__KD_UecUeContextData *llvm_cbe_this) {
  bool llvm_cbe_retval;    /* Address-exposed local */
  struct l_class_OC_Uec_KD__KD_UecUeContextData *llvm_cbe_this_2e_addr;    /* Address-exposed local */
  struct l_class_OC_Uec_KD__KD_UecUeContextData *llvm_cbe_this1;
  unsigned char llvm_cbe_tmp3;
  bool llvm_cbe_tmp__45;

#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_this_2e_addr) = llvm_cbe_this;
#line 0 "LLVM INTERNAL"
  llvm_cbe_this1 = *(&llvm_cbe_this_2e_addr);
#line 1155 "UecUeContextData.hpp"
  llvm_cbe_tmp3 = *((&((&llvm_cbe_this1->field1))->field17));
#line 1155 "UecUeContextData.hpp"
  *(&llvm_cbe_retval) = (((((((bool )llvm_cbe_tmp3&1u))&1))) & 1);
#line 1156 "UecUeContextData.hpp"
  llvm_cbe_tmp__45 = ((*(&llvm_cbe_retval))&1);
#line 1156 "UecUeContextData.hpp"
  return llvm_cbe_tmp__45;
}


#line 0 "LLVM INTERNAL"
void _ZN3Uec16UecUeContextData12setMmeUeS1IdEj(struct l_class_OC_Uec_KD__KD_UecUeContextData *llvm_cbe_this, unsigned int llvm_cbe_p_mmeUeS1Id) {
  struct l_class_OC_Uec_KD__KD_UecUeContextData *llvm_cbe_this_2e_addr;    /* Address-exposed local */
  unsigned int llvm_cbe_p_mmeUeS1Id_2e_addr;    /* Address-exposed local */
  struct l_class_OC_Uec_KD__KD_UecUeContextData *llvm_cbe_this1;
  unsigned int llvm_cbe_tmp;

#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_this_2e_addr) = llvm_cbe_this;
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_p_mmeUeS1Id_2e_addr) = llvm_cbe_p_mmeUeS1Id;
#line 0 "LLVM INTERNAL"
  llvm_cbe_this1 = *(&llvm_cbe_this_2e_addr);
#line 1147 "UecUeContextData.hpp"
  llvm_cbe_tmp = *(&llvm_cbe_p_mmeUeS1Id_2e_addr);
#line 1147 "UecUeContextData.hpp"
  *((&((&llvm_cbe_this1->field1))->field18)) = llvm_cbe_tmp;
#line 1148 "UecUeContextData.hpp"
  *((&((&llvm_cbe_this1->field1))->field17)) = ((unsigned char )1);
#line 1150 "UecUeContextData.hpp"
  _ZN3Uec16UecUeContextData10connectMmeEv(llvm_cbe_this1);
#line 1151 "UecUeContextData.hpp"
  return;
}


#line 0 "LLVM INTERNAL"
struct l_class_OC_BaseEvent *_ZN11DummyStreamlsIiEERS_T_(struct l_class_OC_BaseEvent *llvm_cbe_this, unsigned int llvm_cbe_arg) {
  struct l_class_OC_BaseEvent *llvm_cbe_retval;    /* Address-exposed local */
  struct l_class_OC_BaseEvent *llvm_cbe_this_2e_addr;    /* Address-exposed local */
  unsigned int llvm_cbe_arg_2e_addr;    /* Address-exposed local */
  struct l_class_OC_BaseEvent *llvm_cbe_this1;
  struct l_class_OC_BaseEvent *llvm_cbe_tmp__46;

#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_this_2e_addr) = llvm_cbe_this;
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_arg_2e_addr) = llvm_cbe_arg;
#line 0 "LLVM INTERNAL"
  llvm_cbe_this1 = *(&llvm_cbe_this_2e_addr);
#line 30 "BaseTypes.hpp"
  *(&llvm_cbe_retval) = llvm_cbe_this1;
#line 31 "BaseTypes.hpp"
  llvm_cbe_tmp__46 = *(&llvm_cbe_retval);
#line 31 "BaseTypes.hpp"
  return llvm_cbe_tmp__46;
}


#line 0 "LLVM INTERNAL"
struct l_class_OC_BaseEvent *_ZN11DummyStreamlsIxEERS_T_(struct l_class_OC_BaseEvent *llvm_cbe_this, unsigned long long llvm_cbe_arg) {
  struct l_class_OC_BaseEvent *llvm_cbe_retval;    /* Address-exposed local */
  struct l_class_OC_BaseEvent *llvm_cbe_this_2e_addr;    /* Address-exposed local */
  unsigned long long llvm_cbe_arg_2e_addr;    /* Address-exposed local */
  struct l_class_OC_BaseEvent *llvm_cbe_this1;
  struct l_class_OC_BaseEvent *llvm_cbe_tmp__47;

#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_this_2e_addr) = llvm_cbe_this;
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_arg_2e_addr) = llvm_cbe_arg;
#line 0 "LLVM INTERNAL"
  llvm_cbe_this1 = *(&llvm_cbe_this_2e_addr);
#line 30 "BaseTypes.hpp"
  *(&llvm_cbe_retval) = llvm_cbe_this1;
#line 31 "BaseTypes.hpp"
  llvm_cbe_tmp__47 = *(&llvm_cbe_retval);
#line 31 "BaseTypes.hpp"
  return llvm_cbe_tmp__47;
}


#line 0 "LLVM INTERNAL"
unsigned int _ZNK3Uec16UecUeContextData24getUeAssS1SigConnTimerIdEv(struct l_class_OC_Uec_KD__KD_UecUeContextData *llvm_cbe_this) {
  unsigned int llvm_cbe_retval;    /* Address-exposed local */
  struct l_class_OC_Uec_KD__KD_UecUeContextData *llvm_cbe_this_2e_addr;    /* Address-exposed local */
  struct l_class_OC_Uec_KD__KD_UecUeContextData *llvm_cbe_this1;
  unsigned int llvm_cbe_tmp3;
  unsigned int llvm_cbe_tmp__48;

#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_this_2e_addr) = llvm_cbe_this;
#line 0 "LLVM INTERNAL"
  llvm_cbe_this1 = *(&llvm_cbe_this_2e_addr);
#line 2321 "UecUeContextData.hpp"
  llvm_cbe_tmp3 = *((&((&llvm_cbe_this1->field1))->field26));
#line 2321 "UecUeContextData.hpp"
  *(&llvm_cbe_retval) = llvm_cbe_tmp3;
#line 2322 "UecUeContextData.hpp"
  llvm_cbe_tmp__48 = *(&llvm_cbe_retval);
#line 2322 "UecUeContextData.hpp"
  return llvm_cbe_tmp__48;
}


#line 0 "LLVM INTERNAL"
struct l_class_OC_Uec_KD__KD_UecBaseData *_ZNK3Uec14UecServiceBase23getUecServiceControllerEv(struct l_class_OC_Uec_KD__KD_UecServiceBase *llvm_cbe_this) {
  struct l_class_OC_Uec_KD__KD_UecBaseData *llvm_cbe_retval;    /* Address-exposed local */
  struct l_class_OC_Uec_KD__KD_UecServiceBase *llvm_cbe_this_2e_addr;    /* Address-exposed local */
  struct l_class_OC_Uec_KD__KD_UecServiceBase *llvm_cbe_this1;
  struct l_class_OC_Uec_KD__KD_UecBaseData *llvm_cbe_tmp2;
  struct l_class_OC_Uec_KD__KD_UecBaseData *llvm_cbe_tmp__49;

#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_this_2e_addr) = llvm_cbe_this;
#line 0 "LLVM INTERNAL"
  llvm_cbe_this1 = *(&llvm_cbe_this_2e_addr);
#line 95 "UecServiceBase.hpp"
  llvm_cbe_tmp2 = *((&llvm_cbe_this1->field2));
#line 95 "UecServiceBase.hpp"
  *(&llvm_cbe_retval) = llvm_cbe_tmp2;
#line 95 "UecServiceBase.hpp"
  llvm_cbe_tmp__49 = *(&llvm_cbe_retval);
#line 95 "UecServiceBase.hpp"
  return llvm_cbe_tmp__49;
}


#line 0 "LLVM INTERNAL"
void _ZN3Uec16UecUeContextData24setUeAssS1SigConnTimerIdEj(struct l_class_OC_Uec_KD__KD_UecUeContextData *llvm_cbe_this, unsigned int llvm_cbe_p_timerId) {
  struct l_class_OC_Uec_KD__KD_UecUeContextData *llvm_cbe_this_2e_addr;    /* Address-exposed local */
  unsigned int llvm_cbe_p_timerId_2e_addr;    /* Address-exposed local */
  struct l_class_OC_Uec_KD__KD_UecUeContextData *llvm_cbe_this1;
  unsigned int llvm_cbe_tmp;

#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_this_2e_addr) = llvm_cbe_this;
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_p_timerId_2e_addr) = llvm_cbe_p_timerId;
#line 0 "LLVM INTERNAL"
  llvm_cbe_this1 = *(&llvm_cbe_this_2e_addr);
#line 2316 "UecUeContextData.hpp"
  llvm_cbe_tmp = *(&llvm_cbe_p_timerId_2e_addr);
#line 2316 "UecUeContextData.hpp"
  *((&((&llvm_cbe_this1->field1))->field26)) = llvm_cbe_tmp;
#line 2317 "UecUeContextData.hpp"
  return;
}


#line 0 "LLVM INTERNAL"
unsigned int _ZNK3Uec16UecUeContextData12getEnbUeS1IdEv(struct l_class_OC_Uec_KD__KD_UecUeContextData *llvm_cbe_this) {
  unsigned int llvm_cbe_retval;    /* Address-exposed local */
  struct l_class_OC_Uec_KD__KD_UecUeContextData *llvm_cbe_this_2e_addr;    /* Address-exposed local */
  struct l_class_OC_Uec_KD__KD_UecUeContextData *llvm_cbe_this1;
  unsigned int llvm_cbe_tmp3;
  unsigned int llvm_cbe_tmp__50;

#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_this_2e_addr) = llvm_cbe_this;
#line 0 "LLVM INTERNAL"
  llvm_cbe_this1 = *(&llvm_cbe_this_2e_addr);
#line 1044 "UecUeContextData.hpp"
  llvm_cbe_tmp3 = *((&((&llvm_cbe_this1->field1))->field11));
#line 1044 "UecUeContextData.hpp"
  *(&llvm_cbe_retval) = llvm_cbe_tmp3;
#line 1045 "UecUeContextData.hpp"
  llvm_cbe_tmp__50 = *(&llvm_cbe_retval);
#line 1045 "UecUeContextData.hpp"
  return llvm_cbe_tmp__50;
}


#line 0 "LLVM INTERNAL"
unsigned int _ZNK3Uec16UecUeContextData12getMmeUeS1IdEv(struct l_class_OC_Uec_KD__KD_UecUeContextData *llvm_cbe_this) {
  unsigned int llvm_cbe_retval;    /* Address-exposed local */
  struct l_class_OC_Uec_KD__KD_UecUeContextData *llvm_cbe_this_2e_addr;    /* Address-exposed local */
  struct l_class_OC_Uec_KD__KD_UecUeContextData *llvm_cbe_this1;
  unsigned int llvm_cbe_tmp3;
  unsigned int llvm_cbe_tmp__51;

#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_this_2e_addr) = llvm_cbe_this;
#line 0 "LLVM INTERNAL"
  llvm_cbe_this1 = *(&llvm_cbe_this_2e_addr);
#line 1142 "UecUeContextData.hpp"
  llvm_cbe_tmp3 = *((&((&llvm_cbe_this1->field1))->field18));
#line 1142 "UecUeContextData.hpp"
  *(&llvm_cbe_retval) = llvm_cbe_tmp3;
#line 1143 "UecUeContextData.hpp"
  llvm_cbe_tmp__51 = *(&llvm_cbe_retval);
#line 1143 "UecUeContextData.hpp"
  return llvm_cbe_tmp__51;
}


#line 0 "LLVM INTERNAL"
unsigned int _ZN3Uec17UecDirectTransfer31handleEmbeddedHoRestrictionListEPK27SS1apUFDownlinkNASTransportPNS_16UecUeContextDataE(struct l_class_OC_Uec_KD__KD_UecDirectTransfer *llvm_cbe_this, struct l_struct_OC_SS1apUFDownlinkNASTransport *llvm_cbe_p_asn1PayloadPtr, struct l_class_OC_Uec_KD__KD_UecUeContextData *llvm_cbe_p_uecUeContextDataPtr) {
  unsigned int llvm_cbe_retval;    /* Address-exposed local */
  struct l_class_OC_Uec_KD__KD_UecDirectTransfer *llvm_cbe_this_2e_addr;    /* Address-exposed local */
  struct l_struct_OC_SS1apUFDownlinkNASTransport *llvm_cbe_p_asn1PayloadPtr_2e_addr;    /* Address-exposed local */
  struct l_class_OC_Uec_KD__KD_UecUeContextData *llvm_cbe_p_uecUeContextDataPtr_2e_addr;    /* Address-exposed local */
  struct l_class_OC_std_KD__KD_basic_string llvm_cbe__currentFunction_;    /* Address-exposed local */
  struct l_class_OC_BaseEvent llvm_cbe_agg_2e_tmp;    /* Address-exposed local */
  struct l_class_OC_Uec_KD__KD_UecDebugCallstack llvm_cbe__callstackTmp_;    /* Address-exposed local */
  unsigned int llvm_cbe_cleanup_2e_dst;    /* Address-exposed local */
  unsigned int llvm_cbe_cleanup_2e_dst215;    /* Address-exposed local */
  struct l_class_OC_Uec_KD__KD_UecDirectTransfer *llvm_cbe_this1;
  struct l_struct_OC_SS1apUFDownlinkNASTransport *llvm_cbe_tmp;
  unsigned int llvm_cbe_tmp14;
  struct l_class_OC_BaseEvent *llvm_cbe_call;
  struct l_class_OC_BaseEvent *llvm_cbe_call17;
  struct l_class_OC_Uec_KD__KD_UecManagerBase *llvm_cbe_call19;
  unsigned int llvm_cbe_call21;
  struct l_class_OC_BaseEvent *llvm_cbe_call23;
  struct l_class_OC_BaseEvent *llvm_cbe_call25;
  struct l_struct_OC_SS1apUFDownlinkNASTransport *llvm_cbe_tmp26;
  unsigned int llvm_cbe_tmp29;
  struct l_struct_OC_SS1apUFDownlinkNASTransport *llvm_cbe_tmp30;
  unsigned char llvm_cbe_tmp34;
  struct l_class_OC_BaseEvent *llvm_cbe_call38;
  struct l_class_OC_BaseEvent *llvm_cbe_call40;
  struct l_class_OC_Uec_KD__KD_UecManagerBase *llvm_cbe_call42;
  unsigned int llvm_cbe_call44;
  struct l_class_OC_BaseEvent *llvm_cbe_call46;
  struct l_class_OC_BaseEvent *llvm_cbe_call48;
  struct l_class_OC_BaseEvent *llvm_cbe_call50;
  struct l_struct_OC_SS1apUFDownlinkNASTransport *llvm_cbe_tmp51;
  unsigned char llvm_cbe_tmp55;
  struct l_class_OC_BaseEvent *llvm_cbe_call57;
  struct l_class_OC_BaseEvent *llvm_cbe_call59;
  struct l_class_OC_BaseEvent *llvm_cbe_call61;
  struct l_class_OC_BaseEvent *llvm_cbe_call63;
  struct l_class_OC_BaseEvent *llvm_cbe_call65;
  struct l_struct_OC_SS1apUFDownlinkNASTransport *llvm_cbe_tmp67;
  unsigned int llvm_cbe_tmp70;
  struct l_struct_OC_SS1apUFDownlinkNASTransport *llvm_cbe_tmp73;
  unsigned char llvm_cbe_tmp77;
  struct l_class_OC_BaseEvent *llvm_cbe_call82;
  struct l_class_OC_BaseEvent *llvm_cbe_call84;
  struct l_class_OC_Uec_KD__KD_UecManagerBase *llvm_cbe_call86;
  unsigned int llvm_cbe_call88;
  struct l_class_OC_BaseEvent *llvm_cbe_call90;
  struct l_class_OC_BaseEvent *llvm_cbe_call92;
  struct l_class_OC_BaseEvent *llvm_cbe_call94;
  struct l_struct_OC_SS1apUFDownlinkNASTransport *llvm_cbe_tmp95;
  unsigned char llvm_cbe_tmp99;
  struct l_class_OC_BaseEvent *llvm_cbe_call101;
  struct l_class_OC_BaseEvent *llvm_cbe_call103;
  struct l_class_OC_BaseEvent *llvm_cbe_call105;
  struct l_class_OC_BaseEvent *llvm_cbe_call107;
  struct l_class_OC_BaseEvent *llvm_cbe_call109;
  struct l_struct_OC_SS1apUFDownlinkNASTransport *llvm_cbe_tmp111;
  unsigned int llvm_cbe_tmp114;
  struct l_struct_OC_SS1apUFDownlinkNASTransport *llvm_cbe_tmp117;
  unsigned char llvm_cbe_tmp121;
  struct l_class_OC_BaseEvent *llvm_cbe_call126;
  struct l_class_OC_BaseEvent *llvm_cbe_call128;
  struct l_class_OC_Uec_KD__KD_UecManagerBase *llvm_cbe_call130;
  unsigned int llvm_cbe_call132;
  struct l_class_OC_BaseEvent *llvm_cbe_call134;
  struct l_class_OC_BaseEvent *llvm_cbe_call136;
  struct l_class_OC_BaseEvent *llvm_cbe_call138;
  struct l_struct_OC_SS1apUFDownlinkNASTransport *llvm_cbe_tmp139;
  unsigned char llvm_cbe_tmp143;
  struct l_class_OC_BaseEvent *llvm_cbe_call145;
  struct l_class_OC_BaseEvent *llvm_cbe_call147;
  struct l_class_OC_BaseEvent *llvm_cbe_call149;
  struct l_class_OC_BaseEvent *llvm_cbe_call151;
  struct l_class_OC_BaseEvent *llvm_cbe_call153;
  struct l_class_OC_Uec_KD__KD_UecUeContextData *llvm_cbe_tmp155;
  struct l_struct_OC_SS1apUFDownlinkNASTransport *llvm_cbe_tmp156;
  struct l_class_OC_BaseEvent *llvm_cbe_call160;
  struct l_class_OC_BaseEvent *llvm_cbe_call162;
  struct l_class_OC_Uec_KD__KD_UecManagerBase *llvm_cbe_call164;
  unsigned int llvm_cbe_call166;
  struct l_class_OC_BaseEvent *llvm_cbe_call168;
  struct l_class_OC_BaseEvent *llvm_cbe_call170;
  struct l_class_OC_BaseEvent *llvm_cbe_call172;
  struct l_class_OC_BaseEvent *llvm_cbe_call174;
  struct l_struct_OC_SS1apUFDownlinkNASTransport *llvm_cbe_tmp175;
  unsigned char llvm_cbe_tmp179;
  struct l_class_OC_BaseEvent *llvm_cbe_call181;
  struct l_class_OC_BaseEvent *llvm_cbe_call183;
  struct l_class_OC_BaseEvent *llvm_cbe_call185;
  struct l_class_OC_BaseEvent *llvm_cbe_call187;
  struct l_struct_OC_SS1apUFDownlinkNASTransport *llvm_cbe_tmp188;
  unsigned char llvm_cbe_tmp192;
  struct l_class_OC_BaseEvent *llvm_cbe_call194;
  struct l_class_OC_BaseEvent *llvm_cbe_call196;
  struct l_class_OC_BaseEvent *llvm_cbe_call198;
  struct l_class_OC_BaseEvent *llvm_cbe_call200;
  struct l_struct_OC_SS1apUFDownlinkNASTransport *llvm_cbe_tmp201;
  unsigned char llvm_cbe_tmp205;
  struct l_class_OC_BaseEvent *llvm_cbe_call207;
  struct l_class_OC_BaseEvent *llvm_cbe_call209;
  unsigned int llvm_cbe_tmp214;
  unsigned int llvm_cbe_tmp217;
  unsigned int llvm_cbe_tmp__52;

#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_this_2e_addr) = llvm_cbe_this;
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_p_asn1PayloadPtr_2e_addr) = llvm_cbe_p_asn1PayloadPtr;
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_p_uecUeContextDataPtr_2e_addr) = llvm_cbe_p_uecUeContextDataPtr;
#line 0 "LLVM INTERNAL"
  llvm_cbe_this1 = *(&llvm_cbe_this_2e_addr);
#line 1064 "UecDirectTransfer.cpp"
  _ZNSaIcEC1Ev((&llvm_cbe_agg_2e_tmp));
#line 1064 "UecDirectTransfer.cpp"
  _ZNSsC1EPKcRKSaIcE((&llvm_cbe__currentFunction_), ((&__PRETTY_FUNCTION___OC__ZN3Uec17UecDirectTransfer31handleEmbeddedHoRestrictionListEPK27SS1apUFDownlinkNASTransportPNS_16UecUeContextDataE.array[((signed int )0u)])), (&llvm_cbe_agg_2e_tmp));
#line 1064 "UecDirectTransfer.cpp"
  _ZNSaIcED1Ev((&llvm_cbe_agg_2e_tmp));
#line 1064 "UecDirectTransfer.cpp"
  _ZN3Uec17UecDebugCallstackC1ERKSs((&llvm_cbe__callstackTmp_), (&llvm_cbe__currentFunction_));
#line 1067 "UecDirectTransfer.cpp"
  llvm_cbe_tmp = *(&llvm_cbe_p_asn1PayloadPtr_2e_addr);
#line 1067 "UecDirectTransfer.cpp"
  llvm_cbe_tmp14 = *((&llvm_cbe_tmp->field3));
#line 1067 "UecDirectTransfer.cpp"
  if ((llvm_cbe_tmp14 != 0u)) {    goto llvm_cbe_if_2e_end;  } else {    goto llvm_cbe_if_2e_then;  }


llvm_cbe_if_2e_then:
#line 1069 "UecDirectTransfer.cpp"
  llvm_cbe_call = _ZN11DummyStreamlsIPKcEERS_T_((&mout), ((&_OC_str68.array[((signed int )0u)])));
#line 1069 "UecDirectTransfer.cpp"
  llvm_cbe_call17 = _ZN11DummyStreamlsIPFRSt8ios_baseS2_EEERS_T_(llvm_cbe_call, _ZSt3decRSt8ios_base);
#line 1069 "UecDirectTransfer.cpp"
  llvm_cbe_call19 = _ZNK3Uec14UecServiceBase13getUecManagerEv((((struct l_class_OC_Uec_KD__KD_UecServiceBase *)llvm_cbe_this1)));
#line 1069 "UecDirectTransfer.cpp"
  llvm_cbe_call21 = _ZNK3Uec14UecManagerBase13getInstanceIdEv(llvm_cbe_call19);
#line 1069 "UecDirectTransfer.cpp"
  llvm_cbe_call23 = _ZN11DummyStreamlsIjEERS_T_(llvm_cbe_call17, llvm_cbe_call21);
#line 1069 "UecDirectTransfer.cpp"
  llvm_cbe_call25 = _ZN11DummyStreamlsIPKcEERS_T_(llvm_cbe_call23, ((&_OC_str69.array[((signed int )0u)])));
#line 1072 "UecDirectTransfer.cpp"
  *(&llvm_cbe_retval) = 1u;
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_cleanup_2e_dst) = 1u;
#line 1072 "UecDirectTransfer.cpp"
  goto llvm_cbe_cleanup;

llvm_cbe_if_2e_end:
#line 1077 "UecDirectTransfer.cpp"
  llvm_cbe_tmp26 = *(&llvm_cbe_p_asn1PayloadPtr_2e_addr);
#line 1077 "UecDirectTransfer.cpp"
  llvm_cbe_tmp29 = *((&((&llvm_cbe_tmp26->field4))->field1));
#line 1077 "UecDirectTransfer.cpp"
  if ((llvm_cbe_tmp29 == 1u)) {    goto llvm_cbe_land_2e_lhs_2e_true;  } else {    goto llvm_cbe_if_2e_end66;  }


llvm_cbe_land_2e_lhs_2e_true:
#line 1077 "UecDirectTransfer.cpp"
  llvm_cbe_tmp30 = *(&llvm_cbe_p_asn1PayloadPtr_2e_addr);
#line 1077 "UecDirectTransfer.cpp"
  llvm_cbe_tmp34 = *((&((&((&llvm_cbe_tmp30->field4))->field2))->field0));
#line 1077 "UecDirectTransfer.cpp"
  if ((((signed int )(((unsigned int )(unsigned char )llvm_cbe_tmp34))) > ((signed int )15u))) {    goto llvm_cbe_if_2e_then36;  } else {    goto llvm_cbe_if_2e_end66;  }


llvm_cbe_if_2e_then36:
#line 1080 "UecDirectTransfer.cpp"
  llvm_cbe_call38 = _ZN11DummyStreamlsIPKcEERS_T_((&mout), ((&_OC_str68.array[((signed int )0u)])));
#line 1080 "UecDirectTransfer.cpp"
  llvm_cbe_call40 = _ZN11DummyStreamlsIPFRSt8ios_baseS2_EEERS_T_(llvm_cbe_call38, _ZSt3decRSt8ios_base);
#line 1080 "UecDirectTransfer.cpp"
  llvm_cbe_call42 = _ZNK3Uec14UecServiceBase13getUecManagerEv((((struct l_class_OC_Uec_KD__KD_UecServiceBase *)llvm_cbe_this1)));
#line 1080 "UecDirectTransfer.cpp"
  llvm_cbe_call44 = _ZNK3Uec14UecManagerBase13getInstanceIdEv(llvm_cbe_call42);
#line 1080 "UecDirectTransfer.cpp"
  llvm_cbe_call46 = _ZN11DummyStreamlsIjEERS_T_(llvm_cbe_call40, llvm_cbe_call44);
#line 1080 "UecDirectTransfer.cpp"
  llvm_cbe_call48 = _ZN11DummyStreamlsIPKcEERS_T_(llvm_cbe_call46, ((&_OC_str70.array[((signed int )0u)])));
#line 1080 "UecDirectTransfer.cpp"
  llvm_cbe_call50 = _ZN11DummyStreamlsIPFRSt8ios_baseS2_EEERS_T_(llvm_cbe_call48, _ZSt3decRSt8ios_base);
#line 1080 "UecDirectTransfer.cpp"
  llvm_cbe_tmp51 = *(&llvm_cbe_p_asn1PayloadPtr_2e_addr);
#line 1080 "UecDirectTransfer.cpp"
  llvm_cbe_tmp55 = *((&((&((&llvm_cbe_tmp51->field4))->field2))->field0));
#line 1080 "UecDirectTransfer.cpp"
  llvm_cbe_call57 = _ZN11DummyStreamlsIhEERS_T_(llvm_cbe_call50, llvm_cbe_tmp55);
#line 1080 "UecDirectTransfer.cpp"
  llvm_cbe_call59 = _ZN11DummyStreamlsIPKcEERS_T_(llvm_cbe_call57, ((&_OC_str71.array[((signed int )0u)])));
#line 1080 "UecDirectTransfer.cpp"
  llvm_cbe_call61 = _ZN11DummyStreamlsIPFRSt8ios_baseS2_EEERS_T_(llvm_cbe_call59, _ZSt3decRSt8ios_base);
#line 1080 "UecDirectTransfer.cpp"
  llvm_cbe_call63 = _ZN11DummyStreamlsIiEERS_T_(llvm_cbe_call61, 15u);
#line 1080 "UecDirectTransfer.cpp"
  llvm_cbe_call65 = _ZN11DummyStreamlsIPKcEERS_T_(llvm_cbe_call63, ((&_OC_str72.array[((signed int )0u)])));
#line 1083 "UecDirectTransfer.cpp"
  *(&llvm_cbe_retval) = 4294967295u;
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_cleanup_2e_dst) = 2u;
#line 1083 "UecDirectTransfer.cpp"
  goto llvm_cbe_cleanup;

llvm_cbe_if_2e_end66:
#line 1088 "UecDirectTransfer.cpp"
  llvm_cbe_tmp67 = *(&llvm_cbe_p_asn1PayloadPtr_2e_addr);
#line 1088 "UecDirectTransfer.cpp"
  llvm_cbe_tmp70 = *((&((&llvm_cbe_tmp67->field4))->field3));
#line 1088 "UecDirectTransfer.cpp"
  if ((llvm_cbe_tmp70 == 1u)) {    goto llvm_cbe_land_2e_lhs_2e_true72;  } else {    goto llvm_cbe_if_2e_end110;  }


llvm_cbe_land_2e_lhs_2e_true72:
#line 1088 "UecDirectTransfer.cpp"
  llvm_cbe_tmp73 = *(&llvm_cbe_p_asn1PayloadPtr_2e_addr);
#line 1088 "UecDirectTransfer.cpp"
  llvm_cbe_tmp77 = *((&((&((&llvm_cbe_tmp73->field4))->field4))->field0));
#line 1088 "UecDirectTransfer.cpp"
  if ((((signed int )(((unsigned int )(unsigned char )llvm_cbe_tmp77))) > ((signed int )16u))) {    goto llvm_cbe_if_2e_then80;  } else {    goto llvm_cbe_if_2e_end110;  }


llvm_cbe_if_2e_then80:
#line 1091 "UecDirectTransfer.cpp"
  llvm_cbe_call82 = _ZN11DummyStreamlsIPKcEERS_T_((&mout), ((&_OC_str68.array[((signed int )0u)])));
#line 1091 "UecDirectTransfer.cpp"
  llvm_cbe_call84 = _ZN11DummyStreamlsIPFRSt8ios_baseS2_EEERS_T_(llvm_cbe_call82, _ZSt3decRSt8ios_base);
#line 1091 "UecDirectTransfer.cpp"
  llvm_cbe_call86 = _ZNK3Uec14UecServiceBase13getUecManagerEv((((struct l_class_OC_Uec_KD__KD_UecServiceBase *)llvm_cbe_this1)));
#line 1091 "UecDirectTransfer.cpp"
  llvm_cbe_call88 = _ZNK3Uec14UecManagerBase13getInstanceIdEv(llvm_cbe_call86);
#line 1091 "UecDirectTransfer.cpp"
  llvm_cbe_call90 = _ZN11DummyStreamlsIjEERS_T_(llvm_cbe_call84, llvm_cbe_call88);
#line 1091 "UecDirectTransfer.cpp"
  llvm_cbe_call92 = _ZN11DummyStreamlsIPKcEERS_T_(llvm_cbe_call90, ((&_OC_str73.array[((signed int )0u)])));
#line 1091 "UecDirectTransfer.cpp"
  llvm_cbe_call94 = _ZN11DummyStreamlsIPFRSt8ios_baseS2_EEERS_T_(llvm_cbe_call92, _ZSt3decRSt8ios_base);
#line 1091 "UecDirectTransfer.cpp"
  llvm_cbe_tmp95 = *(&llvm_cbe_p_asn1PayloadPtr_2e_addr);
#line 1091 "UecDirectTransfer.cpp"
  llvm_cbe_tmp99 = *((&((&((&llvm_cbe_tmp95->field4))->field4))->field0));
#line 1091 "UecDirectTransfer.cpp"
  llvm_cbe_call101 = _ZN11DummyStreamlsIhEERS_T_(llvm_cbe_call94, llvm_cbe_tmp99);
#line 1091 "UecDirectTransfer.cpp"
  llvm_cbe_call103 = _ZN11DummyStreamlsIPKcEERS_T_(llvm_cbe_call101, ((&_OC_str71.array[((signed int )0u)])));
#line 1091 "UecDirectTransfer.cpp"
  llvm_cbe_call105 = _ZN11DummyStreamlsIPFRSt8ios_baseS2_EEERS_T_(llvm_cbe_call103, _ZSt3decRSt8ios_base);
#line 1091 "UecDirectTransfer.cpp"
  llvm_cbe_call107 = _ZN11DummyStreamlsIiEERS_T_(llvm_cbe_call105, 16u);
#line 1091 "UecDirectTransfer.cpp"
  llvm_cbe_call109 = _ZN11DummyStreamlsIPKcEERS_T_(llvm_cbe_call107, ((&_OC_str72.array[((signed int )0u)])));
#line 1094 "UecDirectTransfer.cpp"
  *(&llvm_cbe_retval) = 4294967295u;
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_cleanup_2e_dst) = 3u;
#line 1094 "UecDirectTransfer.cpp"
  goto llvm_cbe_cleanup;

llvm_cbe_if_2e_end110:
#line 1099 "UecDirectTransfer.cpp"
  llvm_cbe_tmp111 = *(&llvm_cbe_p_asn1PayloadPtr_2e_addr);
#line 1099 "UecDirectTransfer.cpp"
  llvm_cbe_tmp114 = *((&((&llvm_cbe_tmp111->field4))->field5));
#line 1099 "UecDirectTransfer.cpp"
  if ((llvm_cbe_tmp114 == 1u)) {    goto llvm_cbe_land_2e_lhs_2e_true116;  } else {    goto llvm_cbe_if_2e_end154;  }


llvm_cbe_land_2e_lhs_2e_true116:
#line 1099 "UecDirectTransfer.cpp"
  llvm_cbe_tmp117 = *(&llvm_cbe_p_asn1PayloadPtr_2e_addr);
#line 1099 "UecDirectTransfer.cpp"
  llvm_cbe_tmp121 = *((&((&((&llvm_cbe_tmp117->field4))->field6))->field0));
#line 1099 "UecDirectTransfer.cpp"
  if ((((signed int )(((unsigned int )(unsigned char )llvm_cbe_tmp121))) > ((signed int )16u))) {    goto llvm_cbe_if_2e_then124;  } else {    goto llvm_cbe_if_2e_end154;  }


llvm_cbe_if_2e_then124:
#line 1102 "UecDirectTransfer.cpp"
  llvm_cbe_call126 = _ZN11DummyStreamlsIPKcEERS_T_((&mout), ((&_OC_str68.array[((signed int )0u)])));
#line 1102 "UecDirectTransfer.cpp"
  llvm_cbe_call128 = _ZN11DummyStreamlsIPFRSt8ios_baseS2_EEERS_T_(llvm_cbe_call126, _ZSt3decRSt8ios_base);
#line 1102 "UecDirectTransfer.cpp"
  llvm_cbe_call130 = _ZNK3Uec14UecServiceBase13getUecManagerEv((((struct l_class_OC_Uec_KD__KD_UecServiceBase *)llvm_cbe_this1)));
#line 1102 "UecDirectTransfer.cpp"
  llvm_cbe_call132 = _ZNK3Uec14UecManagerBase13getInstanceIdEv(llvm_cbe_call130);
#line 1102 "UecDirectTransfer.cpp"
  llvm_cbe_call134 = _ZN11DummyStreamlsIjEERS_T_(llvm_cbe_call128, llvm_cbe_call132);
#line 1102 "UecDirectTransfer.cpp"
  llvm_cbe_call136 = _ZN11DummyStreamlsIPKcEERS_T_(llvm_cbe_call134, ((&_OC_str74.array[((signed int )0u)])));
#line 1102 "UecDirectTransfer.cpp"
  llvm_cbe_call138 = _ZN11DummyStreamlsIPFRSt8ios_baseS2_EEERS_T_(llvm_cbe_call136, _ZSt3decRSt8ios_base);
#line 1102 "UecDirectTransfer.cpp"
  llvm_cbe_tmp139 = *(&llvm_cbe_p_asn1PayloadPtr_2e_addr);
#line 1102 "UecDirectTransfer.cpp"
  llvm_cbe_tmp143 = *((&((&((&llvm_cbe_tmp139->field4))->field6))->field0));
#line 1102 "UecDirectTransfer.cpp"
  llvm_cbe_call145 = _ZN11DummyStreamlsIhEERS_T_(llvm_cbe_call138, llvm_cbe_tmp143);
#line 1102 "UecDirectTransfer.cpp"
  llvm_cbe_call147 = _ZN11DummyStreamlsIPKcEERS_T_(llvm_cbe_call145, ((&_OC_str71.array[((signed int )0u)])));
#line 1102 "UecDirectTransfer.cpp"
  llvm_cbe_call149 = _ZN11DummyStreamlsIPFRSt8ios_baseS2_EEERS_T_(llvm_cbe_call147, _ZSt3decRSt8ios_base);
#line 1102 "UecDirectTransfer.cpp"
  llvm_cbe_call151 = _ZN11DummyStreamlsIiEERS_T_(llvm_cbe_call149, 16u);
#line 1102 "UecDirectTransfer.cpp"
  llvm_cbe_call153 = _ZN11DummyStreamlsIPKcEERS_T_(llvm_cbe_call151, ((&_OC_str72.array[((signed int )0u)])));
#line 1105 "UecDirectTransfer.cpp"
  *(&llvm_cbe_retval) = 4294967295u;
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_cleanup_2e_dst) = 4u;
#line 1105 "UecDirectTransfer.cpp"
  goto llvm_cbe_cleanup;

llvm_cbe_if_2e_end154:
#line 1110 "UecDirectTransfer.cpp"
  llvm_cbe_tmp155 = *(&llvm_cbe_p_uecUeContextDataPtr_2e_addr);
#line 1110 "UecDirectTransfer.cpp"
  llvm_cbe_tmp156 = *(&llvm_cbe_p_asn1PayloadPtr_2e_addr);
#line 1110 "UecDirectTransfer.cpp"
  _ZN3Uec16UecUeContextData26setHandoverRestrictionListERK28SS1apHandoverRestrictionList(llvm_cbe_tmp155, ((&llvm_cbe_tmp156->field4)));
#line 1112 "UecDirectTransfer.cpp"
  llvm_cbe_call160 = _ZN11DummyStreamlsIPKcEERS_T_((&mout), ((&_OC_str68.array[((signed int )0u)])));
#line 1112 "UecDirectTransfer.cpp"
  llvm_cbe_call162 = _ZN11DummyStreamlsIPFRSt8ios_baseS2_EEERS_T_(llvm_cbe_call160, _ZSt3decRSt8ios_base);
#line 1112 "UecDirectTransfer.cpp"
  llvm_cbe_call164 = _ZNK3Uec14UecServiceBase13getUecManagerEv((((struct l_class_OC_Uec_KD__KD_UecServiceBase *)llvm_cbe_this1)));
#line 1112 "UecDirectTransfer.cpp"
  llvm_cbe_call166 = _ZNK3Uec14UecManagerBase13getInstanceIdEv(llvm_cbe_call164);
#line 1112 "UecDirectTransfer.cpp"
  llvm_cbe_call168 = _ZN11DummyStreamlsIjEERS_T_(llvm_cbe_call162, llvm_cbe_call166);
#line 1112 "UecDirectTransfer.cpp"
  llvm_cbe_call170 = _ZN11DummyStreamlsIPKcEERS_T_(llvm_cbe_call168, ((&_OC_str75.array[((signed int )0u)])));
#line 1112 "UecDirectTransfer.cpp"
  llvm_cbe_call172 = _ZN11DummyStreamlsIPKcEERS_T_(llvm_cbe_call170, ((&_OC_str76.array[((signed int )0u)])));
#line 1112 "UecDirectTransfer.cpp"
  llvm_cbe_call174 = _ZN11DummyStreamlsIPFRSt8ios_baseS2_EEERS_T_(llvm_cbe_call172, _ZSt3decRSt8ios_base);
#line 1112 "UecDirectTransfer.cpp"
  llvm_cbe_tmp175 = *(&llvm_cbe_p_asn1PayloadPtr_2e_addr);
#line 1112 "UecDirectTransfer.cpp"
  llvm_cbe_tmp179 = *((&((&((&llvm_cbe_tmp175->field4))->field2))->field0));
#line 1112 "UecDirectTransfer.cpp"
  llvm_cbe_call181 = _ZN11DummyStreamlsIhEERS_T_(llvm_cbe_call174, llvm_cbe_tmp179);
#line 1112 "UecDirectTransfer.cpp"
  llvm_cbe_call183 = _ZN11DummyStreamlsIPKcEERS_T_(llvm_cbe_call181, ((&_OC_str77.array[((signed int )0u)])));
#line 1112 "UecDirectTransfer.cpp"
  llvm_cbe_call185 = _ZN11DummyStreamlsIPKcEERS_T_(llvm_cbe_call183, ((&_OC_str78.array[((signed int )0u)])));
#line 1112 "UecDirectTransfer.cpp"
  llvm_cbe_call187 = _ZN11DummyStreamlsIPFRSt8ios_baseS2_EEERS_T_(llvm_cbe_call185, _ZSt3decRSt8ios_base);
#line 1112 "UecDirectTransfer.cpp"
  llvm_cbe_tmp188 = *(&llvm_cbe_p_asn1PayloadPtr_2e_addr);
#line 1112 "UecDirectTransfer.cpp"
  llvm_cbe_tmp192 = *((&((&((&llvm_cbe_tmp188->field4))->field4))->field0));
#line 1112 "UecDirectTransfer.cpp"
  llvm_cbe_call194 = _ZN11DummyStreamlsIhEERS_T_(llvm_cbe_call187, llvm_cbe_tmp192);
#line 1112 "UecDirectTransfer.cpp"
  llvm_cbe_call196 = _ZN11DummyStreamlsIPKcEERS_T_(llvm_cbe_call194, ((&_OC_str77.array[((signed int )0u)])));
#line 1112 "UecDirectTransfer.cpp"
  llvm_cbe_call198 = _ZN11DummyStreamlsIPKcEERS_T_(llvm_cbe_call196, ((&_OC_str79.array[((signed int )0u)])));
#line 1112 "UecDirectTransfer.cpp"
  llvm_cbe_call200 = _ZN11DummyStreamlsIPFRSt8ios_baseS2_EEERS_T_(llvm_cbe_call198, _ZSt3decRSt8ios_base);
#line 1112 "UecDirectTransfer.cpp"
  llvm_cbe_tmp201 = *(&llvm_cbe_p_asn1PayloadPtr_2e_addr);
#line 1112 "UecDirectTransfer.cpp"
  llvm_cbe_tmp205 = *((&((&((&llvm_cbe_tmp201->field4))->field6))->field0));
#line 1112 "UecDirectTransfer.cpp"
  llvm_cbe_call207 = _ZN11DummyStreamlsIhEERS_T_(llvm_cbe_call200, llvm_cbe_tmp205);
#line 1112 "UecDirectTransfer.cpp"
  llvm_cbe_call209 = _ZN11DummyStreamlsIPKcEERS_T_(llvm_cbe_call207, ((&_OC_str80.array[((signed int )0u)])));
#line 1118 "UecDirectTransfer.cpp"
  *(&llvm_cbe_retval) = 1u;
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_cleanup_2e_dst) = 5u;
#line 1118 "UecDirectTransfer.cpp"
  goto llvm_cbe_cleanup;

llvm_cbe_cleanup_2e_pad:
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_cleanup_2e_dst215) = 1u;
#line 0 "LLVM INTERNAL"
  goto llvm_cbe_cleanup216;

llvm_cbe_cleanup_2e_pad210:
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_cleanup_2e_dst215) = 1u;
#line 0 "LLVM INTERNAL"
  goto llvm_cbe_cleanup216;

llvm_cbe_cleanup_2e_pad211:
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_cleanup_2e_dst215) = 1u;
#line 0 "LLVM INTERNAL"
  goto llvm_cbe_cleanup216;

llvm_cbe_cleanup_2e_pad212:
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_cleanup_2e_dst215) = 1u;
#line 0 "LLVM INTERNAL"
  goto llvm_cbe_cleanup216;

llvm_cbe_cleanup_2e_pad213:
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_cleanup_2e_dst215) = 1u;
#line 0 "LLVM INTERNAL"
  goto llvm_cbe_cleanup216;

llvm_cbe_cleanup:
#line 1119 "UecDirectTransfer.cpp"
  _ZN3Uec17UecDebugCallstackD1Ev((&llvm_cbe__callstackTmp_));
#line 1119 "UecDirectTransfer.cpp"
  llvm_cbe_tmp214 = *(&llvm_cbe_cleanup_2e_dst);
#line 1119 "UecDirectTransfer.cpp"
  switch (llvm_cbe_tmp214) {
  default:
    goto llvm_cbe_cleanup_2e_end;;
  case 1u:
    goto llvm_cbe_cleanup_2e_pad;  case 2u:
    goto llvm_cbe_cleanup_2e_pad210;  case 3u:
    goto llvm_cbe_cleanup_2e_pad211;  case 4u:
    goto llvm_cbe_cleanup_2e_pad212;  case 5u:
    goto llvm_cbe_cleanup_2e_pad213;  }

llvm_cbe_cleanup_2e_end:
#line 1119 "UecDirectTransfer.cpp"
  *(&llvm_cbe_cleanup_2e_dst215) = 0u;
#line 1119 "UecDirectTransfer.cpp"
  goto llvm_cbe_cleanup216;

llvm_cbe_cleanup216:
#line 1119 "UecDirectTransfer.cpp"
  _ZNSsD1Ev((&llvm_cbe__currentFunction_));
#line 1119 "UecDirectTransfer.cpp"
  llvm_cbe_tmp217 = *(&llvm_cbe_cleanup_2e_dst215);
#line 1119 "UecDirectTransfer.cpp"
  llvm_cbe_tmp__52 = *(&llvm_cbe_retval);
#line 1119 "UecDirectTransfer.cpp"
  return llvm_cbe_tmp__52;
}


#line 0 "LLVM INTERNAL"
unsigned int _ZN3Uec17UecDirectTransfer28handleTupL3MessageIndicationEPNS_8UecEventE(struct l_class_OC_Uec_KD__KD_UecDirectTransfer *llvm_cbe_this, struct l_class_OC_Uec_KD__KD_UecEvent *llvm_cbe_p_uecEvent_ptr) {
  unsigned int llvm_cbe_retval;    /* Address-exposed local */
  struct l_class_OC_Uec_KD__KD_UecDirectTransfer *llvm_cbe_this_2e_addr;    /* Address-exposed local */
  struct l_class_OC_Uec_KD__KD_UecEvent *llvm_cbe_p_uecEvent_ptr_2e_addr;    /* Address-exposed local */
  struct l_class_OC_std_KD__KD_basic_string llvm_cbe__currentFunction_;    /* Address-exposed local */
  struct l_class_OC_BaseEvent llvm_cbe_agg_2e_tmp;    /* Address-exposed local */
  struct l_class_OC_Uec_KD__KD_UecDebugCallstack llvm_cbe__callstackTmp_;    /* Address-exposed local */
  struct l_struct_OC_TUP_L3MessageInd *llvm_cbe_l_payLoad_ptr;    /* Address-exposed local */
  struct l_class_OC_Uec_KD__KD_UecUeContextData *llvm_cbe_l_uecUeContextDataWrite_ptr;    /* Address-exposed local */
  unsigned int llvm_cbe_cleanup_2e_dst;    /* Address-exposed local */
  unsigned int llvm_cbe_cleanup_2e_dst92;    /* Address-exposed local */
  struct l_class_OC_Uec_KD__KD_UecDirectTransfer *llvm_cbe_this1;
  struct l_class_OC_Uec_KD__KD_UecEvent *llvm_cbe_tmp;
  unsigned char *llvm_cbe_call;
  struct l_class_OC_BaseEvent *llvm_cbe_call15;
  struct l_class_OC_BaseEvent *llvm_cbe_call17;
  struct l_class_OC_Uec_KD__KD_UecEvent *llvm_cbe_tmp18;
  unsigned int llvm_cbe_call20;
  struct l_class_OC_BaseEvent *llvm_cbe_call22;
  struct l_class_OC_BaseEvent *llvm_cbe_call24;
  struct l_class_OC_BaseEvent *llvm_cbe_call26;
  struct l_struct_OC_TUP_L3MessageInd *llvm_cbe_tmp27;
  unsigned int llvm_cbe_tmp29;
  struct l_class_OC_BaseEvent *llvm_cbe_call31;
  struct l_class_OC_Uec_KD__KD_UecBaseData *llvm_cbe_call34;
  struct l_class_OC_Uec_KD__KD_UecBaseData * (**llvm_cbe_tmp__53) (struct l_class_OC_Uec_KD__KD_UecBaseData *, unsigned int );
  struct l_class_OC_Uec_KD__KD_UecBaseData * (*llvm_cbe_tmp__54) (struct l_class_OC_Uec_KD__KD_UecBaseData *, unsigned int );
  struct l_class_OC_Uec_KD__KD_UecBaseData *llvm_cbe_call36;
  struct l_class_OC_Uec_KD__KD_UecManagerBase *llvm_cbe_call38;
  unsigned int llvm_cbe_call40;
  struct l_struct_OC_TUP_L3MessageInd *llvm_cbe_tmp41;
  unsigned int llvm_cbe_tmp43;
  struct l_class_OC_Uec_KD__KD_UecUeContextData *llvm_cbe_tmp44;
  unsigned int llvm_cbe_call46;
  struct l_struct_OC_TUP_L3MessageInd *llvm_cbe_tmp47;
  unsigned int llvm_cbe_tmp49;
  struct l_class_OC_BaseEvent *llvm_cbe_call52;
  struct l_class_OC_BaseEvent *llvm_cbe_call54;
  struct l_class_OC_Uec_KD__KD_UecManagerBase *llvm_cbe_call56;
  unsigned int llvm_cbe_call58;
  struct l_class_OC_BaseEvent *llvm_cbe_call60;
  struct l_class_OC_BaseEvent *llvm_cbe_call62;
  struct l_class_OC_BaseEvent *llvm_cbe_call64;
  struct l_struct_OC_TUP_L3MessageInd *llvm_cbe_tmp65;
  unsigned int llvm_cbe_tmp67;
  struct l_class_OC_BaseEvent *llvm_cbe_call69;
  struct l_class_OC_BaseEvent *llvm_cbe_call71;
  struct l_class_OC_BaseEvent *llvm_cbe_call73;
  struct l_class_OC_Uec_KD__KD_UecUeContextData *llvm_cbe_tmp74;
  unsigned int llvm_cbe_call76;
  struct l_class_OC_BaseEvent *llvm_cbe_call78;
  struct l_class_OC_BaseEvent *llvm_cbe_call80;
  struct l_class_OC_BaseEvent *llvm_cbe_call82;
  struct l_struct_OC_TUP_L3MessageInd *llvm_cbe_tmp83;
  unsigned int llvm_cbe_tmp85;
  struct l_class_OC_BaseEvent *llvm_cbe_call87;
  struct l_class_OC_BaseEvent *llvm_cbe_call89;
  unsigned int llvm_cbe_tmp91;
  unsigned int llvm_cbe_tmp94;
  unsigned int llvm_cbe_tmp__55;

#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_this_2e_addr) = llvm_cbe_this;
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_p_uecEvent_ptr_2e_addr) = llvm_cbe_p_uecEvent_ptr;
#line 0 "LLVM INTERNAL"
  llvm_cbe_this1 = *(&llvm_cbe_this_2e_addr);
#line 311 "UecDirectTransfer.cpp"
  _ZNSaIcEC1Ev((&llvm_cbe_agg_2e_tmp));
#line 311 "UecDirectTransfer.cpp"
  _ZNSsC1EPKcRKSaIcE((&llvm_cbe__currentFunction_), ((&__PRETTY_FUNCTION___OC__ZN3Uec17UecDirectTransfer28handleTupL3MessageIndicationEPNS_8UecEventE.array[((signed int )0u)])), (&llvm_cbe_agg_2e_tmp));
#line 311 "UecDirectTransfer.cpp"
  _ZNSaIcED1Ev((&llvm_cbe_agg_2e_tmp));
#line 311 "UecDirectTransfer.cpp"
  _ZN3Uec17UecDebugCallstackC1ERKSs((&llvm_cbe__callstackTmp_), (&llvm_cbe__currentFunction_));
#line 313 "UecDirectTransfer.cpp"
  llvm_cbe_tmp = *(&llvm_cbe_p_uecEvent_ptr_2e_addr);
#line 313 "UecDirectTransfer.cpp"
  llvm_cbe_call = _ZNK3Uec8UecEvent10getPayLoadEv(llvm_cbe_tmp);
#line 313 "UecDirectTransfer.cpp"
  *(&llvm_cbe_l_payLoad_ptr) = (((struct l_struct_OC_TUP_L3MessageInd *)llvm_cbe_call));
#line 315 "UecDirectTransfer.cpp"
  llvm_cbe_call15 = _ZN11DummyStreamlsIPKcEERS_T_((&mout), ((&_OC_str22.array[((signed int )0u)])));
#line 315 "UecDirectTransfer.cpp"
  llvm_cbe_call17 = _ZN11DummyStreamlsIPFRSt8ios_baseS2_EEERS_T_(llvm_cbe_call15, _ZSt3decRSt8ios_base);
#line 315 "UecDirectTransfer.cpp"
  llvm_cbe_tmp18 = *(&llvm_cbe_p_uecEvent_ptr_2e_addr);
#line 315 "UecDirectTransfer.cpp"
  llvm_cbe_call20 = _ZNK3Uec8UecEvent13getInstanceIdEv(llvm_cbe_tmp18);
#line 315 "UecDirectTransfer.cpp"
  llvm_cbe_call22 = _ZN11DummyStreamlsIjEERS_T_(llvm_cbe_call17, llvm_cbe_call20);
#line 315 "UecDirectTransfer.cpp"
  llvm_cbe_call24 = _ZN11DummyStreamlsIPKcEERS_T_(llvm_cbe_call22, ((&_OC_str23.array[((signed int )0u)])));
#line 315 "UecDirectTransfer.cpp"
  llvm_cbe_call26 = _ZN11DummyStreamlsIPFRSt8ios_baseS2_EEERS_T_(llvm_cbe_call24, _ZSt3decRSt8ios_base);
#line 315 "UecDirectTransfer.cpp"
  llvm_cbe_tmp27 = *(&llvm_cbe_l_payLoad_ptr);
#line 315 "UecDirectTransfer.cpp"
  llvm_cbe_tmp29 = *((&llvm_cbe_tmp27->field0));
#line 315 "UecDirectTransfer.cpp"
  llvm_cbe_call31 = _ZN11DummyStreamlsIjEERS_T_(llvm_cbe_call26, llvm_cbe_tmp29);
#line 319 "UecDirectTransfer.cpp"
  llvm_cbe_call34 = _ZNK3Uec14UecServiceBase16getUecDataAccessEv((((struct l_class_OC_Uec_KD__KD_UecServiceBase *)llvm_cbe_this1)));
#line 319 "UecDirectTransfer.cpp"
  llvm_cbe_tmp__53 = *(((struct l_class_OC_Uec_KD__KD_UecBaseData * (***) (struct l_class_OC_Uec_KD__KD_UecBaseData *, unsigned int ))llvm_cbe_call34));
#line 319 "UecDirectTransfer.cpp"
  llvm_cbe_tmp__54 = *((&llvm_cbe_tmp__53[((signed long long )2ull)]));
#line 319 "UecDirectTransfer.cpp"
  llvm_cbe_call36 = llvm_cbe_tmp__54(llvm_cbe_call34, 1u);
#line 319 "UecDirectTransfer.cpp"
  *(&llvm_cbe_l_uecUeContextDataWrite_ptr) = (((struct l_class_OC_Uec_KD__KD_UecUeContextData *)llvm_cbe_call36));
#line 322 "UecDirectTransfer.cpp"
  llvm_cbe_call38 = _ZNK3Uec14UecServiceBase13getUecManagerEv((((struct l_class_OC_Uec_KD__KD_UecServiceBase *)llvm_cbe_this1)));
#line 322 "UecDirectTransfer.cpp"
  llvm_cbe_call40 = _ZNK3Uec14UecManagerBase13getInstanceIdEv(llvm_cbe_call38);
#line 322 "UecDirectTransfer.cpp"
  llvm_cbe_tmp41 = *(&llvm_cbe_l_payLoad_ptr);
#line 322 "UecDirectTransfer.cpp"
  llvm_cbe_tmp43 = *((&llvm_cbe_tmp41->field2));
#line 322 "UecDirectTransfer.cpp"
  if ((llvm_cbe_call40 != llvm_cbe_tmp43)) {    goto llvm_cbe_if_2e_then;  } else {    goto llvm_cbe_lor_2e_lhs_2e_false;  }


llvm_cbe_lor_2e_lhs_2e_false:
#line 322 "UecDirectTransfer.cpp"
  llvm_cbe_tmp44 = *(&llvm_cbe_l_uecUeContextDataWrite_ptr);
#line 322 "UecDirectTransfer.cpp"
  llvm_cbe_call46 = _ZNK3Uec16UecUeContextData11getS1LinkIdEv(llvm_cbe_tmp44);
#line 322 "UecDirectTransfer.cpp"
  llvm_cbe_tmp47 = *(&llvm_cbe_l_payLoad_ptr);
#line 322 "UecDirectTransfer.cpp"
  llvm_cbe_tmp49 = *((&llvm_cbe_tmp47->field0));
#line 322 "UecDirectTransfer.cpp"
  if ((llvm_cbe_call46 != llvm_cbe_tmp49)) {    goto llvm_cbe_if_2e_then;  } else {    goto llvm_cbe_if_2e_end;  }


llvm_cbe_if_2e_then:
#line 326 "UecDirectTransfer.cpp"
  llvm_cbe_call52 = _ZN11DummyStreamlsIPKcEERS_T_((&mout), ((&_OC_str22.array[((signed int )0u)])));
#line 326 "UecDirectTransfer.cpp"
  llvm_cbe_call54 = _ZN11DummyStreamlsIPFRSt8ios_baseS2_EEERS_T_(llvm_cbe_call52, _ZSt3decRSt8ios_base);
#line 326 "UecDirectTransfer.cpp"
  llvm_cbe_call56 = _ZNK3Uec14UecServiceBase13getUecManagerEv((((struct l_class_OC_Uec_KD__KD_UecServiceBase *)llvm_cbe_this1)));
#line 326 "UecDirectTransfer.cpp"
  llvm_cbe_call58 = _ZNK3Uec14UecManagerBase13getInstanceIdEv(llvm_cbe_call56);
#line 326 "UecDirectTransfer.cpp"
  llvm_cbe_call60 = _ZN11DummyStreamlsIjEERS_T_(llvm_cbe_call54, llvm_cbe_call58);
#line 326 "UecDirectTransfer.cpp"
  llvm_cbe_call62 = _ZN11DummyStreamlsIPKcEERS_T_(llvm_cbe_call60, ((&_OC_str24.array[((signed int )0u)])));
#line 326 "UecDirectTransfer.cpp"
  llvm_cbe_call64 = _ZN11DummyStreamlsIPFRSt8ios_baseS2_EEERS_T_(llvm_cbe_call62, _ZSt3decRSt8ios_base);
#line 326 "UecDirectTransfer.cpp"
  llvm_cbe_tmp65 = *(&llvm_cbe_l_payLoad_ptr);
#line 326 "UecDirectTransfer.cpp"
  llvm_cbe_tmp67 = *((&llvm_cbe_tmp65->field2));
#line 326 "UecDirectTransfer.cpp"
  llvm_cbe_call69 = _ZN11DummyStreamlsIjEERS_T_(llvm_cbe_call64, llvm_cbe_tmp67);
#line 326 "UecDirectTransfer.cpp"
  llvm_cbe_call71 = _ZN11DummyStreamlsIPKcEERS_T_(llvm_cbe_call69, ((&_OC_str25.array[((signed int )0u)])));
#line 326 "UecDirectTransfer.cpp"
  llvm_cbe_call73 = _ZN11DummyStreamlsIPFRSt8ios_baseS2_EEERS_T_(llvm_cbe_call71, _ZSt3decRSt8ios_base);
#line 326 "UecDirectTransfer.cpp"
  llvm_cbe_tmp74 = *(&llvm_cbe_l_uecUeContextDataWrite_ptr);
#line 326 "UecDirectTransfer.cpp"
  llvm_cbe_call76 = _ZNK3Uec16UecUeContextData11getS1LinkIdEv(llvm_cbe_tmp74);
#line 326 "UecDirectTransfer.cpp"
  llvm_cbe_call78 = _ZN11DummyStreamlsIjEERS_T_(llvm_cbe_call73, llvm_cbe_call76);
#line 326 "UecDirectTransfer.cpp"
  llvm_cbe_call80 = _ZN11DummyStreamlsIPKcEERS_T_(llvm_cbe_call78, ((&_OC_str26.array[((signed int )0u)])));
#line 326 "UecDirectTransfer.cpp"
  llvm_cbe_call82 = _ZN11DummyStreamlsIPFRSt8ios_baseS2_EEERS_T_(llvm_cbe_call80, _ZSt3decRSt8ios_base);
#line 326 "UecDirectTransfer.cpp"
  llvm_cbe_tmp83 = *(&llvm_cbe_l_payLoad_ptr);
#line 326 "UecDirectTransfer.cpp"
  llvm_cbe_tmp85 = *((&llvm_cbe_tmp83->field0));
#line 326 "UecDirectTransfer.cpp"
  llvm_cbe_call87 = _ZN11DummyStreamlsIjEERS_T_(llvm_cbe_call82, llvm_cbe_tmp85);
#line 326 "UecDirectTransfer.cpp"
  llvm_cbe_call89 = _ZN11DummyStreamlsIPKcEERS_T_(llvm_cbe_call87, ((&_OC_str27.array[((signed int )0u)])));
#line 332 "UecDirectTransfer.cpp"
  *(&llvm_cbe_retval) = 4294967295u;
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_cleanup_2e_dst) = 1u;
#line 332 "UecDirectTransfer.cpp"
  goto llvm_cbe_cleanup;

llvm_cbe_if_2e_end:
#line 335 "UecDirectTransfer.cpp"
  *(&llvm_cbe_retval) = 1u;
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_cleanup_2e_dst) = 2u;
#line 335 "UecDirectTransfer.cpp"
  goto llvm_cbe_cleanup;

llvm_cbe_cleanup_2e_pad:
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_cleanup_2e_dst92) = 1u;
#line 0 "LLVM INTERNAL"
  goto llvm_cbe_cleanup93;

llvm_cbe_cleanup_2e_pad90:
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_cleanup_2e_dst92) = 1u;
#line 0 "LLVM INTERNAL"
  goto llvm_cbe_cleanup93;

llvm_cbe_cleanup:
#line 336 "UecDirectTransfer.cpp"
  _ZN3Uec17UecDebugCallstackD1Ev((&llvm_cbe__callstackTmp_));
#line 336 "UecDirectTransfer.cpp"
  llvm_cbe_tmp91 = *(&llvm_cbe_cleanup_2e_dst);
#line 336 "UecDirectTransfer.cpp"
  switch (llvm_cbe_tmp91) {
  default:
    goto llvm_cbe_cleanup_2e_end;;
  case 1u:
    goto llvm_cbe_cleanup_2e_pad;  case 2u:
    goto llvm_cbe_cleanup_2e_pad90;  }

llvm_cbe_cleanup_2e_end:
#line 336 "UecDirectTransfer.cpp"
  *(&llvm_cbe_cleanup_2e_dst92) = 0u;
#line 336 "UecDirectTransfer.cpp"
  goto llvm_cbe_cleanup93;

llvm_cbe_cleanup93:
#line 336 "UecDirectTransfer.cpp"
  _ZNSsD1Ev((&llvm_cbe__currentFunction_));
#line 336 "UecDirectTransfer.cpp"
  llvm_cbe_tmp94 = *(&llvm_cbe_cleanup_2e_dst92);
#line 336 "UecDirectTransfer.cpp"
  llvm_cbe_tmp__55 = *(&llvm_cbe_retval);
#line 336 "UecDirectTransfer.cpp"
  return llvm_cbe_tmp__55;
}


#line 0 "LLVM INTERNAL"
unsigned char *_ZNK3Uec8UecEvent10getPayLoadEv(struct l_class_OC_Uec_KD__KD_UecEvent *llvm_cbe_this) {
  unsigned char *llvm_cbe_retval;    /* Address-exposed local */
  struct l_class_OC_Uec_KD__KD_UecEvent *llvm_cbe_this_2e_addr;    /* Address-exposed local */
  struct l_class_OC_Uec_KD__KD_UecEvent *llvm_cbe_this1;
  unsigned char *llvm_cbe_tmp2;
  unsigned char *llvm_cbe_tmp__56;

#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_this_2e_addr) = llvm_cbe_this;
#line 0 "LLVM INTERNAL"
  llvm_cbe_this1 = *(&llvm_cbe_this_2e_addr);
#line 271 "UecEvent.hpp"
  llvm_cbe_tmp2 = *((&llvm_cbe_this1->field4));
#line 271 "UecEvent.hpp"
  *(&llvm_cbe_retval) = llvm_cbe_tmp2;
#line 271 "UecEvent.hpp"
  llvm_cbe_tmp__56 = *(&llvm_cbe_retval);
#line 271 "UecEvent.hpp"
  return llvm_cbe_tmp__56;
}


#line 0 "LLVM INTERNAL"
unsigned int _ZNK3Uec16UecUeContextData11getS1LinkIdEv(struct l_class_OC_Uec_KD__KD_UecUeContextData *llvm_cbe_this) {
  unsigned int llvm_cbe_retval;    /* Address-exposed local */
  struct l_class_OC_Uec_KD__KD_UecUeContextData *llvm_cbe_this_2e_addr;    /* Address-exposed local */
  struct l_class_OC_Uec_KD__KD_UecUeContextData *llvm_cbe_this1;
  unsigned int llvm_cbe_tmp3;
  unsigned int llvm_cbe_tmp__57;

#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_this_2e_addr) = llvm_cbe_this;
#line 0 "LLVM INTERNAL"
  llvm_cbe_this1 = *(&llvm_cbe_this_2e_addr);
#line 1080 "UecUeContextData.hpp"
  llvm_cbe_tmp3 = *((&((&llvm_cbe_this1->field1))->field13));
#line 1080 "UecUeContextData.hpp"
  *(&llvm_cbe_retval) = llvm_cbe_tmp3;
#line 1081 "UecUeContextData.hpp"
  llvm_cbe_tmp__57 = *(&llvm_cbe_retval);
#line 1081 "UecUeContextData.hpp"
  return llvm_cbe_tmp__57;
}


#line 0 "LLVM INTERNAL"
unsigned int _ZN3Uec17UecDirectTransfer22handleTupSrbReceiveIndEPNS_8UecEventE(struct l_class_OC_Uec_KD__KD_UecDirectTransfer *llvm_cbe_this, struct l_class_OC_Uec_KD__KD_UecEvent *llvm_cbe_p_uecEvent_ptr) {
  unsigned int llvm_cbe_retval;    /* Address-exposed local */
  struct l_class_OC_Uec_KD__KD_UecDirectTransfer *llvm_cbe_this_2e_addr;    /* Address-exposed local */
  struct l_class_OC_Uec_KD__KD_UecEvent *llvm_cbe_p_uecEvent_ptr_2e_addr;    /* Address-exposed local */
  struct l_class_OC_std_KD__KD_basic_string llvm_cbe__currentFunction_;    /* Address-exposed local */
  struct l_class_OC_BaseEvent llvm_cbe_agg_2e_tmp;    /* Address-exposed local */
  struct l_class_OC_Uec_KD__KD_UecDebugCallstack llvm_cbe__callstackTmp_;    /* Address-exposed local */
  struct l_struct_OC_TUP_SrbReceiveInd *llvm_cbe_l_payLoad_ptr;    /* Address-exposed local */
  struct l_class_OC_Uec_KD__KD_UecUeContextData *llvm_cbe_l_uecUeContextDataWrite_ptr;    /* Address-exposed local */
  unsigned int llvm_cbe_cleanup_2e_dst;    /* Address-exposed local */
  unsigned int llvm_cbe_cleanup_2e_dst133;    /* Address-exposed local */
  struct l_class_OC_Uec_KD__KD_UecDirectTransfer *llvm_cbe_this1;
  struct l_class_OC_Uec_KD__KD_UecEvent *llvm_cbe_tmp;
  unsigned char *llvm_cbe_call;
  struct l_class_OC_Uec_KD__KD_UecBaseData *llvm_cbe_call16;
  struct l_class_OC_Uec_KD__KD_UecBaseData * (**llvm_cbe_tmp__58) (struct l_class_OC_Uec_KD__KD_UecBaseData *, unsigned int );
  struct l_class_OC_Uec_KD__KD_UecBaseData * (*llvm_cbe_tmp__59) (struct l_class_OC_Uec_KD__KD_UecBaseData *, unsigned int );
  struct l_class_OC_Uec_KD__KD_UecBaseData *llvm_cbe_call18;
  struct l_class_OC_BaseEvent *llvm_cbe_call20;
  struct l_class_OC_BaseEvent *llvm_cbe_call22;
  struct l_class_OC_Uec_KD__KD_UecEvent *llvm_cbe_tmp23;
  unsigned int llvm_cbe_call25;
  struct l_class_OC_BaseEvent *llvm_cbe_call27;
  struct l_class_OC_BaseEvent *llvm_cbe_call29;
  struct l_class_OC_BaseEvent *llvm_cbe_call31;
  struct l_class_OC_Uec_KD__KD_UecUeContextData *llvm_cbe_tmp32;
  unsigned int llvm_cbe_call34;
  struct l_class_OC_BaseEvent *llvm_cbe_call36;
  struct l_class_OC_BaseEvent *llvm_cbe_call38;
  struct l_class_OC_BaseEvent *llvm_cbe_call40;
  struct l_struct_OC_TUP_SrbReceiveInd *llvm_cbe_tmp41;
  unsigned int llvm_cbe_tmp43;
  struct l_class_OC_BaseEvent *llvm_cbe_call45;
  struct l_class_OC_BaseEvent *llvm_cbe_call47;
  struct l_class_OC_BaseEvent *llvm_cbe_call49;
  struct l_struct_OC_TUP_SrbReceiveInd *llvm_cbe_tmp50;
  unsigned int llvm_cbe_tmp52;
  struct l_class_OC_BaseEvent *llvm_cbe_call54;
  struct l_class_OC_Uec_KD__KD_UecUeContextData *llvm_cbe_tmp55;
  unsigned int llvm_cbe_call57;
  struct l_struct_OC_TUP_SrbReceiveInd *llvm_cbe_tmp58;
  unsigned int llvm_cbe_tmp60;
  struct l_class_OC_BaseEvent *llvm_cbe_call62;
  struct l_class_OC_BaseEvent *llvm_cbe_call64;
  struct l_class_OC_Uec_KD__KD_UecEvent *llvm_cbe_tmp65;
  unsigned int llvm_cbe_call67;
  struct l_class_OC_BaseEvent *llvm_cbe_call69;
  struct l_class_OC_BaseEvent *llvm_cbe_call71;
  struct l_class_OC_BaseEvent *llvm_cbe_call73;
  struct l_class_OC_Uec_KD__KD_UecUeContextData *llvm_cbe_tmp74;
  unsigned int llvm_cbe_call76;
  struct l_class_OC_BaseEvent *llvm_cbe_call78;
  struct l_class_OC_BaseEvent *llvm_cbe_call80;
  struct l_class_OC_BaseEvent *llvm_cbe_call82;
  struct l_class_OC_Uec_KD__KD_UecUeContextData *llvm_cbe_tmp83;
  unsigned int llvm_cbe_call85;
  struct l_class_OC_BaseEvent *llvm_cbe_call87;
  struct l_class_OC_BaseEvent *llvm_cbe_call89;
  struct l_class_OC_BaseEvent *llvm_cbe_call91;
  struct l_struct_OC_TUP_SrbReceiveInd *llvm_cbe_tmp92;
  unsigned int llvm_cbe_tmp94;
  struct l_class_OC_BaseEvent *llvm_cbe_call96;
  struct l_class_OC_BaseEvent *llvm_cbe_call98;
  struct l_struct_OC_TUP_SrbReceiveInd *llvm_cbe_tmp99;
  unsigned int llvm_cbe_tmp101;
  struct l_class_OC_BaseEvent *llvm_cbe_call104;
  struct l_class_OC_BaseEvent *llvm_cbe_call106;
  struct l_class_OC_Uec_KD__KD_UecEvent *llvm_cbe_tmp107;
  unsigned int llvm_cbe_call109;
  struct l_class_OC_BaseEvent *llvm_cbe_call111;
  struct l_class_OC_BaseEvent *llvm_cbe_call113;
  struct l_class_OC_BaseEvent *llvm_cbe_call115;
  struct l_struct_OC_TUP_SrbReceiveInd *llvm_cbe_tmp116;
  unsigned int llvm_cbe_tmp118;
  struct l_class_OC_BaseEvent *llvm_cbe_call120;
  struct l_class_OC_BaseEvent *llvm_cbe_call122;
  struct l_class_OC_BaseEvent *llvm_cbe_call124;
  struct l_struct_OC_TUP_SrbReceiveInd *llvm_cbe_tmp125;
  unsigned int llvm_cbe_tmp127;
  struct l_class_OC_BaseEvent *llvm_cbe_call129;
  unsigned int llvm_cbe_tmp132;
  unsigned int llvm_cbe_tmp135;
  unsigned int llvm_cbe_tmp__60;

#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_this_2e_addr) = llvm_cbe_this;
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_p_uecEvent_ptr_2e_addr) = llvm_cbe_p_uecEvent_ptr;
#line 0 "LLVM INTERNAL"
  llvm_cbe_this1 = *(&llvm_cbe_this_2e_addr);
#line 343 "UecDirectTransfer.cpp"
  _ZNSaIcEC1Ev((&llvm_cbe_agg_2e_tmp));
#line 343 "UecDirectTransfer.cpp"
  _ZNSsC1EPKcRKSaIcE((&llvm_cbe__currentFunction_), ((&__PRETTY_FUNCTION___OC__ZN3Uec17UecDirectTransfer22handleTupSrbReceiveIndEPNS_8UecEventE.array[((signed int )0u)])), (&llvm_cbe_agg_2e_tmp));
#line 343 "UecDirectTransfer.cpp"
  _ZNSaIcED1Ev((&llvm_cbe_agg_2e_tmp));
#line 343 "UecDirectTransfer.cpp"
  _ZN3Uec17UecDebugCallstackC1ERKSs((&llvm_cbe__callstackTmp_), (&llvm_cbe__currentFunction_));
#line 346 "UecDirectTransfer.cpp"
  llvm_cbe_tmp = *(&llvm_cbe_p_uecEvent_ptr_2e_addr);
#line 346 "UecDirectTransfer.cpp"
  llvm_cbe_call = _ZNK3Uec8UecEvent10getPayLoadEv(llvm_cbe_tmp);
#line 346 "UecDirectTransfer.cpp"
  *(&llvm_cbe_l_payLoad_ptr) = (((struct l_struct_OC_TUP_SrbReceiveInd *)llvm_cbe_call));
#line 349 "UecDirectTransfer.cpp"
  llvm_cbe_call16 = _ZNK3Uec14UecServiceBase16getUecDataAccessEv((((struct l_class_OC_Uec_KD__KD_UecServiceBase *)llvm_cbe_this1)));
#line 349 "UecDirectTransfer.cpp"
  llvm_cbe_tmp__58 = *(((struct l_class_OC_Uec_KD__KD_UecBaseData * (***) (struct l_class_OC_Uec_KD__KD_UecBaseData *, unsigned int ))llvm_cbe_call16));
#line 349 "UecDirectTransfer.cpp"
  llvm_cbe_tmp__59 = *((&llvm_cbe_tmp__58[((signed long long )2ull)]));
#line 349 "UecDirectTransfer.cpp"
  llvm_cbe_call18 = llvm_cbe_tmp__59(llvm_cbe_call16, 1u);
#line 349 "UecDirectTransfer.cpp"
  *(&llvm_cbe_l_uecUeContextDataWrite_ptr) = (((struct l_class_OC_Uec_KD__KD_UecUeContextData *)llvm_cbe_call18));
#line 351 "UecDirectTransfer.cpp"
  llvm_cbe_call20 = _ZN11DummyStreamlsIPKcEERS_T_((&mout), ((&_OC_str28.array[((signed int )0u)])));
#line 351 "UecDirectTransfer.cpp"
  llvm_cbe_call22 = _ZN11DummyStreamlsIPFRSt8ios_baseS2_EEERS_T_(llvm_cbe_call20, _ZSt3decRSt8ios_base);
#line 351 "UecDirectTransfer.cpp"
  llvm_cbe_tmp23 = *(&llvm_cbe_p_uecEvent_ptr_2e_addr);
#line 351 "UecDirectTransfer.cpp"
  llvm_cbe_call25 = _ZNK3Uec8UecEvent13getInstanceIdEv(llvm_cbe_tmp23);
#line 351 "UecDirectTransfer.cpp"
  llvm_cbe_call27 = _ZN11DummyStreamlsIjEERS_T_(llvm_cbe_call22, llvm_cbe_call25);
#line 351 "UecDirectTransfer.cpp"
  llvm_cbe_call29 = _ZN11DummyStreamlsIPKcEERS_T_(llvm_cbe_call27, ((&_OC_str29.array[((signed int )0u)])));
#line 351 "UecDirectTransfer.cpp"
  llvm_cbe_call31 = _ZN11DummyStreamlsIPFRSt8ios_baseS2_EEERS_T_(llvm_cbe_call29, _ZSt3decRSt8ios_base);
#line 351 "UecDirectTransfer.cpp"
  llvm_cbe_tmp32 = *(&llvm_cbe_l_uecUeContextDataWrite_ptr);
#line 351 "UecDirectTransfer.cpp"
  llvm_cbe_call34 = _ZNK3Uec16UecUeContextData9getCellIdEv(llvm_cbe_tmp32);
#line 351 "UecDirectTransfer.cpp"
  llvm_cbe_call36 = _ZN11DummyStreamlsIjEERS_T_(llvm_cbe_call31, llvm_cbe_call34);
#line 351 "UecDirectTransfer.cpp"
  llvm_cbe_call38 = _ZN11DummyStreamlsIPKcEERS_T_(llvm_cbe_call36, ((&_OC_str30.array[((signed int )0u)])));
#line 351 "UecDirectTransfer.cpp"
  llvm_cbe_call40 = _ZN11DummyStreamlsIPFRSt8ios_baseS2_EEERS_T_(llvm_cbe_call38, _ZSt3decRSt8ios_base);
#line 351 "UecDirectTransfer.cpp"
  llvm_cbe_tmp41 = *(&llvm_cbe_l_payLoad_ptr);
#line 351 "UecDirectTransfer.cpp"
  llvm_cbe_tmp43 = *((&llvm_cbe_tmp41->field0));
#line 351 "UecDirectTransfer.cpp"
  llvm_cbe_call45 = _ZN11DummyStreamlsIjEERS_T_(llvm_cbe_call40, llvm_cbe_tmp43);
#line 351 "UecDirectTransfer.cpp"
  llvm_cbe_call47 = _ZN11DummyStreamlsIPKcEERS_T_(llvm_cbe_call45, ((&_OC_str31.array[((signed int )0u)])));
#line 351 "UecDirectTransfer.cpp"
  llvm_cbe_call49 = _ZN11DummyStreamlsIPFRSt8ios_baseS2_EEERS_T_(llvm_cbe_call47, _ZSt3decRSt8ios_base);
#line 351 "UecDirectTransfer.cpp"
  llvm_cbe_tmp50 = *(&llvm_cbe_l_payLoad_ptr);
#line 351 "UecDirectTransfer.cpp"
  llvm_cbe_tmp52 = *((&llvm_cbe_tmp50->field1));
#line 351 "UecDirectTransfer.cpp"
  llvm_cbe_call54 = _ZN11DummyStreamlsIjEERS_T_(llvm_cbe_call49, llvm_cbe_tmp52);
#line 357 "UecDirectTransfer.cpp"
  llvm_cbe_tmp55 = *(&llvm_cbe_l_uecUeContextDataWrite_ptr);
#line 357 "UecDirectTransfer.cpp"
  llvm_cbe_call57 = _ZNK3Uec16UecUeContextData15getUplaneCellIdEv(llvm_cbe_tmp55);
#line 357 "UecDirectTransfer.cpp"
  llvm_cbe_tmp58 = *(&llvm_cbe_l_payLoad_ptr);
#line 357 "UecDirectTransfer.cpp"
  llvm_cbe_tmp60 = *((&llvm_cbe_tmp58->field0));
#line 357 "UecDirectTransfer.cpp"
  if ((llvm_cbe_call57 != llvm_cbe_tmp60)) {    goto llvm_cbe_if_2e_then;  } else {    goto llvm_cbe_if_2e_end;  }


llvm_cbe_if_2e_then:
#line 359 "UecDirectTransfer.cpp"
  llvm_cbe_call62 = _ZN11DummyStreamlsIPKcEERS_T_((&mout), ((&_OC_str28.array[((signed int )0u)])));
#line 359 "UecDirectTransfer.cpp"
  llvm_cbe_call64 = _ZN11DummyStreamlsIPFRSt8ios_baseS2_EEERS_T_(llvm_cbe_call62, _ZSt3decRSt8ios_base);
#line 359 "UecDirectTransfer.cpp"
  llvm_cbe_tmp65 = *(&llvm_cbe_p_uecEvent_ptr_2e_addr);
#line 359 "UecDirectTransfer.cpp"
  llvm_cbe_call67 = _ZNK3Uec8UecEvent13getInstanceIdEv(llvm_cbe_tmp65);
#line 359 "UecDirectTransfer.cpp"
  llvm_cbe_call69 = _ZN11DummyStreamlsIjEERS_T_(llvm_cbe_call64, llvm_cbe_call67);
#line 359 "UecDirectTransfer.cpp"
  llvm_cbe_call71 = _ZN11DummyStreamlsIPKcEERS_T_(llvm_cbe_call69, ((&_OC_str32.array[((signed int )0u)])));
#line 359 "UecDirectTransfer.cpp"
  llvm_cbe_call73 = _ZN11DummyStreamlsIPFRSt8ios_baseS2_EEERS_T_(llvm_cbe_call71, _ZSt3decRSt8ios_base);
#line 359 "UecDirectTransfer.cpp"
  llvm_cbe_tmp74 = *(&llvm_cbe_l_uecUeContextDataWrite_ptr);
#line 359 "UecDirectTransfer.cpp"
  llvm_cbe_call76 = _ZNK3Uec16UecUeContextData9getCellIdEv(llvm_cbe_tmp74);
#line 359 "UecDirectTransfer.cpp"
  llvm_cbe_call78 = _ZN11DummyStreamlsIjEERS_T_(llvm_cbe_call73, llvm_cbe_call76);
#line 359 "UecDirectTransfer.cpp"
  llvm_cbe_call80 = _ZN11DummyStreamlsIPKcEERS_T_(llvm_cbe_call78, ((&_OC_str33.array[((signed int )0u)])));
#line 359 "UecDirectTransfer.cpp"
  llvm_cbe_call82 = _ZN11DummyStreamlsIPFRSt8ios_baseS2_EEERS_T_(llvm_cbe_call80, _ZSt3decRSt8ios_base);
#line 359 "UecDirectTransfer.cpp"
  llvm_cbe_tmp83 = *(&llvm_cbe_l_uecUeContextDataWrite_ptr);
#line 359 "UecDirectTransfer.cpp"
  llvm_cbe_call85 = _ZNK3Uec16UecUeContextData15getUplaneCellIdEv(llvm_cbe_tmp83);
#line 359 "UecDirectTransfer.cpp"
  llvm_cbe_call87 = _ZN11DummyStreamlsIjEERS_T_(llvm_cbe_call82, llvm_cbe_call85);
#line 359 "UecDirectTransfer.cpp"
  llvm_cbe_call89 = _ZN11DummyStreamlsIPKcEERS_T_(llvm_cbe_call87, ((&_OC_str34.array[((signed int )0u)])));
#line 359 "UecDirectTransfer.cpp"
  llvm_cbe_call91 = _ZN11DummyStreamlsIPFRSt8ios_baseS2_EEERS_T_(llvm_cbe_call89, _ZSt3decRSt8ios_base);
#line 359 "UecDirectTransfer.cpp"
  llvm_cbe_tmp92 = *(&llvm_cbe_l_payLoad_ptr);
#line 359 "UecDirectTransfer.cpp"
  llvm_cbe_tmp94 = *((&llvm_cbe_tmp92->field0));
#line 359 "UecDirectTransfer.cpp"
  llvm_cbe_call96 = _ZN11DummyStreamlsIjEERS_T_(llvm_cbe_call91, llvm_cbe_tmp94);
#line 359 "UecDirectTransfer.cpp"
  llvm_cbe_call98 = _ZN11DummyStreamlsIPKcEERS_T_(llvm_cbe_call96, ((&_OC_str35.array[((signed int )0u)])));
#line 365 "UecDirectTransfer.cpp"
  *(&llvm_cbe_retval) = 4294967295u;
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_cleanup_2e_dst) = 1u;
#line 365 "UecDirectTransfer.cpp"
  goto llvm_cbe_cleanup;

llvm_cbe_if_2e_end:
#line 369 "UecDirectTransfer.cpp"
  llvm_cbe_tmp99 = *(&llvm_cbe_l_payLoad_ptr);
#line 369 "UecDirectTransfer.cpp"
  llvm_cbe_tmp101 = *((&llvm_cbe_tmp99->field4));
#line 369 "UecDirectTransfer.cpp"
  if ((llvm_cbe_tmp101 != 0u)) {    goto llvm_cbe_if_2e_then102;  } else {    goto llvm_cbe_if_2e_end130;  }


llvm_cbe_if_2e_then102:
#line 371 "UecDirectTransfer.cpp"
  llvm_cbe_call104 = _ZN11DummyStreamlsIPKcEERS_T_((&mout), ((&_OC_str28.array[((signed int )0u)])));
#line 371 "UecDirectTransfer.cpp"
  llvm_cbe_call106 = _ZN11DummyStreamlsIPFRSt8ios_baseS2_EEERS_T_(llvm_cbe_call104, _ZSt3decRSt8ios_base);
#line 371 "UecDirectTransfer.cpp"
  llvm_cbe_tmp107 = *(&llvm_cbe_p_uecEvent_ptr_2e_addr);
#line 371 "UecDirectTransfer.cpp"
  llvm_cbe_call109 = _ZNK3Uec8UecEvent13getInstanceIdEv(llvm_cbe_tmp107);
#line 371 "UecDirectTransfer.cpp"
  llvm_cbe_call111 = _ZN11DummyStreamlsIjEERS_T_(llvm_cbe_call106, llvm_cbe_call109);
#line 371 "UecDirectTransfer.cpp"
  llvm_cbe_call113 = _ZN11DummyStreamlsIPKcEERS_T_(llvm_cbe_call111, ((&_OC_str36.array[((signed int )0u)])));
#line 371 "UecDirectTransfer.cpp"
  llvm_cbe_call115 = _ZN11DummyStreamlsIPFRSt8ios_baseS2_EEERS_T_(llvm_cbe_call113, _ZSt3decRSt8ios_base);
#line 371 "UecDirectTransfer.cpp"
  llvm_cbe_tmp116 = *(&llvm_cbe_l_payLoad_ptr);
#line 371 "UecDirectTransfer.cpp"
  llvm_cbe_tmp118 = *((&llvm_cbe_tmp116->field4));
#line 371 "UecDirectTransfer.cpp"
  llvm_cbe_call120 = _ZN11DummyStreamlsIjEERS_T_(llvm_cbe_call115, llvm_cbe_tmp118);
#line 371 "UecDirectTransfer.cpp"
  llvm_cbe_call122 = _ZN11DummyStreamlsIPKcEERS_T_(llvm_cbe_call120, ((&_OC_str37.array[((signed int )0u)])));
#line 371 "UecDirectTransfer.cpp"
  llvm_cbe_call124 = _ZN11DummyStreamlsIPFRSt8ios_baseS2_EEERS_T_(llvm_cbe_call122, _ZSt3decRSt8ios_base);
#line 371 "UecDirectTransfer.cpp"
  llvm_cbe_tmp125 = *(&llvm_cbe_l_payLoad_ptr);
#line 371 "UecDirectTransfer.cpp"
  llvm_cbe_tmp127 = *((&llvm_cbe_tmp125->field5));
#line 371 "UecDirectTransfer.cpp"
  llvm_cbe_call129 = _ZN11DummyStreamlsI21EIntegrityCheckResultEERS_T_(llvm_cbe_call124, llvm_cbe_tmp127);
#line 0 "LLVM INTERNAL"
  goto llvm_cbe_if_2e_end130;

llvm_cbe_if_2e_end130:
#line 376 "UecDirectTransfer.cpp"
  *(&llvm_cbe_retval) = 1u;
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_cleanup_2e_dst) = 2u;
#line 376 "UecDirectTransfer.cpp"
  goto llvm_cbe_cleanup;

llvm_cbe_cleanup_2e_pad:
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_cleanup_2e_dst133) = 1u;
#line 0 "LLVM INTERNAL"
  goto llvm_cbe_cleanup134;

llvm_cbe_cleanup_2e_pad131:
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_cleanup_2e_dst133) = 1u;
#line 0 "LLVM INTERNAL"
  goto llvm_cbe_cleanup134;

llvm_cbe_cleanup:
#line 377 "UecDirectTransfer.cpp"
  _ZN3Uec17UecDebugCallstackD1Ev((&llvm_cbe__callstackTmp_));
#line 377 "UecDirectTransfer.cpp"
  llvm_cbe_tmp132 = *(&llvm_cbe_cleanup_2e_dst);
#line 377 "UecDirectTransfer.cpp"
  switch (llvm_cbe_tmp132) {
  default:
    goto llvm_cbe_cleanup_2e_end;;
  case 1u:
    goto llvm_cbe_cleanup_2e_pad;  case 2u:
    goto llvm_cbe_cleanup_2e_pad131;  }

llvm_cbe_cleanup_2e_end:
#line 377 "UecDirectTransfer.cpp"
  *(&llvm_cbe_cleanup_2e_dst133) = 0u;
#line 377 "UecDirectTransfer.cpp"
  goto llvm_cbe_cleanup134;

llvm_cbe_cleanup134:
#line 377 "UecDirectTransfer.cpp"
  _ZNSsD1Ev((&llvm_cbe__currentFunction_));
#line 377 "UecDirectTransfer.cpp"
  llvm_cbe_tmp135 = *(&llvm_cbe_cleanup_2e_dst133);
#line 377 "UecDirectTransfer.cpp"
  llvm_cbe_tmp__60 = *(&llvm_cbe_retval);
#line 377 "UecDirectTransfer.cpp"
  return llvm_cbe_tmp__60;
}


#line 0 "LLVM INTERNAL"
unsigned int _ZNK3Uec16UecUeContextData9getCellIdEv(struct l_class_OC_Uec_KD__KD_UecUeContextData *llvm_cbe_this) {
  unsigned int llvm_cbe_retval;    /* Address-exposed local */
  struct l_class_OC_Uec_KD__KD_UecUeContextData *llvm_cbe_this_2e_addr;    /* Address-exposed local */
  struct l_class_OC_Uec_KD__KD_UecUeContextData *llvm_cbe_this1;
  unsigned int llvm_cbe_tmp3;
  unsigned int llvm_cbe_tmp__61;

#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_this_2e_addr) = llvm_cbe_this;
#line 0 "LLVM INTERNAL"
  llvm_cbe_this1 = *(&llvm_cbe_this_2e_addr);
#line 890 "UecUeContextData.hpp"
  llvm_cbe_tmp3 = *((&((&llvm_cbe_this1->field1))->field2));
#line 890 "UecUeContextData.hpp"
  *(&llvm_cbe_retval) = llvm_cbe_tmp3;
#line 891 "UecUeContextData.hpp"
  llvm_cbe_tmp__61 = *(&llvm_cbe_retval);
#line 891 "UecUeContextData.hpp"
  return llvm_cbe_tmp__61;
}


#line 0 "LLVM INTERNAL"
unsigned int _ZNK3Uec16UecUeContextData15getUplaneCellIdEv(struct l_class_OC_Uec_KD__KD_UecUeContextData *llvm_cbe_this) {
  unsigned int llvm_cbe_retval;    /* Address-exposed local */
  struct l_class_OC_Uec_KD__KD_UecUeContextData *llvm_cbe_this_2e_addr;    /* Address-exposed local */
  struct l_class_OC_Uec_KD__KD_UecUeContextData *llvm_cbe_this1;
  unsigned int llvm_cbe_tmp3;
  unsigned int llvm_cbe_tmp__62;

#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_this_2e_addr) = llvm_cbe_this;
#line 0 "LLVM INTERNAL"
  llvm_cbe_this1 = *(&llvm_cbe_this_2e_addr);
#line 909 "UecUeContextData.hpp"
  llvm_cbe_tmp3 = *((&((&llvm_cbe_this1->field1))->field3));
#line 909 "UecUeContextData.hpp"
  *(&llvm_cbe_retval) = llvm_cbe_tmp3;
#line 910 "UecUeContextData.hpp"
  llvm_cbe_tmp__62 = *(&llvm_cbe_retval);
#line 910 "UecUeContextData.hpp"
  return llvm_cbe_tmp__62;
}


#line 0 "LLVM INTERNAL"
struct l_class_OC_BaseEvent *_ZN11DummyStreamlsI21EIntegrityCheckResultEERS_T_(struct l_class_OC_BaseEvent *llvm_cbe_this, unsigned int llvm_cbe_arg) {
  struct l_class_OC_BaseEvent *llvm_cbe_retval;    /* Address-exposed local */
  struct l_class_OC_BaseEvent *llvm_cbe_this_2e_addr;    /* Address-exposed local */
  unsigned int llvm_cbe_arg_2e_addr;    /* Address-exposed local */
  struct l_class_OC_BaseEvent *llvm_cbe_this1;
  struct l_class_OC_BaseEvent *llvm_cbe_tmp__63;

#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_this_2e_addr) = llvm_cbe_this;
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_arg_2e_addr) = llvm_cbe_arg;
#line 0 "LLVM INTERNAL"
  llvm_cbe_this1 = *(&llvm_cbe_this_2e_addr);
#line 30 "BaseTypes.hpp"
  *(&llvm_cbe_retval) = llvm_cbe_this1;
#line 31 "BaseTypes.hpp"
  llvm_cbe_tmp__63 = *(&llvm_cbe_retval);
#line 31 "BaseTypes.hpp"
  return llvm_cbe_tmp__63;
}


#line 0 "LLVM INTERNAL"
unsigned int _ZN3Uec17UecDirectTransfer20handleTupSrbSendRespEPNS_8UecEventE(struct l_class_OC_Uec_KD__KD_UecDirectTransfer *llvm_cbe_this, struct l_class_OC_Uec_KD__KD_UecEvent *llvm_cbe_p_uecEvent_ptr) {
  unsigned int llvm_cbe_retval;    /* Address-exposed local */
  struct l_class_OC_Uec_KD__KD_UecDirectTransfer *llvm_cbe_this_2e_addr;    /* Address-exposed local */
  struct l_class_OC_Uec_KD__KD_UecEvent *llvm_cbe_p_uecEvent_ptr_2e_addr;    /* Address-exposed local */
  struct l_class_OC_std_KD__KD_basic_string llvm_cbe__currentFunction_;    /* Address-exposed local */
  struct l_class_OC_BaseEvent llvm_cbe_agg_2e_tmp;    /* Address-exposed local */
  struct l_class_OC_Uec_KD__KD_UecDebugCallstack llvm_cbe__callstackTmp_;    /* Address-exposed local */
  struct l_struct_OC_TUP_SrbSendResp *llvm_cbe_l_payLoad_ptr;    /* Address-exposed local */
  struct l_class_OC_Uec_KD__KD_UecUeContextData *llvm_cbe_l_uecUeContextDataWrite_ptr;    /* Address-exposed local */
  unsigned int llvm_cbe_cleanup_2e_dst;    /* Address-exposed local */
  unsigned int llvm_cbe_cleanup_2e_dst169;    /* Address-exposed local */
  struct l_class_OC_Uec_KD__KD_UecDirectTransfer *llvm_cbe_this1;
  struct l_class_OC_Uec_KD__KD_UecEvent *llvm_cbe_tmp;
  unsigned char *llvm_cbe_call;
  struct l_class_OC_BaseEvent *llvm_cbe_call15;
  struct l_class_OC_BaseEvent *llvm_cbe_call17;
  struct l_class_OC_Uec_KD__KD_UecEvent *llvm_cbe_tmp18;
  unsigned int llvm_cbe_call20;
  struct l_class_OC_BaseEvent *llvm_cbe_call22;
  struct l_class_OC_BaseEvent *llvm_cbe_call24;
  struct l_class_OC_BaseEvent *llvm_cbe_call26;
  struct l_struct_OC_TUP_SrbSendResp *llvm_cbe_tmp27;
  unsigned int llvm_cbe_tmp30;
  struct l_class_OC_BaseEvent *llvm_cbe_call32;
  struct l_class_OC_BaseEvent *llvm_cbe_call34;
  struct l_class_OC_BaseEvent *llvm_cbe_call36;
  struct l_struct_OC_TUP_SrbSendResp *llvm_cbe_tmp37;
  unsigned int llvm_cbe_tmp40;
  struct l_class_OC_BaseEvent *llvm_cbe_call42;
  struct l_class_OC_BaseEvent *llvm_cbe_call44;
  struct l_class_OC_BaseEvent *llvm_cbe_call46;
  struct l_struct_OC_TUP_SrbSendResp *llvm_cbe_tmp47;
  unsigned int llvm_cbe_tmp50;
  struct l_class_OC_BaseEvent *llvm_cbe_call52;
  struct l_class_OC_BaseEvent *llvm_cbe_call54;
  struct l_class_OC_BaseEvent *llvm_cbe_call56;
  struct l_struct_OC_TUP_SrbSendResp *llvm_cbe_tmp57;
  unsigned int llvm_cbe_tmp59;
  struct l_class_OC_BaseEvent *llvm_cbe_call61;
  struct l_class_OC_Uec_KD__KD_UecBaseData *llvm_cbe_call64;
  struct l_class_OC_Uec_KD__KD_UecBaseData * (**llvm_cbe_tmp__64) (struct l_class_OC_Uec_KD__KD_UecBaseData *, unsigned int );
  struct l_class_OC_Uec_KD__KD_UecBaseData * (*llvm_cbe_tmp__65) (struct l_class_OC_Uec_KD__KD_UecBaseData *, unsigned int );
  struct l_class_OC_Uec_KD__KD_UecBaseData *llvm_cbe_call66;
  struct l_class_OC_Uec_KD__KD_UecUeContextData *llvm_cbe_tmp67;
  unsigned int llvm_cbe_call69;
  struct l_struct_OC_TUP_SrbSendResp *llvm_cbe_tmp70;
  unsigned int llvm_cbe_tmp72;
  struct l_class_OC_BaseEvent *llvm_cbe_call74;
  struct l_class_OC_BaseEvent *llvm_cbe_call76;
  struct l_class_OC_Uec_KD__KD_UecEvent *llvm_cbe_tmp77;
  unsigned int llvm_cbe_call79;
  struct l_class_OC_BaseEvent *llvm_cbe_call81;
  struct l_class_OC_BaseEvent *llvm_cbe_call83;
  struct l_class_OC_BaseEvent *llvm_cbe_call85;
  struct l_class_OC_Uec_KD__KD_UecUeContextData *llvm_cbe_tmp86;
  unsigned int llvm_cbe_call88;
  struct l_class_OC_BaseEvent *llvm_cbe_call90;
  struct l_class_OC_BaseEvent *llvm_cbe_call92;
  struct l_class_OC_BaseEvent *llvm_cbe_call94;
  struct l_class_OC_Uec_KD__KD_UecUeContextData *llvm_cbe_tmp95;
  unsigned int llvm_cbe_call97;
  struct l_class_OC_BaseEvent *llvm_cbe_call99;
  struct l_class_OC_BaseEvent *llvm_cbe_call101;
  struct l_class_OC_BaseEvent *llvm_cbe_call103;
  struct l_struct_OC_TUP_SrbSendResp *llvm_cbe_tmp104;
  unsigned int llvm_cbe_tmp106;
  struct l_class_OC_BaseEvent *llvm_cbe_call108;
  struct l_class_OC_BaseEvent *llvm_cbe_call110;
  struct l_struct_OC_TUP_SrbSendResp *llvm_cbe_tmp111;
  unsigned int llvm_cbe_tmp114;
  struct l_class_OC_BaseEvent *llvm_cbe_call118;
  struct l_class_OC_BaseEvent *llvm_cbe_call120;
  struct l_class_OC_Uec_KD__KD_UecEvent *llvm_cbe_tmp121;
  unsigned int llvm_cbe_call123;
  struct l_class_OC_BaseEvent *llvm_cbe_call125;
  struct l_class_OC_BaseEvent *llvm_cbe_call127;
  struct l_class_OC_BaseEvent *llvm_cbe_call129;
  struct l_struct_OC_TUP_SrbSendResp *llvm_cbe_tmp130;
  unsigned int llvm_cbe_tmp133;
  struct l_class_OC_BaseEvent *llvm_cbe_call135;
  struct l_class_OC_BaseEvent *llvm_cbe_call137;
  struct l_class_OC_BaseEvent *llvm_cbe_call139;
  struct l_struct_OC_TUP_SrbSendResp *llvm_cbe_tmp140;
  unsigned int llvm_cbe_tmp143;
  struct l_class_OC_BaseEvent *llvm_cbe_call145;
  struct l_class_OC_BaseEvent *llvm_cbe_call147;
  struct l_class_OC_BaseEvent *llvm_cbe_call149;
  struct l_struct_OC_TUP_SrbSendResp *llvm_cbe_tmp150;
  unsigned int llvm_cbe_tmp153;
  struct l_class_OC_BaseEvent *llvm_cbe_call155;
  struct l_class_OC_BaseEvent *llvm_cbe_call157;
  struct l_class_OC_BaseEvent *llvm_cbe_call159;
  struct l_struct_OC_TUP_SrbSendResp *llvm_cbe_tmp160;
  unsigned int llvm_cbe_tmp162;
  struct l_class_OC_BaseEvent *llvm_cbe_call164;
  unsigned int llvm_cbe_tmp168;
  unsigned int llvm_cbe_tmp171;
  unsigned int llvm_cbe_tmp__66;

#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_this_2e_addr) = llvm_cbe_this;
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_p_uecEvent_ptr_2e_addr) = llvm_cbe_p_uecEvent_ptr;
#line 0 "LLVM INTERNAL"
  llvm_cbe_this1 = *(&llvm_cbe_this_2e_addr);
#line 385 "UecDirectTransfer.cpp"
  _ZNSaIcEC1Ev((&llvm_cbe_agg_2e_tmp));
#line 385 "UecDirectTransfer.cpp"
  _ZNSsC1EPKcRKSaIcE((&llvm_cbe__currentFunction_), ((&__PRETTY_FUNCTION___OC__ZN3Uec17UecDirectTransfer20handleTupSrbSendRespEPNS_8UecEventE.array[((signed int )0u)])), (&llvm_cbe_agg_2e_tmp));
#line 385 "UecDirectTransfer.cpp"
  _ZNSaIcED1Ev((&llvm_cbe_agg_2e_tmp));
#line 385 "UecDirectTransfer.cpp"
  _ZN3Uec17UecDebugCallstackC1ERKSs((&llvm_cbe__callstackTmp_), (&llvm_cbe__currentFunction_));
#line 387 "UecDirectTransfer.cpp"
  llvm_cbe_tmp = *(&llvm_cbe_p_uecEvent_ptr_2e_addr);
#line 387 "UecDirectTransfer.cpp"
  llvm_cbe_call = _ZNK3Uec8UecEvent10getPayLoadEv(llvm_cbe_tmp);
#line 387 "UecDirectTransfer.cpp"
  *(&llvm_cbe_l_payLoad_ptr) = (((struct l_struct_OC_TUP_SrbSendResp *)llvm_cbe_call));
#line 389 "UecDirectTransfer.cpp"
  llvm_cbe_call15 = _ZN11DummyStreamlsIPKcEERS_T_((&mout), ((&_OC_str38.array[((signed int )0u)])));
#line 389 "UecDirectTransfer.cpp"
  llvm_cbe_call17 = _ZN11DummyStreamlsIPFRSt8ios_baseS2_EEERS_T_(llvm_cbe_call15, _ZSt3decRSt8ios_base);
#line 389 "UecDirectTransfer.cpp"
  llvm_cbe_tmp18 = *(&llvm_cbe_p_uecEvent_ptr_2e_addr);
#line 389 "UecDirectTransfer.cpp"
  llvm_cbe_call20 = _ZNK3Uec8UecEvent13getInstanceIdEv(llvm_cbe_tmp18);
#line 389 "UecDirectTransfer.cpp"
  llvm_cbe_call22 = _ZN11DummyStreamlsIjEERS_T_(llvm_cbe_call17, llvm_cbe_call20);
#line 389 "UecDirectTransfer.cpp"
  llvm_cbe_call24 = _ZN11DummyStreamlsIPKcEERS_T_(llvm_cbe_call22, ((&_OC_str39.array[((signed int )0u)])));
#line 389 "UecDirectTransfer.cpp"
  llvm_cbe_call26 = _ZN11DummyStreamlsIPFRSt8ios_baseS2_EEERS_T_(llvm_cbe_call24, _ZSt3decRSt8ios_base);
#line 389 "UecDirectTransfer.cpp"
  llvm_cbe_tmp27 = *(&llvm_cbe_l_payLoad_ptr);
#line 389 "UecDirectTransfer.cpp"
  llvm_cbe_tmp30 = *((&((&llvm_cbe_tmp27->field0))->field0));
#line 389 "UecDirectTransfer.cpp"
  llvm_cbe_call32 = _ZN11DummyStreamlsI10EStatusLteEERS_T_(llvm_cbe_call26, llvm_cbe_tmp30);
#line 389 "UecDirectTransfer.cpp"
  llvm_cbe_call34 = _ZN11DummyStreamlsIPKcEERS_T_(llvm_cbe_call32, ((&_OC_str40.array[((signed int )0u)])));
#line 389 "UecDirectTransfer.cpp"
  llvm_cbe_call36 = _ZN11DummyStreamlsIPFRSt8ios_baseS2_EEERS_T_(llvm_cbe_call34, _ZSt3decRSt8ios_base);
#line 389 "UecDirectTransfer.cpp"
  llvm_cbe_tmp37 = *(&llvm_cbe_l_payLoad_ptr);
#line 389 "UecDirectTransfer.cpp"
  llvm_cbe_tmp40 = *((&((&llvm_cbe_tmp37->field0))->field1));
#line 389 "UecDirectTransfer.cpp"
  llvm_cbe_call42 = _ZN11DummyStreamlsI9ECauseLteEERS_T_(llvm_cbe_call36, llvm_cbe_tmp40);
#line 389 "UecDirectTransfer.cpp"
  llvm_cbe_call44 = _ZN11DummyStreamlsIPKcEERS_T_(llvm_cbe_call42, ((&_OC_str41.array[((signed int )0u)])));
#line 389 "UecDirectTransfer.cpp"
  llvm_cbe_call46 = _ZN11DummyStreamlsIPFRSt8ios_baseS2_EEERS_T_(llvm_cbe_call44, _ZSt3decRSt8ios_base);
#line 389 "UecDirectTransfer.cpp"
  llvm_cbe_tmp47 = *(&llvm_cbe_l_payLoad_ptr);
#line 389 "UecDirectTransfer.cpp"
  llvm_cbe_tmp50 = *((&((&llvm_cbe_tmp47->field0))->field2));
#line 389 "UecDirectTransfer.cpp"
  llvm_cbe_call52 = _ZN11DummyStreamlsI17ESpecificCauseLteEERS_T_(llvm_cbe_call46, llvm_cbe_tmp50);
#line 389 "UecDirectTransfer.cpp"
  llvm_cbe_call54 = _ZN11DummyStreamlsIPKcEERS_T_(llvm_cbe_call52, ((&_OC_str42.array[((signed int )0u)])));
#line 389 "UecDirectTransfer.cpp"
  llvm_cbe_call56 = _ZN11DummyStreamlsIPFRSt8ios_baseS2_EEERS_T_(llvm_cbe_call54, _ZSt3decRSt8ios_base);
#line 389 "UecDirectTransfer.cpp"
  llvm_cbe_tmp57 = *(&llvm_cbe_l_payLoad_ptr);
#line 389 "UecDirectTransfer.cpp"
  llvm_cbe_tmp59 = *((&llvm_cbe_tmp57->field7));
#line 389 "UecDirectTransfer.cpp"
  llvm_cbe_call61 = _ZN11DummyStreamlsIjEERS_T_(llvm_cbe_call56, llvm_cbe_tmp59);
#line 396 "UecDirectTransfer.cpp"
  llvm_cbe_call64 = _ZNK3Uec14UecServiceBase16getUecDataAccessEv((((struct l_class_OC_Uec_KD__KD_UecServiceBase *)llvm_cbe_this1)));
#line 396 "UecDirectTransfer.cpp"
  llvm_cbe_tmp__64 = *(((struct l_class_OC_Uec_KD__KD_UecBaseData * (***) (struct l_class_OC_Uec_KD__KD_UecBaseData *, unsigned int ))llvm_cbe_call64));
#line 396 "UecDirectTransfer.cpp"
  llvm_cbe_tmp__65 = *((&llvm_cbe_tmp__64[((signed long long )2ull)]));
#line 396 "UecDirectTransfer.cpp"
  llvm_cbe_call66 = llvm_cbe_tmp__65(llvm_cbe_call64, 1u);
#line 396 "UecDirectTransfer.cpp"
  *(&llvm_cbe_l_uecUeContextDataWrite_ptr) = (((struct l_class_OC_Uec_KD__KD_UecUeContextData *)llvm_cbe_call66));
#line 399 "UecDirectTransfer.cpp"
  llvm_cbe_tmp67 = *(&llvm_cbe_l_uecUeContextDataWrite_ptr);
#line 399 "UecDirectTransfer.cpp"
  llvm_cbe_call69 = _ZNK3Uec16UecUeContextData15getUplaneCellIdEv(llvm_cbe_tmp67);
#line 399 "UecDirectTransfer.cpp"
  llvm_cbe_tmp70 = *(&llvm_cbe_l_payLoad_ptr);
#line 399 "UecDirectTransfer.cpp"
  llvm_cbe_tmp72 = *((&llvm_cbe_tmp70->field1));
#line 399 "UecDirectTransfer.cpp"
  if ((llvm_cbe_call69 != llvm_cbe_tmp72)) {    goto llvm_cbe_if_2e_then;  } else {    goto llvm_cbe_if_2e_end;  }


llvm_cbe_if_2e_then:
#line 402 "UecDirectTransfer.cpp"
  llvm_cbe_call74 = _ZN11DummyStreamlsIPKcEERS_T_((&mout), ((&_OC_str38.array[((signed int )0u)])));
#line 402 "UecDirectTransfer.cpp"
  llvm_cbe_call76 = _ZN11DummyStreamlsIPFRSt8ios_baseS2_EEERS_T_(llvm_cbe_call74, _ZSt3decRSt8ios_base);
#line 402 "UecDirectTransfer.cpp"
  llvm_cbe_tmp77 = *(&llvm_cbe_p_uecEvent_ptr_2e_addr);
#line 402 "UecDirectTransfer.cpp"
  llvm_cbe_call79 = _ZNK3Uec8UecEvent13getInstanceIdEv(llvm_cbe_tmp77);
#line 402 "UecDirectTransfer.cpp"
  llvm_cbe_call81 = _ZN11DummyStreamlsIjEERS_T_(llvm_cbe_call76, llvm_cbe_call79);
#line 402 "UecDirectTransfer.cpp"
  llvm_cbe_call83 = _ZN11DummyStreamlsIPKcEERS_T_(llvm_cbe_call81, ((&_OC_str43.array[((signed int )0u)])));
#line 402 "UecDirectTransfer.cpp"
  llvm_cbe_call85 = _ZN11DummyStreamlsIPFRSt8ios_baseS2_EEERS_T_(llvm_cbe_call83, _ZSt3decRSt8ios_base);
#line 402 "UecDirectTransfer.cpp"
  llvm_cbe_tmp86 = *(&llvm_cbe_l_uecUeContextDataWrite_ptr);
#line 402 "UecDirectTransfer.cpp"
  llvm_cbe_call88 = _ZNK3Uec16UecUeContextData9getCellIdEv(llvm_cbe_tmp86);
#line 402 "UecDirectTransfer.cpp"
  llvm_cbe_call90 = _ZN11DummyStreamlsIjEERS_T_(llvm_cbe_call85, llvm_cbe_call88);
#line 402 "UecDirectTransfer.cpp"
  llvm_cbe_call92 = _ZN11DummyStreamlsIPKcEERS_T_(llvm_cbe_call90, ((&_OC_str33.array[((signed int )0u)])));
#line 402 "UecDirectTransfer.cpp"
  llvm_cbe_call94 = _ZN11DummyStreamlsIPFRSt8ios_baseS2_EEERS_T_(llvm_cbe_call92, _ZSt3decRSt8ios_base);
#line 402 "UecDirectTransfer.cpp"
  llvm_cbe_tmp95 = *(&llvm_cbe_l_uecUeContextDataWrite_ptr);
#line 402 "UecDirectTransfer.cpp"
  llvm_cbe_call97 = _ZNK3Uec16UecUeContextData15getUplaneCellIdEv(llvm_cbe_tmp95);
#line 402 "UecDirectTransfer.cpp"
  llvm_cbe_call99 = _ZN11DummyStreamlsIjEERS_T_(llvm_cbe_call94, llvm_cbe_call97);
#line 402 "UecDirectTransfer.cpp"
  llvm_cbe_call101 = _ZN11DummyStreamlsIPKcEERS_T_(llvm_cbe_call99, ((&_OC_str34.array[((signed int )0u)])));
#line 402 "UecDirectTransfer.cpp"
  llvm_cbe_call103 = _ZN11DummyStreamlsIPFRSt8ios_baseS2_EEERS_T_(llvm_cbe_call101, _ZSt3decRSt8ios_base);
#line 402 "UecDirectTransfer.cpp"
  llvm_cbe_tmp104 = *(&llvm_cbe_l_payLoad_ptr);
#line 402 "UecDirectTransfer.cpp"
  llvm_cbe_tmp106 = *((&llvm_cbe_tmp104->field1));
#line 402 "UecDirectTransfer.cpp"
  llvm_cbe_call108 = _ZN11DummyStreamlsIjEERS_T_(llvm_cbe_call103, llvm_cbe_tmp106);
#line 402 "UecDirectTransfer.cpp"
  llvm_cbe_call110 = _ZN11DummyStreamlsIPKcEERS_T_(llvm_cbe_call108, ((&_OC_str35.array[((signed int )0u)])));
#line 408 "UecDirectTransfer.cpp"
  *(&llvm_cbe_retval) = 4294967295u;
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_cleanup_2e_dst) = 1u;
#line 408 "UecDirectTransfer.cpp"
  goto llvm_cbe_cleanup;

llvm_cbe_if_2e_end:
#line 411 "UecDirectTransfer.cpp"
  llvm_cbe_tmp111 = *(&llvm_cbe_l_payLoad_ptr);
#line 411 "UecDirectTransfer.cpp"
  llvm_cbe_tmp114 = *((&((&llvm_cbe_tmp111->field0))->field0));
#line 411 "UecDirectTransfer.cpp"
  if ((0u == llvm_cbe_tmp114)) {    goto llvm_cbe_if_2e_then116;  } else {    goto llvm_cbe_if_2e_end165;  }


llvm_cbe_if_2e_then116:
#line 414 "UecDirectTransfer.cpp"
  llvm_cbe_call118 = _ZN11DummyStreamlsIPKcEERS_T_((&mout), ((&_OC_str38.array[((signed int )0u)])));
#line 414 "UecDirectTransfer.cpp"
  llvm_cbe_call120 = _ZN11DummyStreamlsIPFRSt8ios_baseS2_EEERS_T_(llvm_cbe_call118, _ZSt3decRSt8ios_base);
#line 414 "UecDirectTransfer.cpp"
  llvm_cbe_tmp121 = *(&llvm_cbe_p_uecEvent_ptr_2e_addr);
#line 414 "UecDirectTransfer.cpp"
  llvm_cbe_call123 = _ZNK3Uec8UecEvent13getInstanceIdEv(llvm_cbe_tmp121);
#line 414 "UecDirectTransfer.cpp"
  llvm_cbe_call125 = _ZN11DummyStreamlsIjEERS_T_(llvm_cbe_call120, llvm_cbe_call123);
#line 414 "UecDirectTransfer.cpp"
  llvm_cbe_call127 = _ZN11DummyStreamlsIPKcEERS_T_(llvm_cbe_call125, ((&_OC_str44.array[((signed int )0u)])));
#line 414 "UecDirectTransfer.cpp"
  llvm_cbe_call129 = _ZN11DummyStreamlsIPFRSt8ios_baseS2_EEERS_T_(llvm_cbe_call127, _ZSt3decRSt8ios_base);
#line 414 "UecDirectTransfer.cpp"
  llvm_cbe_tmp130 = *(&llvm_cbe_l_payLoad_ptr);
#line 414 "UecDirectTransfer.cpp"
  llvm_cbe_tmp133 = *((&((&llvm_cbe_tmp130->field0))->field0));
#line 414 "UecDirectTransfer.cpp"
  llvm_cbe_call135 = _ZN11DummyStreamlsI10EStatusLteEERS_T_(llvm_cbe_call129, llvm_cbe_tmp133);
#line 414 "UecDirectTransfer.cpp"
  llvm_cbe_call137 = _ZN11DummyStreamlsIPKcEERS_T_(llvm_cbe_call135, ((&_OC_str40.array[((signed int )0u)])));
#line 414 "UecDirectTransfer.cpp"
  llvm_cbe_call139 = _ZN11DummyStreamlsIPFRSt8ios_baseS2_EEERS_T_(llvm_cbe_call137, _ZSt3decRSt8ios_base);
#line 414 "UecDirectTransfer.cpp"
  llvm_cbe_tmp140 = *(&llvm_cbe_l_payLoad_ptr);
#line 414 "UecDirectTransfer.cpp"
  llvm_cbe_tmp143 = *((&((&llvm_cbe_tmp140->field0))->field1));
#line 414 "UecDirectTransfer.cpp"
  llvm_cbe_call145 = _ZN11DummyStreamlsI9ECauseLteEERS_T_(llvm_cbe_call139, llvm_cbe_tmp143);
#line 414 "UecDirectTransfer.cpp"
  llvm_cbe_call147 = _ZN11DummyStreamlsIPKcEERS_T_(llvm_cbe_call145, ((&_OC_str41.array[((signed int )0u)])));
#line 414 "UecDirectTransfer.cpp"
  llvm_cbe_call149 = _ZN11DummyStreamlsIPFRSt8ios_baseS2_EEERS_T_(llvm_cbe_call147, _ZSt3decRSt8ios_base);
#line 414 "UecDirectTransfer.cpp"
  llvm_cbe_tmp150 = *(&llvm_cbe_l_payLoad_ptr);
#line 414 "UecDirectTransfer.cpp"
  llvm_cbe_tmp153 = *((&((&llvm_cbe_tmp150->field0))->field2));
#line 414 "UecDirectTransfer.cpp"
  llvm_cbe_call155 = _ZN11DummyStreamlsI17ESpecificCauseLteEERS_T_(llvm_cbe_call149, llvm_cbe_tmp153);
#line 414 "UecDirectTransfer.cpp"
  llvm_cbe_call157 = _ZN11DummyStreamlsIPKcEERS_T_(llvm_cbe_call155, ((&_OC_str42.array[((signed int )0u)])));
#line 414 "UecDirectTransfer.cpp"
  llvm_cbe_call159 = _ZN11DummyStreamlsIPFRSt8ios_baseS2_EEERS_T_(llvm_cbe_call157, _ZSt3decRSt8ios_base);
#line 414 "UecDirectTransfer.cpp"
  llvm_cbe_tmp160 = *(&llvm_cbe_l_payLoad_ptr);
#line 414 "UecDirectTransfer.cpp"
  llvm_cbe_tmp162 = *((&llvm_cbe_tmp160->field7));
#line 414 "UecDirectTransfer.cpp"
  llvm_cbe_call164 = _ZN11DummyStreamlsIjEERS_T_(llvm_cbe_call159, llvm_cbe_tmp162);
#line 421 "UecDirectTransfer.cpp"
  *(&llvm_cbe_retval) = 0u;
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_cleanup_2e_dst) = 2u;
#line 421 "UecDirectTransfer.cpp"
  goto llvm_cbe_cleanup;

llvm_cbe_if_2e_end165:
#line 424 "UecDirectTransfer.cpp"
  *(&llvm_cbe_retval) = 1u;
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_cleanup_2e_dst) = 3u;
#line 424 "UecDirectTransfer.cpp"
  goto llvm_cbe_cleanup;

llvm_cbe_cleanup_2e_pad:
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_cleanup_2e_dst169) = 1u;
#line 0 "LLVM INTERNAL"
  goto llvm_cbe_cleanup170;

llvm_cbe_cleanup_2e_pad166:
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_cleanup_2e_dst169) = 1u;
#line 0 "LLVM INTERNAL"
  goto llvm_cbe_cleanup170;

llvm_cbe_cleanup_2e_pad167:
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_cleanup_2e_dst169) = 1u;
#line 0 "LLVM INTERNAL"
  goto llvm_cbe_cleanup170;

llvm_cbe_cleanup:
#line 425 "UecDirectTransfer.cpp"
  _ZN3Uec17UecDebugCallstackD1Ev((&llvm_cbe__callstackTmp_));
#line 425 "UecDirectTransfer.cpp"
  llvm_cbe_tmp168 = *(&llvm_cbe_cleanup_2e_dst);
#line 425 "UecDirectTransfer.cpp"
  switch (llvm_cbe_tmp168) {
  default:
    goto llvm_cbe_cleanup_2e_end;;
  case 1u:
    goto llvm_cbe_cleanup_2e_pad;  case 2u:
    goto llvm_cbe_cleanup_2e_pad166;  case 3u:
    goto llvm_cbe_cleanup_2e_pad167;  }

llvm_cbe_cleanup_2e_end:
#line 425 "UecDirectTransfer.cpp"
  *(&llvm_cbe_cleanup_2e_dst169) = 0u;
#line 425 "UecDirectTransfer.cpp"
  goto llvm_cbe_cleanup170;

llvm_cbe_cleanup170:
#line 425 "UecDirectTransfer.cpp"
  _ZNSsD1Ev((&llvm_cbe__currentFunction_));
#line 425 "UecDirectTransfer.cpp"
  llvm_cbe_tmp171 = *(&llvm_cbe_cleanup_2e_dst169);
#line 425 "UecDirectTransfer.cpp"
  llvm_cbe_tmp__66 = *(&llvm_cbe_retval);
#line 425 "UecDirectTransfer.cpp"
  return llvm_cbe_tmp__66;
}


#line 0 "LLVM INTERNAL"
struct l_class_OC_BaseEvent *_ZN11DummyStreamlsI10EStatusLteEERS_T_(struct l_class_OC_BaseEvent *llvm_cbe_this, unsigned int llvm_cbe_arg) {
  struct l_class_OC_BaseEvent *llvm_cbe_retval;    /* Address-exposed local */
  struct l_class_OC_BaseEvent *llvm_cbe_this_2e_addr;    /* Address-exposed local */
  unsigned int llvm_cbe_arg_2e_addr;    /* Address-exposed local */
  struct l_class_OC_BaseEvent *llvm_cbe_this1;
  struct l_class_OC_BaseEvent *llvm_cbe_tmp__67;

#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_this_2e_addr) = llvm_cbe_this;
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_arg_2e_addr) = llvm_cbe_arg;
#line 0 "LLVM INTERNAL"
  llvm_cbe_this1 = *(&llvm_cbe_this_2e_addr);
#line 30 "BaseTypes.hpp"
  *(&llvm_cbe_retval) = llvm_cbe_this1;
#line 31 "BaseTypes.hpp"
  llvm_cbe_tmp__67 = *(&llvm_cbe_retval);
#line 31 "BaseTypes.hpp"
  return llvm_cbe_tmp__67;
}


#line 0 "LLVM INTERNAL"
struct l_class_OC_BaseEvent *_ZN11DummyStreamlsI9ECauseLteEERS_T_(struct l_class_OC_BaseEvent *llvm_cbe_this, unsigned int llvm_cbe_arg) {
  struct l_class_OC_BaseEvent *llvm_cbe_retval;    /* Address-exposed local */
  struct l_class_OC_BaseEvent *llvm_cbe_this_2e_addr;    /* Address-exposed local */
  unsigned int llvm_cbe_arg_2e_addr;    /* Address-exposed local */
  struct l_class_OC_BaseEvent *llvm_cbe_this1;
  struct l_class_OC_BaseEvent *llvm_cbe_tmp__68;

#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_this_2e_addr) = llvm_cbe_this;
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_arg_2e_addr) = llvm_cbe_arg;
#line 0 "LLVM INTERNAL"
  llvm_cbe_this1 = *(&llvm_cbe_this_2e_addr);
#line 30 "BaseTypes.hpp"
  *(&llvm_cbe_retval) = llvm_cbe_this1;
#line 31 "BaseTypes.hpp"
  llvm_cbe_tmp__68 = *(&llvm_cbe_retval);
#line 31 "BaseTypes.hpp"
  return llvm_cbe_tmp__68;
}


#line 0 "LLVM INTERNAL"
struct l_class_OC_BaseEvent *_ZN11DummyStreamlsI17ESpecificCauseLteEERS_T_(struct l_class_OC_BaseEvent *llvm_cbe_this, unsigned int llvm_cbe_arg) {
  struct l_class_OC_BaseEvent *llvm_cbe_retval;    /* Address-exposed local */
  struct l_class_OC_BaseEvent *llvm_cbe_this_2e_addr;    /* Address-exposed local */
  unsigned int llvm_cbe_arg_2e_addr;    /* Address-exposed local */
  struct l_class_OC_BaseEvent *llvm_cbe_this1;
  struct l_class_OC_BaseEvent *llvm_cbe_tmp__69;

#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_this_2e_addr) = llvm_cbe_this;
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_arg_2e_addr) = llvm_cbe_arg;
#line 0 "LLVM INTERNAL"
  llvm_cbe_this1 = *(&llvm_cbe_this_2e_addr);
#line 30 "BaseTypes.hpp"
  *(&llvm_cbe_retval) = llvm_cbe_this1;
#line 31 "BaseTypes.hpp"
  llvm_cbe_tmp__69 = *(&llvm_cbe_retval);
#line 31 "BaseTypes.hpp"
  return llvm_cbe_tmp__69;
}


#line 0 "LLVM INTERNAL"
unsigned int _ZNK3Uec17UecDirectTransfer26handleUecDtfDirectTransferEPNS_8UecEventE(struct l_class_OC_Uec_KD__KD_UecDirectTransfer *llvm_cbe_this, struct l_class_OC_Uec_KD__KD_UecEvent *llvm_cbe_p_uecEvent_ptr) {
  unsigned int llvm_cbe_retval;    /* Address-exposed local */
  struct l_class_OC_Uec_KD__KD_UecDirectTransfer *llvm_cbe_this_2e_addr;    /* Address-exposed local */
  struct l_class_OC_Uec_KD__KD_UecEvent *llvm_cbe_p_uecEvent_ptr_2e_addr;    /* Address-exposed local */
  struct l_class_OC_std_KD__KD_basic_string llvm_cbe__currentFunction_;    /* Address-exposed local */
  struct l_class_OC_BaseEvent llvm_cbe_agg_2e_tmp;    /* Address-exposed local */
  struct l_class_OC_Uec_KD__KD_UecDebugCallstack llvm_cbe__callstackTmp_;    /* Address-exposed local */
  struct l_struct_OC_Uec_KD__KD_UEC_DTF_DirectTransfer *llvm_cbe_l_payLoad_ptr;    /* Address-exposed local */
  unsigned int llvm_cbe_cleanup_2e_dst;    /* Address-exposed local */
  unsigned int llvm_cbe_cleanup_2e_dst54;    /* Address-exposed local */
  struct l_class_OC_Uec_KD__KD_UecDirectTransfer *llvm_cbe_this1;
  struct l_class_OC_Uec_KD__KD_UecEvent *llvm_cbe_tmp;
  unsigned char *llvm_cbe_call;
  struct l_struct_OC_Uec_KD__KD_UEC_DTF_DirectTransfer *llvm_cbe_tmp14;
  unsigned char *llvm_cbe_tmp17;
  struct l_struct_OC_Uec_KD__KD_UEC_DTF_DirectTransfer *llvm_cbe_tmp18;
  unsigned int llvm_cbe_tmp21;
  struct l_class_OC_BaseEvent *llvm_cbe_call24;
  struct l_class_OC_BaseEvent *llvm_cbe_call26;
  struct l_class_OC_Uec_KD__KD_UecEvent *llvm_cbe_tmp27;
  unsigned int llvm_cbe_call29;
  struct l_class_OC_BaseEvent *llvm_cbe_call31;
  struct l_class_OC_BaseEvent *llvm_cbe_call33;
  struct l_class_OC_BaseEvent *llvm_cbe_call35;
  struct l_struct_OC_Uec_KD__KD_UEC_DTF_DirectTransfer *llvm_cbe_tmp36;
  unsigned int llvm_cbe_tmp39;
  struct l_class_OC_BaseEvent *llvm_cbe_call41;
  struct l_class_OC_BaseEvent *llvm_cbe_call43;
  struct l_class_OC_BaseEvent *llvm_cbe_call45;
  struct l_struct_OC_Uec_KD__KD_UEC_DTF_DirectTransfer *llvm_cbe_tmp46;
  unsigned char *llvm_cbe_tmp49;
  struct l_class_OC_BaseEvent *llvm_cbe_call51;
  unsigned int llvm_cbe_tmp53;
  unsigned int llvm_cbe_tmp56;
  unsigned int llvm_cbe_tmp__70;

#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_this_2e_addr) = llvm_cbe_this;
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_p_uecEvent_ptr_2e_addr) = llvm_cbe_p_uecEvent_ptr;
#line 0 "LLVM INTERNAL"
  llvm_cbe_this1 = *(&llvm_cbe_this_2e_addr);
#line 433 "UecDirectTransfer.cpp"
  _ZNSaIcEC1Ev((&llvm_cbe_agg_2e_tmp));
#line 433 "UecDirectTransfer.cpp"
  _ZNSsC1EPKcRKSaIcE((&llvm_cbe__currentFunction_), ((&__PRETTY_FUNCTION___OC__ZNK3Uec17UecDirectTransfer26handleUecDtfDirectTransferEPNS_8UecEventE.array[((signed int )0u)])), (&llvm_cbe_agg_2e_tmp));
#line 433 "UecDirectTransfer.cpp"
  _ZNSaIcED1Ev((&llvm_cbe_agg_2e_tmp));
#line 433 "UecDirectTransfer.cpp"
  _ZN3Uec17UecDebugCallstackC1ERKSs((&llvm_cbe__callstackTmp_), (&llvm_cbe__currentFunction_));
#line 435 "UecDirectTransfer.cpp"
  llvm_cbe_tmp = *(&llvm_cbe_p_uecEvent_ptr_2e_addr);
#line 435 "UecDirectTransfer.cpp"
  llvm_cbe_call = _ZNK3Uec8UecEvent10getPayLoadEv(llvm_cbe_tmp);
#line 435 "UecDirectTransfer.cpp"
  *(&llvm_cbe_l_payLoad_ptr) = (((struct l_struct_OC_Uec_KD__KD_UEC_DTF_DirectTransfer *)llvm_cbe_call));
#line 437 "UecDirectTransfer.cpp"
  llvm_cbe_tmp14 = *(&llvm_cbe_l_payLoad_ptr);
#line 437 "UecDirectTransfer.cpp"
  llvm_cbe_tmp17 = *((&((&llvm_cbe_tmp14->field2))->field1));
#line 437 "UecDirectTransfer.cpp"
  if ((((unsigned char *)/*NULL*/0) == llvm_cbe_tmp17)) {    goto llvm_cbe_if_2e_then;  } else {    goto llvm_cbe_lor_2e_lhs_2e_false;  }


llvm_cbe_lor_2e_lhs_2e_false:
#line 437 "UecDirectTransfer.cpp"
  llvm_cbe_tmp18 = *(&llvm_cbe_l_payLoad_ptr);
#line 437 "UecDirectTransfer.cpp"
  llvm_cbe_tmp21 = *((&((&llvm_cbe_tmp18->field2))->field0));
#line 437 "UecDirectTransfer.cpp"
  if ((0u == llvm_cbe_tmp21)) {    goto llvm_cbe_if_2e_then;  } else {    goto llvm_cbe_if_2e_end;  }


llvm_cbe_if_2e_then:
#line 441 "UecDirectTransfer.cpp"
  llvm_cbe_call24 = _ZN11DummyStreamlsIPKcEERS_T_((&mout), ((&_OC_str45.array[((signed int )0u)])));
#line 441 "UecDirectTransfer.cpp"
  llvm_cbe_call26 = _ZN11DummyStreamlsIPFRSt8ios_baseS2_EEERS_T_(llvm_cbe_call24, _ZSt3decRSt8ios_base);
#line 441 "UecDirectTransfer.cpp"
  llvm_cbe_tmp27 = *(&llvm_cbe_p_uecEvent_ptr_2e_addr);
#line 441 "UecDirectTransfer.cpp"
  llvm_cbe_call29 = _ZNK3Uec8UecEvent13getInstanceIdEv(llvm_cbe_tmp27);
#line 441 "UecDirectTransfer.cpp"
  llvm_cbe_call31 = _ZN11DummyStreamlsIjEERS_T_(llvm_cbe_call26, llvm_cbe_call29);
#line 441 "UecDirectTransfer.cpp"
  llvm_cbe_call33 = _ZN11DummyStreamlsIPKcEERS_T_(llvm_cbe_call31, ((&_OC_str7.array[((signed int )0u)])));
#line 441 "UecDirectTransfer.cpp"
  llvm_cbe_call35 = _ZN11DummyStreamlsIPFRSt8ios_baseS2_EEERS_T_(llvm_cbe_call33, _ZSt3decRSt8ios_base);
#line 441 "UecDirectTransfer.cpp"
  llvm_cbe_tmp36 = *(&llvm_cbe_l_payLoad_ptr);
#line 441 "UecDirectTransfer.cpp"
  llvm_cbe_tmp39 = *((&((&llvm_cbe_tmp36->field2))->field0));
#line 441 "UecDirectTransfer.cpp"
  llvm_cbe_call41 = _ZN11DummyStreamlsIjEERS_T_(llvm_cbe_call35, llvm_cbe_tmp39);
#line 441 "UecDirectTransfer.cpp"
  llvm_cbe_call43 = _ZN11DummyStreamlsIPKcEERS_T_(llvm_cbe_call41, ((&_OC_str8.array[((signed int )0u)])));
#line 441 "UecDirectTransfer.cpp"
  llvm_cbe_call45 = _ZN11DummyStreamlsIPFRSt8ios_baseS2_EEERS_T_(llvm_cbe_call43, _ZSt3hexRSt8ios_base);
#line 441 "UecDirectTransfer.cpp"
  llvm_cbe_tmp46 = *(&llvm_cbe_l_payLoad_ptr);
#line 441 "UecDirectTransfer.cpp"
  llvm_cbe_tmp49 = *((&((&llvm_cbe_tmp46->field2))->field1));
#line 441 "UecDirectTransfer.cpp"
  llvm_cbe_call51 = _ZN11DummyStreamlsIPhEERS_T_(llvm_cbe_call45, llvm_cbe_tmp49);
#line 445 "UecDirectTransfer.cpp"
  *(&llvm_cbe_retval) = 4294967295u;
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_cleanup_2e_dst) = 1u;
#line 445 "UecDirectTransfer.cpp"
  goto llvm_cbe_cleanup;

llvm_cbe_if_2e_end:
#line 448 "UecDirectTransfer.cpp"
  *(&llvm_cbe_retval) = 1u;
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_cleanup_2e_dst) = 2u;
#line 448 "UecDirectTransfer.cpp"
  goto llvm_cbe_cleanup;

llvm_cbe_cleanup_2e_pad:
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_cleanup_2e_dst54) = 1u;
#line 0 "LLVM INTERNAL"
  goto llvm_cbe_cleanup55;

llvm_cbe_cleanup_2e_pad52:
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_cleanup_2e_dst54) = 1u;
#line 0 "LLVM INTERNAL"
  goto llvm_cbe_cleanup55;

llvm_cbe_cleanup:
#line 449 "UecDirectTransfer.cpp"
  _ZN3Uec17UecDebugCallstackD1Ev((&llvm_cbe__callstackTmp_));
#line 449 "UecDirectTransfer.cpp"
  llvm_cbe_tmp53 = *(&llvm_cbe_cleanup_2e_dst);
#line 449 "UecDirectTransfer.cpp"
  switch (llvm_cbe_tmp53) {
  default:
    goto llvm_cbe_cleanup_2e_end;;
  case 1u:
    goto llvm_cbe_cleanup_2e_pad;  case 2u:
    goto llvm_cbe_cleanup_2e_pad52;  }

llvm_cbe_cleanup_2e_end:
#line 449 "UecDirectTransfer.cpp"
  *(&llvm_cbe_cleanup_2e_dst54) = 0u;
#line 449 "UecDirectTransfer.cpp"
  goto llvm_cbe_cleanup55;

llvm_cbe_cleanup55:
#line 449 "UecDirectTransfer.cpp"
  _ZNSsD1Ev((&llvm_cbe__currentFunction_));
#line 449 "UecDirectTransfer.cpp"
  llvm_cbe_tmp56 = *(&llvm_cbe_cleanup_2e_dst54);
#line 449 "UecDirectTransfer.cpp"
  llvm_cbe_tmp__70 = *(&llvm_cbe_retval);
#line 449 "UecDirectTransfer.cpp"
  return llvm_cbe_tmp__70;
}


unsigned int _ZN3Uec17UecDirectTransfer20setTupSrbSendRequestER14TUP_SrbSendReqj(struct l_class_OC_Uec_KD__KD_UecDirectTransfer *llvm_cbe_this, struct l_struct_OC_TUP_SrbSendReq *llvm_cbe_p_payLoad, unsigned int llvm_cbe_p_respFlag);

#line 0 "LLVM INTERNAL"
unsigned int _ZN3Uec17UecDirectTransfer28sendRrcDlInformationTransferEPNS_8UecEventE(struct l_class_OC_Uec_KD__KD_UecDirectTransfer *llvm_cbe_this, struct l_class_OC_Uec_KD__KD_UecEvent *llvm_cbe_p_uecEvent_ptr) {
  unsigned int llvm_cbe_retval;    /* Address-exposed local */
  struct l_class_OC_Uec_KD__KD_UecDirectTransfer *llvm_cbe_this_2e_addr;    /* Address-exposed local */
  struct l_class_OC_Uec_KD__KD_UecEvent *llvm_cbe_p_uecEvent_ptr_2e_addr;    /* Address-exposed local */
  struct l_class_OC_std_KD__KD_basic_string llvm_cbe__currentFunction_;    /* Address-exposed local */
  struct l_class_OC_BaseEvent llvm_cbe_agg_2e_tmp;    /* Address-exposed local */
  struct l_class_OC_Uec_KD__KD_UecDebugCallstack llvm_cbe__callstackTmp_;    /* Address-exposed local */
  unsigned int llvm_cbe_l_success;    /* Address-exposed local */
  struct l_struct_OC_TUP_SrbSendReq llvm_cbe_l_payLoad;    /* Address-exposed local */
  struct l_struct_OC_SErrcDLInformationTransfer llvm_cbe_l_asn1PayLoad;    /* Address-exposed local */
  struct l_class_OC_Uec_KD__KD_UecUeContextData *llvm_cbe_l_uecUeContextDataWrite_ptr;    /* Address-exposed local */
  struct l_class_OC_Uec_KD__KD_UecEvent llvm_cbe_l_uecEvent;    /* Address-exposed local */
  unsigned int llvm_cbe_cleanup_2e_dst;    /* Address-exposed local */
  unsigned int llvm_cbe_cleanup_2e_dst90;    /* Address-exposed local */
  unsigned int llvm_cbe_cleanup_2e_dst98;    /* Address-exposed local */
  struct l_class_OC_Uec_KD__KD_UecDirectTransfer *llvm_cbe_this1;
  unsigned int llvm_cbe_call;
  struct l_class_OC_Uec_KD__KD_UecEvent *llvm_cbe_tmp;
  unsigned int llvm_cbe_call15;
  unsigned int llvm_cbe_tmp16;
  unsigned int llvm_cbe_tmp17;
  struct l_class_OC_Uec_KD__KD_UecBaseData *llvm_cbe_call20;
  struct l_class_OC_Uec_KD__KD_UecBaseData * (**llvm_cbe_tmp__71) (struct l_class_OC_Uec_KD__KD_UecBaseData *, unsigned int );
  struct l_class_OC_Uec_KD__KD_UecBaseData * (*llvm_cbe_tmp__72) (struct l_class_OC_Uec_KD__KD_UecBaseData *, unsigned int );
  struct l_class_OC_Uec_KD__KD_UecBaseData *llvm_cbe_call22;
  struct l_class_OC_BaseEvent *llvm_cbe_call24;
  struct l_class_OC_BaseEvent *llvm_cbe_call26;
  struct l_class_OC_Uec_KD__KD_UecEvent *llvm_cbe_tmp27;
  unsigned int llvm_cbe_call29;
  struct l_class_OC_BaseEvent *llvm_cbe_call31;
  struct l_class_OC_BaseEvent *llvm_cbe_call33;
  struct l_class_OC_BaseEvent *llvm_cbe_call35;
  unsigned int llvm_cbe_tmp45;
  struct l_class_OC_BaseEvent *llvm_cbe_call47;
  struct l_class_OC_Uec_KD__KD_UecUeContextData *llvm_cbe_tmp49;
  unsigned int llvm_cbe_call51;
  struct l_class_OC_Uec_KD__KD_UecBaseData *llvm_cbe_call60;
  struct l_class_OC_Uec_KD__KD_UecBaseData * (**llvm_cbe_tmp__73) (struct l_class_OC_Uec_KD__KD_UecBaseData *, unsigned int );
  struct l_class_OC_Uec_KD__KD_UecBaseData * (*llvm_cbe_tmp__74) (struct l_class_OC_Uec_KD__KD_UecBaseData *, unsigned int );
  struct l_class_OC_Uec_KD__KD_UecBaseData *llvm_cbe_call63;
  unsigned int  (**llvm_cbe_tmp__75) (struct l_class_OC_Uec_KD__KD_UecBaseData *, struct l_class_OC_Uec_KD__KD_UecEvent *);
  unsigned int  (*llvm_cbe_tmp__76) (struct l_class_OC_Uec_KD__KD_UecBaseData *, struct l_class_OC_Uec_KD__KD_UecEvent *);
  unsigned int llvm_cbe_call66;
  unsigned int llvm_cbe_tmp67;
  struct l_class_OC_BaseEvent *llvm_cbe_call71;
  struct l_class_OC_BaseEvent *llvm_cbe_call73;
  struct l_class_OC_Uec_KD__KD_UecManagerBase *llvm_cbe_call75;
  unsigned int llvm_cbe_call77;
  struct l_class_OC_BaseEvent *llvm_cbe_call79;
  struct l_class_OC_BaseEvent *llvm_cbe_call81;
  struct l_class_OC_BaseEvent *llvm_cbe_call83;
  unsigned int llvm_cbe_tmp84;
  struct l_class_OC_BaseEvent *llvm_cbe_call86;
  unsigned int llvm_cbe_tmp89;
  unsigned int llvm_cbe_tmp95;
  unsigned int llvm_cbe_tmp100;
  unsigned int llvm_cbe_tmp__77;

#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_this_2e_addr) = llvm_cbe_this;
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_p_uecEvent_ptr_2e_addr) = llvm_cbe_p_uecEvent_ptr;
#line 0 "LLVM INTERNAL"
  llvm_cbe_this1 = *(&llvm_cbe_this_2e_addr);
#line 457 "UecDirectTransfer.cpp"
  _ZNSaIcEC1Ev((&llvm_cbe_agg_2e_tmp));
#line 457 "UecDirectTransfer.cpp"
  _ZNSsC1EPKcRKSaIcE((&llvm_cbe__currentFunction_), ((&__PRETTY_FUNCTION___OC__ZN3Uec17UecDirectTransfer28sendRrcDlInformationTransferEPNS_8UecEventE.array[((signed int )0u)])), (&llvm_cbe_agg_2e_tmp));
#line 457 "UecDirectTransfer.cpp"
  _ZNSaIcED1Ev((&llvm_cbe_agg_2e_tmp));
#line 457 "UecDirectTransfer.cpp"
  _ZN3Uec17UecDebugCallstackC1ERKSs((&llvm_cbe__callstackTmp_), (&llvm_cbe__currentFunction_));
#line 459 "UecDirectTransfer.cpp"
  *(&llvm_cbe_l_success) = 0u;
#line 465 "UecDirectTransfer.cpp"
  llvm_cbe_call = _ZN3Uec17UecDirectTransfer20setTupSrbSendRequestER14TUP_SrbSendReqj(llvm_cbe_this1, (&llvm_cbe_l_payLoad), 0u);
#line 468 "UecDirectTransfer.cpp"
  llvm_cbe_tmp = *(&llvm_cbe_p_uecEvent_ptr_2e_addr);
#line 468 "UecDirectTransfer.cpp"
  llvm_cbe_call15 = _ZN3Uec17UecDirectTransfer27setRrcDlInformationTransferEPNS_8UecEventER26SErrcDLInformationTransfer(llvm_cbe_this1, llvm_cbe_tmp, (&llvm_cbe_l_asn1PayLoad));
#line 468 "UecDirectTransfer.cpp"
  *(&llvm_cbe_l_success) = llvm_cbe_call15;
#line 468 "UecDirectTransfer.cpp"
  llvm_cbe_tmp16 = *(&llvm_cbe_l_success);
#line 468 "UecDirectTransfer.cpp"
  if ((1u == llvm_cbe_tmp16)) {    goto llvm_cbe_if_2e_end;  } else {    goto llvm_cbe_if_2e_then;  }


llvm_cbe_if_2e_then:
#line 471 "UecDirectTransfer.cpp"
  llvm_cbe_tmp17 = *(&llvm_cbe_l_success);
#line 471 "UecDirectTransfer.cpp"
  *(&llvm_cbe_retval) = llvm_cbe_tmp17;
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_cleanup_2e_dst90) = 1u;
#line 471 "UecDirectTransfer.cpp"
  goto llvm_cbe_cleanup94;

llvm_cbe_if_2e_end:
#line 484 "UecDirectTransfer.cpp"
  llvm_cbe_call20 = _ZNK3Uec14UecServiceBase16getUecDataAccessEv((((struct l_class_OC_Uec_KD__KD_UecServiceBase *)llvm_cbe_this1)));
#line 484 "UecDirectTransfer.cpp"
  llvm_cbe_tmp__71 = *(((struct l_class_OC_Uec_KD__KD_UecBaseData * (***) (struct l_class_OC_Uec_KD__KD_UecBaseData *, unsigned int ))llvm_cbe_call20));
#line 484 "UecDirectTransfer.cpp"
  llvm_cbe_tmp__72 = *((&llvm_cbe_tmp__71[((signed long long )2ull)]));
#line 484 "UecDirectTransfer.cpp"
  llvm_cbe_call22 = llvm_cbe_tmp__72(llvm_cbe_call20, 1u);
#line 484 "UecDirectTransfer.cpp"
  *(&llvm_cbe_l_uecUeContextDataWrite_ptr) = (((struct l_class_OC_Uec_KD__KD_UecUeContextData *)llvm_cbe_call22));
#line 486 "UecDirectTransfer.cpp"
  llvm_cbe_call24 = _ZN11DummyStreamlsIPKcEERS_T_((&mout), ((&_OC_str46.array[((signed int )0u)])));
#line 486 "UecDirectTransfer.cpp"
  llvm_cbe_call26 = _ZN11DummyStreamlsIPFRSt8ios_baseS2_EEERS_T_(llvm_cbe_call24, _ZSt3decRSt8ios_base);
#line 486 "UecDirectTransfer.cpp"
  llvm_cbe_tmp27 = *(&llvm_cbe_p_uecEvent_ptr_2e_addr);
#line 486 "UecDirectTransfer.cpp"
  llvm_cbe_call29 = _ZNK3Uec8UecEvent13getInstanceIdEv(llvm_cbe_tmp27);
#line 486 "UecDirectTransfer.cpp"
  llvm_cbe_call31 = _ZN11DummyStreamlsIjEERS_T_(llvm_cbe_call26, llvm_cbe_call29);
#line 486 "UecDirectTransfer.cpp"
  llvm_cbe_call33 = _ZN11DummyStreamlsIPKcEERS_T_(llvm_cbe_call31, ((&_OC_str47.array[((signed int )0u)])));
#line 486 "UecDirectTransfer.cpp"
  llvm_cbe_call35 = _ZN11DummyStreamlsIPFRSt8ios_baseS2_EEERS_T_(llvm_cbe_call33, _ZSt3decRSt8ios_base);
#line 486 "UecDirectTransfer.cpp"
  llvm_cbe_tmp45 = *((&((&((&((&((&((&((&((&((&llvm_cbe_l_asn1PayLoad.field4))->field1))->field0))->field1))->field0))->field0))->field1))->field0))->field0));
#line 486 "UecDirectTransfer.cpp"
  llvm_cbe_call47 = _ZN11DummyStreamlsIjEERS_T_(llvm_cbe_call35, llvm_cbe_tmp45);
#line 491 "UecDirectTransfer.cpp"
  llvm_cbe_tmp49 = *(&llvm_cbe_l_uecUeContextDataWrite_ptr);
#line 491 "UecDirectTransfer.cpp"
  llvm_cbe_call51 = _ZNK3Uec16UecUeContextData9getCellIdEv(llvm_cbe_tmp49);
#line 491 "UecDirectTransfer.cpp"
  _ZN3Uec8UecEventC1EjNS_15EUecServiceTypeEjPvjiS2_j((&llvm_cbe_l_uecEvent), 9039u, 2u, llvm_cbe_call51, (((unsigned char *)(&llvm_cbe_l_payLoad))), 52u, 2u, (((unsigned char *)(&llvm_cbe_l_asn1PayLoad))), 56u);
#line 494 "UecDirectTransfer.cpp"
  llvm_cbe_call60 = _ZNK3Uec14UecServiceBase23getUecServiceControllerEv((((struct l_class_OC_Uec_KD__KD_UecServiceBase *)llvm_cbe_this1)));
#line 494 "UecDirectTransfer.cpp"
  llvm_cbe_tmp__73 = *(((struct l_class_OC_Uec_KD__KD_UecBaseData * (***) (struct l_class_OC_Uec_KD__KD_UecBaseData *, unsigned int ))llvm_cbe_call60));
#line 494 "UecDirectTransfer.cpp"
  llvm_cbe_tmp__74 = *((&(*llvm_cbe_tmp__73)));
#line 494 "UecDirectTransfer.cpp"
  llvm_cbe_call63 = llvm_cbe_tmp__74(llvm_cbe_call60, 1u);
#line 494 "UecDirectTransfer.cpp"
  llvm_cbe_tmp__75 = *(((unsigned int  (***) (struct l_class_OC_Uec_KD__KD_UecBaseData *, struct l_class_OC_Uec_KD__KD_UecEvent *))llvm_cbe_call63));
#line 494 "UecDirectTransfer.cpp"
  llvm_cbe_tmp__76 = *((&(*llvm_cbe_tmp__75)));
#line 494 "UecDirectTransfer.cpp"
  llvm_cbe_call66 = llvm_cbe_tmp__76(llvm_cbe_call63, (&llvm_cbe_l_uecEvent));
#line 494 "UecDirectTransfer.cpp"
  *(&llvm_cbe_l_success) = llvm_cbe_call66;
#line 494 "UecDirectTransfer.cpp"
  llvm_cbe_tmp67 = *(&llvm_cbe_l_success);
#line 494 "UecDirectTransfer.cpp"
  if ((1u == llvm_cbe_tmp67)) {    goto llvm_cbe_if_2e_end87;  } else {    goto llvm_cbe_if_2e_then69;  }


llvm_cbe_if_2e_then69:
#line 496 "UecDirectTransfer.cpp"
  llvm_cbe_call71 = _ZN11DummyStreamlsIPKcEERS_T_((&mout), ((&_OC_str46.array[((signed int )0u)])));
#line 496 "UecDirectTransfer.cpp"
  llvm_cbe_call73 = _ZN11DummyStreamlsIPFRSt8ios_baseS2_EEERS_T_(llvm_cbe_call71, _ZSt3decRSt8ios_base);
#line 496 "UecDirectTransfer.cpp"
  llvm_cbe_call75 = _ZNK3Uec14UecServiceBase13getUecManagerEv((((struct l_class_OC_Uec_KD__KD_UecServiceBase *)llvm_cbe_this1)));
#line 496 "UecDirectTransfer.cpp"
  llvm_cbe_call77 = _ZNK3Uec14UecManagerBase13getInstanceIdEv(llvm_cbe_call75);
#line 496 "UecDirectTransfer.cpp"
  llvm_cbe_call79 = _ZN11DummyStreamlsIjEERS_T_(llvm_cbe_call73, llvm_cbe_call77);
#line 496 "UecDirectTransfer.cpp"
  llvm_cbe_call81 = _ZN11DummyStreamlsIPKcEERS_T_(llvm_cbe_call79, ((&_OC_str48.array[((signed int )0u)])));
#line 496 "UecDirectTransfer.cpp"
  llvm_cbe_call83 = _ZN11DummyStreamlsIPFRSt8ios_baseS2_EEERS_T_(llvm_cbe_call81, _ZSt3decRSt8ios_base);
#line 496 "UecDirectTransfer.cpp"
  llvm_cbe_tmp84 = *(&llvm_cbe_l_success);
#line 496 "UecDirectTransfer.cpp"
  llvm_cbe_call86 = _ZN11DummyStreamlsIiEERS_T_(llvm_cbe_call83, llvm_cbe_tmp84);
#line 500 "UecDirectTransfer.cpp"
  *(&llvm_cbe_retval) = 0u;
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_cleanup_2e_dst) = 1u;
#line 500 "UecDirectTransfer.cpp"
  goto llvm_cbe_cleanup;

llvm_cbe_if_2e_end87:
#line 503 "UecDirectTransfer.cpp"
  *(&llvm_cbe_retval) = 1u;
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_cleanup_2e_dst) = 2u;
#line 503 "UecDirectTransfer.cpp"
  goto llvm_cbe_cleanup;

llvm_cbe_cleanup_2e_pad:
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_cleanup_2e_dst90) = 2u;
#line 0 "LLVM INTERNAL"
  goto llvm_cbe_cleanup94;

llvm_cbe_cleanup_2e_pad88:
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_cleanup_2e_dst90) = 3u;
#line 0 "LLVM INTERNAL"
  goto llvm_cbe_cleanup94;

llvm_cbe_cleanup:
#line 504 "UecDirectTransfer.cpp"
  _ZN3Uec8UecEventD1Ev((&llvm_cbe_l_uecEvent));
#line 504 "UecDirectTransfer.cpp"
  llvm_cbe_tmp89 = *(&llvm_cbe_cleanup_2e_dst);
#line 504 "UecDirectTransfer.cpp"
  switch (llvm_cbe_tmp89) {
  default:
    goto llvm_cbe_cleanup_2e_end;;
  case 1u:
    goto llvm_cbe_cleanup_2e_pad;  case 2u:
    goto llvm_cbe_cleanup_2e_pad88;  }

llvm_cbe_cleanup_2e_end:
#line 504 "UecDirectTransfer.cpp"
  *(&llvm_cbe_cleanup_2e_dst90) = 0u;
#line 504 "UecDirectTransfer.cpp"
  goto llvm_cbe_cleanup94;

llvm_cbe_cleanup94:
#line 504 "UecDirectTransfer.cpp"
  _ZN3Uec17UecDebugCallstackD1Ev((&llvm_cbe__callstackTmp_));
#line 504 "UecDirectTransfer.cpp"
  llvm_cbe_tmp95 = *(&llvm_cbe_cleanup_2e_dst90);
#line 504 "UecDirectTransfer.cpp"
  switch (llvm_cbe_tmp95) {
  default:
    goto llvm_cbe_cleanup_2e_end97;;
  case 1u:
    goto llvm_cbe_cleanup_2e_pad91;  case 2u:
    goto llvm_cbe_cleanup_2e_pad92;  case 3u:
    goto llvm_cbe_cleanup_2e_pad93;  }

llvm_cbe_cleanup_2e_end97:
#line 504 "UecDirectTransfer.cpp"
  *(&llvm_cbe_cleanup_2e_dst98) = 0u;
#line 504 "UecDirectTransfer.cpp"
  goto llvm_cbe_cleanup99;

llvm_cbe_cleanup99:
#line 504 "UecDirectTransfer.cpp"
  _ZNSsD1Ev((&llvm_cbe__currentFunction_));
#line 504 "UecDirectTransfer.cpp"
  llvm_cbe_tmp100 = *(&llvm_cbe_cleanup_2e_dst98);
#line 504 "UecDirectTransfer.cpp"
  llvm_cbe_tmp__77 = *(&llvm_cbe_retval);
#line 504 "UecDirectTransfer.cpp"
  return llvm_cbe_tmp__77;

llvm_cbe_cleanup_2e_pad91:
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_cleanup_2e_dst98) = 1u;
#line 0 "LLVM INTERNAL"
  goto llvm_cbe_cleanup99;

llvm_cbe_cleanup_2e_pad92:
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_cleanup_2e_dst98) = 1u;
#line 0 "LLVM INTERNAL"
  goto llvm_cbe_cleanup99;

llvm_cbe_cleanup_2e_pad93:
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_cleanup_2e_dst98) = 1u;
#line 0 "LLVM INTERNAL"
  goto llvm_cbe_cleanup99;
}


#line 0 "LLVM INTERNAL"
unsigned int _ZN3Uec17UecDirectTransfer20setTupSrbSendRequestER14TUP_SrbSendReqj(struct l_class_OC_Uec_KD__KD_UecDirectTransfer *llvm_cbe_this, struct l_struct_OC_TUP_SrbSendReq *llvm_cbe_p_payLoad, unsigned int llvm_cbe_p_respFlag) {
  unsigned int llvm_cbe_retval;    /* Address-exposed local */
  struct l_class_OC_Uec_KD__KD_UecDirectTransfer *llvm_cbe_this_2e_addr;    /* Address-exposed local */
  struct l_struct_OC_TUP_SrbSendReq *llvm_cbe_p_payLoad_2e_addr;    /* Address-exposed local */
  unsigned int llvm_cbe_p_respFlag_2e_addr;    /* Address-exposed local */
  struct l_class_OC_std_KD__KD_basic_string llvm_cbe__currentFunction_;    /* Address-exposed local */
  struct l_class_OC_BaseEvent llvm_cbe_agg_2e_tmp;    /* Address-exposed local */
  struct l_class_OC_Uec_KD__KD_UecDebugCallstack llvm_cbe__callstackTmp_;    /* Address-exposed local */
  struct l_class_OC_Uec_KD__KD_UecUeContextData *llvm_cbe_l_uecUeContextDataWrite_ptr;    /* Address-exposed local */
  unsigned int llvm_cbe_cleanup_2e_dst;    /* Address-exposed local */
  unsigned int llvm_cbe_cleanup_2e_dst60;    /* Address-exposed local */
  struct l_class_OC_Uec_KD__KD_UecDirectTransfer *llvm_cbe_this1;
  struct l_struct_OC_TUP_SrbSendReq *llvm_cbe_tmp;
  unsigned char *llvm_cbe_tmp__78;
  struct l_class_OC_Uec_KD__KD_UecBaseData *llvm_cbe_call;
  struct l_class_OC_Uec_KD__KD_UecBaseData * (**llvm_cbe_tmp__79) (struct l_class_OC_Uec_KD__KD_UecBaseData *, unsigned int );
  struct l_class_OC_Uec_KD__KD_UecBaseData * (*llvm_cbe_tmp__80) (struct l_class_OC_Uec_KD__KD_UecBaseData *, unsigned int );
  struct l_class_OC_Uec_KD__KD_UecBaseData *llvm_cbe_call16;
  struct l_class_OC_Uec_KD__KD_UecUeContextData *llvm_cbe_tmp17;
  unsigned int llvm_cbe_call19;
  struct l_struct_OC_TUP_SrbSendReq *llvm_cbe_tmp20;
  struct l_class_OC_Uec_KD__KD_UecUeContextData *llvm_cbe_tmp22;
  unsigned int llvm_cbe_call24;
  struct l_struct_OC_TUP_SrbSendReq *llvm_cbe_tmp25;
  struct l_class_OC_Uec_KD__KD_UecManagerBase *llvm_cbe_call28;
  unsigned int llvm_cbe_call30;
  struct l_struct_OC_TUP_SrbSendReq *llvm_cbe_tmp31;
  struct l_struct_OC_TUP_SrbSendReq *llvm_cbe_tmp33;
  struct l_class_OC_Uec_KD__KD_UecUeContextData *llvm_cbe_tmp35;
  bool llvm_cbe_call37;
  struct l_struct_OC_TUP_SrbSendReq *llvm_cbe_tmp38;
  unsigned int *llvm_cbe_tmp39;
  unsigned int llvm_cbe_tmp42;
  struct l_struct_OC_TUP_SrbSendReq *llvm_cbe_tmp43;
  unsigned int llvm_cbe_tmp45;
  struct l_struct_OC_TUP_SrbSendReq *llvm_cbe_tmp47;
  unsigned int *llvm_cbe_tmp48;
  struct l_struct_OC_TUP_SrbSendReq *llvm_cbe_tmp53;
  struct l_struct_OC_TUP_SrbSendReq *llvm_cbe_tmp55;
  struct l_struct_OC_TUP_SrbSendReq *llvm_cbe_tmp57;
  unsigned int llvm_cbe_tmp59;
  unsigned int llvm_cbe_tmp62;
  unsigned int llvm_cbe_tmp__81;

#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_this_2e_addr) = llvm_cbe_this;
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_p_payLoad_2e_addr) = llvm_cbe_p_payLoad;
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_p_respFlag_2e_addr) = llvm_cbe_p_respFlag;
#line 0 "LLVM INTERNAL"
  llvm_cbe_this1 = *(&llvm_cbe_this_2e_addr);
#line 988 "UecDirectTransfer.cpp"
  _ZNSaIcEC1Ev((&llvm_cbe_agg_2e_tmp));
#line 988 "UecDirectTransfer.cpp"
  _ZNSsC1EPKcRKSaIcE((&llvm_cbe__currentFunction_), ((&__PRETTY_FUNCTION___OC__ZN3Uec17UecDirectTransfer20setTupSrbSendRequestER14TUP_SrbSendReqj.array[((signed int )0u)])), (&llvm_cbe_agg_2e_tmp));
#line 988 "UecDirectTransfer.cpp"
  _ZNSaIcED1Ev((&llvm_cbe_agg_2e_tmp));
#line 988 "UecDirectTransfer.cpp"
  _ZN3Uec17UecDebugCallstackC1ERKSs((&llvm_cbe__callstackTmp_), (&llvm_cbe__currentFunction_));
#line 990 "UecDirectTransfer.cpp"
  llvm_cbe_tmp = *(&llvm_cbe_p_payLoad_2e_addr);
#line 0 "LLVM INTERNAL"
  llvm_cbe_tmp__78 = memset((((unsigned char *)llvm_cbe_tmp)), 0u, 52ull);
#line 993 "UecDirectTransfer.cpp"
  llvm_cbe_call = _ZNK3Uec14UecServiceBase16getUecDataAccessEv((((struct l_class_OC_Uec_KD__KD_UecServiceBase *)llvm_cbe_this1)));
#line 993 "UecDirectTransfer.cpp"
  llvm_cbe_tmp__79 = *(((struct l_class_OC_Uec_KD__KD_UecBaseData * (***) (struct l_class_OC_Uec_KD__KD_UecBaseData *, unsigned int ))llvm_cbe_call));
#line 993 "UecDirectTransfer.cpp"
  llvm_cbe_tmp__80 = *((&llvm_cbe_tmp__79[((signed long long )2ull)]));
#line 993 "UecDirectTransfer.cpp"
  llvm_cbe_call16 = llvm_cbe_tmp__80(llvm_cbe_call, 1u);
#line 993 "UecDirectTransfer.cpp"
  *(&llvm_cbe_l_uecUeContextDataWrite_ptr) = (((struct l_class_OC_Uec_KD__KD_UecUeContextData *)llvm_cbe_call16));
#line 996 "UecDirectTransfer.cpp"
  llvm_cbe_tmp17 = *(&llvm_cbe_l_uecUeContextDataWrite_ptr);
#line 996 "UecDirectTransfer.cpp"
  llvm_cbe_call19 = _ZNK3Uec16UecUeContextData15getUplaneCellIdEv(llvm_cbe_tmp17);
#line 996 "UecDirectTransfer.cpp"
  llvm_cbe_tmp20 = *(&llvm_cbe_p_payLoad_2e_addr);
#line 996 "UecDirectTransfer.cpp"
  *((&llvm_cbe_tmp20->field0)) = llvm_cbe_call19;
#line 997 "UecDirectTransfer.cpp"
  llvm_cbe_tmp22 = *(&llvm_cbe_l_uecUeContextDataWrite_ptr);
#line 997 "UecDirectTransfer.cpp"
  llvm_cbe_call24 = _ZNK3Uec16UecUeContextData8getCrntiEv(llvm_cbe_tmp22);
#line 997 "UecDirectTransfer.cpp"
  llvm_cbe_tmp25 = *(&llvm_cbe_p_payLoad_2e_addr);
#line 997 "UecDirectTransfer.cpp"
  *((&llvm_cbe_tmp25->field1)) = llvm_cbe_call24;
#line 998 "UecDirectTransfer.cpp"
  llvm_cbe_call28 = _ZNK3Uec14UecServiceBase13getUecManagerEv((((struct l_class_OC_Uec_KD__KD_UecServiceBase *)llvm_cbe_this1)));
#line 998 "UecDirectTransfer.cpp"
  llvm_cbe_call30 = _ZNK3Uec14UecManagerBase13getInstanceIdEv(llvm_cbe_call28);
#line 998 "UecDirectTransfer.cpp"
  llvm_cbe_tmp31 = *(&llvm_cbe_p_payLoad_2e_addr);
#line 998 "UecDirectTransfer.cpp"
  *((&llvm_cbe_tmp31->field2)) = llvm_cbe_call30;
#line 1000 "UecDirectTransfer.cpp"
  llvm_cbe_tmp33 = *(&llvm_cbe_p_payLoad_2e_addr);
#line 1000 "UecDirectTransfer.cpp"
  *((&llvm_cbe_tmp33->field3)) = 2u;
#line 1002 "UecDirectTransfer.cpp"
  llvm_cbe_tmp35 = *(&llvm_cbe_l_uecUeContextDataWrite_ptr);
#line 1002 "UecDirectTransfer.cpp"
  llvm_cbe_call37 = ((_ZN3Uec16UecUeContextData10isSbActiveEj(llvm_cbe_tmp35, 1u))&1);
#line 1005 "UecDirectTransfer.cpp"
  llvm_cbe_tmp38 = *(&llvm_cbe_p_payLoad_2e_addr);
#line 1005 "UecDirectTransfer.cpp"
  llvm_cbe_tmp39 = (&llvm_cbe_tmp38->field6);
#line 1002 "UecDirectTransfer.cpp"
  if (llvm_cbe_call37) {    goto llvm_cbe_if_2e_then;  } else {    goto llvm_cbe_if_2e_else;  }


llvm_cbe_if_2e_then:
#line 1005 "UecDirectTransfer.cpp"
  *llvm_cbe_tmp39 = 2u;
#line 1006 "UecDirectTransfer.cpp"
  goto llvm_cbe_if_2e_end;

llvm_cbe_if_2e_else:
#line 1009 "UecDirectTransfer.cpp"
  *llvm_cbe_tmp39 = 1u;
#line 1010 "UecDirectTransfer.cpp"
  goto llvm_cbe_if_2e_end;

llvm_cbe_if_2e_end:
#line 1012 "UecDirectTransfer.cpp"
  llvm_cbe_tmp42 = *(&llvm_cbe_p_respFlag_2e_addr);
#line 1012 "UecDirectTransfer.cpp"
  llvm_cbe_tmp43 = *(&llvm_cbe_p_payLoad_2e_addr);
#line 1012 "UecDirectTransfer.cpp"
  *((&llvm_cbe_tmp43->field7)) = llvm_cbe_tmp42;
#line 1014 "UecDirectTransfer.cpp"
  llvm_cbe_tmp45 = *(&llvm_cbe_p_respFlag_2e_addr);
#line 1016 "UecDirectTransfer.cpp"
  llvm_cbe_tmp47 = *(&llvm_cbe_p_payLoad_2e_addr);
#line 1016 "UecDirectTransfer.cpp"
  llvm_cbe_tmp48 = (&llvm_cbe_tmp47->field8);
#line 1014 "UecDirectTransfer.cpp"
  if ((llvm_cbe_tmp45 != 0u)) {    goto llvm_cbe_if_2e_then46;  } else {    goto llvm_cbe_if_2e_else49;  }


llvm_cbe_if_2e_then46:
#line 1016 "UecDirectTransfer.cpp"
  *llvm_cbe_tmp48 = 2u;
#line 1017 "UecDirectTransfer.cpp"
  goto llvm_cbe_if_2e_end52;

llvm_cbe_if_2e_else49:
#line 1020 "UecDirectTransfer.cpp"
  *llvm_cbe_tmp48 = 0u;
#line 1021 "UecDirectTransfer.cpp"
  goto llvm_cbe_if_2e_end52;

llvm_cbe_if_2e_end52:
#line 1024 "UecDirectTransfer.cpp"
  llvm_cbe_tmp53 = *(&llvm_cbe_p_payLoad_2e_addr);
#line 1024 "UecDirectTransfer.cpp"
  *((&llvm_cbe_tmp53->field9)) = 0u;
#line 1025 "UecDirectTransfer.cpp"
  llvm_cbe_tmp55 = *(&llvm_cbe_p_payLoad_2e_addr);
#line 1025 "UecDirectTransfer.cpp"
  *((&llvm_cbe_tmp55->field10)) = 0u;
#line 1027 "UecDirectTransfer.cpp"
  llvm_cbe_tmp57 = *(&llvm_cbe_p_payLoad_2e_addr);
#line 1027 "UecDirectTransfer.cpp"
  *((&llvm_cbe_tmp57->field11)) = 0u;
#line 1029 "UecDirectTransfer.cpp"
  *(&llvm_cbe_retval) = 1u;
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_cleanup_2e_dst) = 1u;
#line 1030 "UecDirectTransfer.cpp"
  _ZN3Uec17UecDebugCallstackD1Ev((&llvm_cbe__callstackTmp_));
#line 1030 "UecDirectTransfer.cpp"
  llvm_cbe_tmp59 = *(&llvm_cbe_cleanup_2e_dst);
#line 0 "LLVM INTERNAL"
  if ((llvm_cbe_tmp59 == 1u)) {    goto llvm_cbe_cleanup_2e_pad;  } else {    goto llvm_cbe_cleanup_2e_end;  }


llvm_cbe_cleanup_2e_pad:
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_cleanup_2e_dst60) = 1u;
#line 0 "LLVM INTERNAL"
  goto llvm_cbe_cleanup61;

llvm_cbe_cleanup_2e_end:
#line 1030 "UecDirectTransfer.cpp"
  *(&llvm_cbe_cleanup_2e_dst60) = 0u;
#line 1030 "UecDirectTransfer.cpp"
  goto llvm_cbe_cleanup61;

llvm_cbe_cleanup61:
#line 1030 "UecDirectTransfer.cpp"
  _ZNSsD1Ev((&llvm_cbe__currentFunction_));
#line 1030 "UecDirectTransfer.cpp"
  llvm_cbe_tmp62 = *(&llvm_cbe_cleanup_2e_dst60);
#line 1030 "UecDirectTransfer.cpp"
  llvm_cbe_tmp__81 = *(&llvm_cbe_retval);
#line 1030 "UecDirectTransfer.cpp"
  return llvm_cbe_tmp__81;
}


#line 0 "LLVM INTERNAL"
unsigned int _ZN3Uec17UecDirectTransfer27setRrcDlInformationTransferEPNS_8UecEventER26SErrcDLInformationTransfer(struct l_class_OC_Uec_KD__KD_UecDirectTransfer *llvm_cbe_this, struct l_class_OC_Uec_KD__KD_UecEvent *llvm_cbe_p_uecEvent_ptr, struct l_struct_OC_SErrcDLInformationTransfer *llvm_cbe_p_asn1PayLoad) {
  unsigned int llvm_cbe_retval;    /* Address-exposed local */
  struct l_class_OC_Uec_KD__KD_UecDirectTransfer *llvm_cbe_this_2e_addr;    /* Address-exposed local */
  struct l_class_OC_Uec_KD__KD_UecEvent *llvm_cbe_p_uecEvent_ptr_2e_addr;    /* Address-exposed local */
  struct l_struct_OC_SErrcDLInformationTransfer *llvm_cbe_p_asn1PayLoad_2e_addr;    /* Address-exposed local */
  struct l_class_OC_std_KD__KD_basic_string llvm_cbe__currentFunction_;    /* Address-exposed local */
  struct l_class_OC_BaseEvent llvm_cbe_agg_2e_tmp;    /* Address-exposed local */
  struct l_class_OC_Uec_KD__KD_UecDebugCallstack llvm_cbe__callstackTmp_;    /* Address-exposed local */
  struct l_struct_OC_SS1apUFDownlinkNASTransport *llvm_cbe_l_asn1PayLoad_ptr;    /* Address-exposed local */
  struct l_struct_OC_Uec_KD__KD_UEC_DTF_DirectTransfer *llvm_cbe_l_payLoad_ptr;    /* Address-exposed local */
  unsigned int llvm_cbe_cleanup_2e_dst;    /* Address-exposed local */
  unsigned int llvm_cbe_cleanup_2e_dst150;    /* Address-exposed local */
  struct l_class_OC_Uec_KD__KD_UecDirectTransfer *llvm_cbe_this1;
  struct l_class_OC_Uec_KD__KD_UecEvent *llvm_cbe_tmp;
  unsigned int llvm_cbe_call;
  struct l_class_OC_Uec_KD__KD_UecEvent *llvm_cbe_tmp14;
  unsigned int llvm_cbe_call16;
  struct l_class_OC_Uec_KD__KD_UecEvent *llvm_cbe_tmp19;
  unsigned char *llvm_cbe_call21;
  struct l_struct_OC_SS1apUFDownlinkNASTransport *llvm_cbe_tmp22;
  unsigned int llvm_cbe_tmp25;
  unsigned int llvm_cbe_tmp30;
  unsigned char *llvm_cbe_call32;
  unsigned char *llvm_cbe_tmp37;
  struct l_struct_OC_SS1apUFDownlinkNASTransport *llvm_cbe_tmp38;
  unsigned char *llvm_cbe_tmp41;
  unsigned int llvm_cbe_tmp44;
  unsigned char *llvm_cbe_tmp__82;
  struct l_class_OC_Uec_KD__KD_UecEvent *llvm_cbe_tmp46;
  unsigned int llvm_cbe_call48;
  struct l_class_OC_Uec_KD__KD_UecEvent *llvm_cbe_tmp52;
  unsigned char *llvm_cbe_call54;
  struct l_struct_OC_Uec_KD__KD_UEC_DTF_DirectTransfer *llvm_cbe_tmp55;
  unsigned int llvm_cbe_tmp58;
  unsigned int llvm_cbe_tmp63;
  unsigned char *llvm_cbe_call66;
  unsigned char *llvm_cbe_tmp71;
  struct l_struct_OC_Uec_KD__KD_UEC_DTF_DirectTransfer *llvm_cbe_tmp72;
  unsigned char *llvm_cbe_tmp75;
  unsigned int llvm_cbe_tmp78;
  unsigned char *llvm_cbe_tmp__83;
  struct l_class_OC_BaseEvent *llvm_cbe_call82;
  struct l_class_OC_BaseEvent *llvm_cbe_call84;
  struct l_class_OC_Uec_KD__KD_UecEvent *llvm_cbe_tmp85;
  unsigned int llvm_cbe_call87;
  struct l_class_OC_BaseEvent *llvm_cbe_call89;
  struct l_class_OC_BaseEvent *llvm_cbe_call91;
  struct l_class_OC_BaseEvent *llvm_cbe_call93;
  struct l_class_OC_Uec_KD__KD_UecEvent *llvm_cbe_tmp94;
  unsigned int llvm_cbe_call96;
  struct l_class_OC_BaseEvent *llvm_cbe_call98;
  struct l_class_OC_BaseEvent *llvm_cbe_call100;
  struct l_class_OC_BaseEvent *llvm_cbe_call102;
  struct l_class_OC_Uec_KD__KD_UecEvent *llvm_cbe_tmp103;
  unsigned int llvm_cbe_call105;
  struct l_class_OC_BaseEvent *llvm_cbe_call107;
  struct l_struct_OC_SErrcDLInformationTransfer *llvm_cbe_tmp109;
  unsigned char *llvm_cbe_tmp__84;
  struct l_struct_OC_SErrcDLInformationTransfer *llvm_cbe_tmp111;
  struct l_struct_OC_SErrcDLInformationTransfer *llvm_cbe_tmp113;
  struct l_struct_OC_SErrcDLInformationTransfer *llvm_cbe_tmp116;
  struct l_struct_OC_SErrcDLInformationTransfer *llvm_cbe_tmp121;
  struct l_struct_OC_SErrcDLInformationTransfer *llvm_cbe_tmp128;
  struct l_struct_OC_SErrcDLInformationTransfer *llvm_cbe_tmp136;
  unsigned char *llvm_cbe_tmp__85;
  unsigned int llvm_cbe_tmp149;
  unsigned int llvm_cbe_tmp152;
  unsigned int llvm_cbe_tmp__86;

#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_this_2e_addr) = llvm_cbe_this;
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_p_uecEvent_ptr_2e_addr) = llvm_cbe_p_uecEvent_ptr;
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_p_asn1PayLoad_2e_addr) = llvm_cbe_p_asn1PayLoad;
#line 0 "LLVM INTERNAL"
  llvm_cbe_this1 = *(&llvm_cbe_this_2e_addr);
#line 711 "UecDirectTransfer.cpp"
  _ZNSaIcEC1Ev((&llvm_cbe_agg_2e_tmp));
#line 711 "UecDirectTransfer.cpp"
  _ZNSsC1EPKcRKSaIcE((&llvm_cbe__currentFunction_), ((&__PRETTY_FUNCTION___OC__ZN3Uec17UecDirectTransfer27setRrcDlInformationTransferEPNS_8UecEventER26SErrcDLInformationTransfer.array[((signed int )0u)])), (&llvm_cbe_agg_2e_tmp));
#line 711 "UecDirectTransfer.cpp"
  _ZNSaIcED1Ev((&llvm_cbe_agg_2e_tmp));
#line 711 "UecDirectTransfer.cpp"
  _ZN3Uec17UecDebugCallstackC1ERKSs((&llvm_cbe__callstackTmp_), (&llvm_cbe__currentFunction_));
#line 713 "UecDirectTransfer.cpp"
  llvm_cbe_tmp = *(&llvm_cbe_p_uecEvent_ptr_2e_addr);
#line 713 "UecDirectTransfer.cpp"
  llvm_cbe_call = _ZNK6CEvent10GetEventIdEv((((struct l_class_OC_CEvent *)llvm_cbe_tmp)));
#line 713 "UecDirectTransfer.cpp"
  if ((10205u == llvm_cbe_call)) {    goto llvm_cbe_land_2e_lhs_2e_true;  } else {    goto llvm_cbe_if_2e_else;  }


llvm_cbe_land_2e_lhs_2e_true:
#line 713 "UecDirectTransfer.cpp"
  llvm_cbe_tmp14 = *(&llvm_cbe_p_uecEvent_ptr_2e_addr);
#line 713 "UecDirectTransfer.cpp"
  llvm_cbe_call16 = _ZNK3Uec8UecEvent16getAsn1PayLoadIdEv(llvm_cbe_tmp14);
#line 713 "UecDirectTransfer.cpp"
  if ((1u == llvm_cbe_call16)) {    goto llvm_cbe_if_2e_then;  } else {    goto llvm_cbe_if_2e_else;  }


llvm_cbe_if_2e_then:
#line 717 "UecDirectTransfer.cpp"
  llvm_cbe_tmp19 = *(&llvm_cbe_p_uecEvent_ptr_2e_addr);
#line 717 "UecDirectTransfer.cpp"
  llvm_cbe_call21 = _ZNK3Uec8UecEvent14getAsn1PayLoadEv(llvm_cbe_tmp19);
#line 717 "UecDirectTransfer.cpp"
  *(&llvm_cbe_l_asn1PayLoad_ptr) = (((struct l_struct_OC_SS1apUFDownlinkNASTransport *)llvm_cbe_call21));
#line 720 "UecDirectTransfer.cpp"
  llvm_cbe_tmp22 = *(&llvm_cbe_l_asn1PayLoad_ptr);
#line 720 "UecDirectTransfer.cpp"
  llvm_cbe_tmp25 = *((&((&llvm_cbe_tmp22->field2))->field0));
#line 720 "UecDirectTransfer.cpp"
  *((&((&llvm_cbe_this1->field2))->field0)) = llvm_cbe_tmp25;
#line 721 "UecDirectTransfer.cpp"
  llvm_cbe_tmp30 = *((&((&llvm_cbe_this1->field2))->field0));
#line 721 "UecDirectTransfer.cpp"
  llvm_cbe_call32 = _Znam((((unsigned long long )(((unsigned long long )(((unsigned long long )(unsigned int )llvm_cbe_tmp30))) * ((unsigned long long )1ull)))));
#line 721 "UecDirectTransfer.cpp"
  *((&((&llvm_cbe_this1->field2))->field1)) = llvm_cbe_call32;
#line 723 "UecDirectTransfer.cpp"
  llvm_cbe_tmp37 = *((&((&llvm_cbe_this1->field2))->field1));
#line 723 "UecDirectTransfer.cpp"
  llvm_cbe_tmp38 = *(&llvm_cbe_l_asn1PayLoad_ptr);
#line 723 "UecDirectTransfer.cpp"
  llvm_cbe_tmp41 = *((&((&llvm_cbe_tmp38->field2))->field1));
#line 723 "UecDirectTransfer.cpp"
  llvm_cbe_tmp44 = *((&((&llvm_cbe_this1->field2))->field0));
#line 0 "LLVM INTERNAL"
  llvm_cbe_tmp__82 = memcpy(llvm_cbe_tmp37, llvm_cbe_tmp41, (((unsigned long long )(unsigned int )llvm_cbe_tmp44)));
#line 724 "UecDirectTransfer.cpp"
  goto llvm_cbe_if_2e_end108;

llvm_cbe_if_2e_else:
#line 725 "UecDirectTransfer.cpp"
  llvm_cbe_tmp46 = *(&llvm_cbe_p_uecEvent_ptr_2e_addr);
#line 725 "UecDirectTransfer.cpp"
  llvm_cbe_call48 = _ZNK6CEvent10GetEventIdEv((((struct l_class_OC_CEvent *)llvm_cbe_tmp46)));
#line 725 "UecDirectTransfer.cpp"
  if ((12672u == llvm_cbe_call48)) {    goto llvm_cbe_if_2e_then50;  } else {    goto llvm_cbe_if_2e_else80;  }


llvm_cbe_if_2e_then50:
#line 728 "UecDirectTransfer.cpp"
  llvm_cbe_tmp52 = *(&llvm_cbe_p_uecEvent_ptr_2e_addr);
#line 728 "UecDirectTransfer.cpp"
  llvm_cbe_call54 = _ZNK3Uec8UecEvent10getPayLoadEv(llvm_cbe_tmp52);
#line 728 "UecDirectTransfer.cpp"
  *(&llvm_cbe_l_payLoad_ptr) = (((struct l_struct_OC_Uec_KD__KD_UEC_DTF_DirectTransfer *)llvm_cbe_call54));
#line 731 "UecDirectTransfer.cpp"
  llvm_cbe_tmp55 = *(&llvm_cbe_l_payLoad_ptr);
#line 731 "UecDirectTransfer.cpp"
  llvm_cbe_tmp58 = *((&((&llvm_cbe_tmp55->field2))->field0));
#line 731 "UecDirectTransfer.cpp"
  *((&((&llvm_cbe_this1->field2))->field0)) = llvm_cbe_tmp58;
#line 732 "UecDirectTransfer.cpp"
  llvm_cbe_tmp63 = *((&((&llvm_cbe_this1->field2))->field0));
#line 732 "UecDirectTransfer.cpp"
  llvm_cbe_call66 = _Znam((((unsigned long long )(((unsigned long long )(((unsigned long long )(unsigned int )llvm_cbe_tmp63))) * ((unsigned long long )1ull)))));
#line 732 "UecDirectTransfer.cpp"
  *((&((&llvm_cbe_this1->field2))->field1)) = llvm_cbe_call66;
#line 734 "UecDirectTransfer.cpp"
  llvm_cbe_tmp71 = *((&((&llvm_cbe_this1->field2))->field1));
#line 734 "UecDirectTransfer.cpp"
  llvm_cbe_tmp72 = *(&llvm_cbe_l_payLoad_ptr);
#line 734 "UecDirectTransfer.cpp"
  llvm_cbe_tmp75 = *((&((&llvm_cbe_tmp72->field2))->field1));
#line 734 "UecDirectTransfer.cpp"
  llvm_cbe_tmp78 = *((&((&llvm_cbe_this1->field2))->field0));
#line 0 "LLVM INTERNAL"
  llvm_cbe_tmp__83 = memcpy(llvm_cbe_tmp71, llvm_cbe_tmp75, (((unsigned long long )(unsigned int )llvm_cbe_tmp78)));
#line 735 "UecDirectTransfer.cpp"
  goto llvm_cbe_if_2e_end108;

llvm_cbe_if_2e_else80:
#line 738 "UecDirectTransfer.cpp"
  llvm_cbe_call82 = _ZN11DummyStreamlsIPKcEERS_T_((&mout), ((&_OC_str59.array[((signed int )0u)])));
#line 738 "UecDirectTransfer.cpp"
  llvm_cbe_call84 = _ZN11DummyStreamlsIPFRSt8ios_baseS2_EEERS_T_(llvm_cbe_call82, _ZSt3decRSt8ios_base);
#line 738 "UecDirectTransfer.cpp"
  llvm_cbe_tmp85 = *(&llvm_cbe_p_uecEvent_ptr_2e_addr);
#line 738 "UecDirectTransfer.cpp"
  llvm_cbe_call87 = _ZNK3Uec8UecEvent13getInstanceIdEv(llvm_cbe_tmp85);
#line 738 "UecDirectTransfer.cpp"
  llvm_cbe_call89 = _ZN11DummyStreamlsIjEERS_T_(llvm_cbe_call84, llvm_cbe_call87);
#line 738 "UecDirectTransfer.cpp"
  llvm_cbe_call91 = _ZN11DummyStreamlsIPKcEERS_T_(llvm_cbe_call89, ((&_OC_str60.array[((signed int )0u)])));
#line 738 "UecDirectTransfer.cpp"
  llvm_cbe_call93 = _ZN11DummyStreamlsIPFRSt8ios_baseS2_EEERS_T_(llvm_cbe_call91, _ZSt3decRSt8ios_base);
#line 738 "UecDirectTransfer.cpp"
  llvm_cbe_tmp94 = *(&llvm_cbe_p_uecEvent_ptr_2e_addr);
#line 738 "UecDirectTransfer.cpp"
  llvm_cbe_call96 = _ZNK6CEvent10GetEventIdEv((((struct l_class_OC_CEvent *)llvm_cbe_tmp94)));
#line 738 "UecDirectTransfer.cpp"
  llvm_cbe_call98 = _ZN11DummyStreamlsIjEERS_T_(llvm_cbe_call93, llvm_cbe_call96);
#line 738 "UecDirectTransfer.cpp"
  llvm_cbe_call100 = _ZN11DummyStreamlsIPKcEERS_T_(llvm_cbe_call98, ((&_OC_str61.array[((signed int )0u)])));
#line 738 "UecDirectTransfer.cpp"
  llvm_cbe_call102 = _ZN11DummyStreamlsIPFRSt8ios_baseS2_EEERS_T_(llvm_cbe_call100, _ZSt3decRSt8ios_base);
#line 738 "UecDirectTransfer.cpp"
  llvm_cbe_tmp103 = *(&llvm_cbe_p_uecEvent_ptr_2e_addr);
#line 738 "UecDirectTransfer.cpp"
  llvm_cbe_call105 = _ZNK3Uec8UecEvent16getAsn1PayLoadIdEv(llvm_cbe_tmp103);
#line 738 "UecDirectTransfer.cpp"
  llvm_cbe_call107 = _ZN11DummyStreamlsIiEERS_T_(llvm_cbe_call102, llvm_cbe_call105);
#line 742 "UecDirectTransfer.cpp"
  *(&llvm_cbe_retval) = 0u;
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_cleanup_2e_dst) = 1u;
#line 742 "UecDirectTransfer.cpp"
  goto llvm_cbe_cleanup;

llvm_cbe_if_2e_end108:
#line 745 "UecDirectTransfer.cpp"
  llvm_cbe_tmp109 = *(&llvm_cbe_p_asn1PayLoad_2e_addr);
#line 0 "LLVM INTERNAL"
  llvm_cbe_tmp__84 = memset((((unsigned char *)llvm_cbe_tmp109)), 0u, 56ull);
#line 746 "UecDirectTransfer.cpp"
  llvm_cbe_tmp111 = *(&llvm_cbe_p_asn1PayLoad_2e_addr);
#line 746 "UecDirectTransfer.cpp"
  *((&llvm_cbe_tmp111->field0)) = ((unsigned char )2);
#line 747 "UecDirectTransfer.cpp"
  llvm_cbe_tmp113 = *(&llvm_cbe_p_asn1PayLoad_2e_addr);
#line 747 "UecDirectTransfer.cpp"
  *((&((&llvm_cbe_tmp113->field4))->field0)) = 0u;
#line 748 "UecDirectTransfer.cpp"
  llvm_cbe_tmp116 = *(&llvm_cbe_p_asn1PayLoad_2e_addr);
#line 748 "UecDirectTransfer.cpp"
  *((&((&((&((&llvm_cbe_tmp116->field4))->field1))->field0))->field0)) = 0u;
#line 749 "UecDirectTransfer.cpp"
  llvm_cbe_tmp121 = *(&llvm_cbe_p_asn1PayLoad_2e_addr);
#line 749 "UecDirectTransfer.cpp"
  *((&((&((&((&((&((&llvm_cbe_tmp121->field4))->field1))->field0))->field1))->field0))->field1)) = 0u;
#line 750 "UecDirectTransfer.cpp"
  llvm_cbe_tmp128 = *(&llvm_cbe_p_asn1PayLoad_2e_addr);
#line 750 "UecDirectTransfer.cpp"
  *((&((&((&((&((&((&((&llvm_cbe_tmp128->field4))->field1))->field0))->field1))->field0))->field0))->field0)) = 0u;
#line 751 "UecDirectTransfer.cpp"
  llvm_cbe_tmp136 = *(&llvm_cbe_p_asn1PayLoad_2e_addr);
#line 0 "LLVM INTERNAL"
  llvm_cbe_tmp__85 = memcpy((((unsigned char *)((&((&((&((&((&((&((&((&llvm_cbe_tmp136->field4))->field1))->field0))->field1))->field0))->field0))->field1))->field0)))), (((unsigned char *)((&llvm_cbe_this1->field2)))), 16ull);
#line 753 "UecDirectTransfer.cpp"
  *(&llvm_cbe_retval) = 1u;
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_cleanup_2e_dst) = 2u;
#line 753 "UecDirectTransfer.cpp"
  goto llvm_cbe_cleanup;

llvm_cbe_cleanup_2e_pad:
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_cleanup_2e_dst150) = 1u;
#line 0 "LLVM INTERNAL"
  goto llvm_cbe_cleanup151;

llvm_cbe_cleanup_2e_pad148:
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_cleanup_2e_dst150) = 1u;
#line 0 "LLVM INTERNAL"
  goto llvm_cbe_cleanup151;

llvm_cbe_cleanup:
#line 754 "UecDirectTransfer.cpp"
  _ZN3Uec17UecDebugCallstackD1Ev((&llvm_cbe__callstackTmp_));
#line 754 "UecDirectTransfer.cpp"
  llvm_cbe_tmp149 = *(&llvm_cbe_cleanup_2e_dst);
#line 754 "UecDirectTransfer.cpp"
  switch (llvm_cbe_tmp149) {
  default:
    goto llvm_cbe_cleanup_2e_end;;
  case 1u:
    goto llvm_cbe_cleanup_2e_pad;  case 2u:
    goto llvm_cbe_cleanup_2e_pad148;  }

llvm_cbe_cleanup_2e_end:
#line 754 "UecDirectTransfer.cpp"
  *(&llvm_cbe_cleanup_2e_dst150) = 0u;
#line 754 "UecDirectTransfer.cpp"
  goto llvm_cbe_cleanup151;

llvm_cbe_cleanup151:
#line 754 "UecDirectTransfer.cpp"
  _ZNSsD1Ev((&llvm_cbe__currentFunction_));
#line 754 "UecDirectTransfer.cpp"
  llvm_cbe_tmp152 = *(&llvm_cbe_cleanup_2e_dst150);
#line 754 "UecDirectTransfer.cpp"
  llvm_cbe_tmp__86 = *(&llvm_cbe_retval);
#line 754 "UecDirectTransfer.cpp"
  return llvm_cbe_tmp__86;
}


#line 0 "LLVM INTERNAL"
void _ZN3Uec8UecEventC1EjNS_15EUecServiceTypeEjPvjiS2_j(struct l_class_OC_Uec_KD__KD_UecEvent *llvm_cbe_this, unsigned int llvm_cbe_messageId, unsigned int llvm_cbe_serviceType, unsigned int llvm_cbe_instanceId, unsigned char *llvm_cbe_payLoadPtr, unsigned int llvm_cbe_payloadSize, unsigned int llvm_cbe_asn1PayLoadId, unsigned char *llvm_cbe_asn1PayLoadPtr, unsigned int llvm_cbe_asn1PayloadSize) {
  struct l_class_OC_Uec_KD__KD_UecEvent *llvm_cbe_this_2e_addr;    /* Address-exposed local */
  unsigned int llvm_cbe_messageId_2e_addr;    /* Address-exposed local */
  unsigned int llvm_cbe_serviceType_2e_addr;    /* Address-exposed local */
  unsigned int llvm_cbe_instanceId_2e_addr;    /* Address-exposed local */
  unsigned char *llvm_cbe_payLoadPtr_2e_addr;    /* Address-exposed local */
  unsigned int llvm_cbe_payloadSize_2e_addr;    /* Address-exposed local */
  unsigned int llvm_cbe_asn1PayLoadId_2e_addr;    /* Address-exposed local */
  unsigned char *llvm_cbe_asn1PayLoadPtr_2e_addr;    /* Address-exposed local */
  unsigned int llvm_cbe_asn1PayloadSize_2e_addr;    /* Address-exposed local */
  struct l_class_OC_Uec_KD__KD_UecEvent *llvm_cbe_this1;
  unsigned int llvm_cbe_tmp;
  unsigned int llvm_cbe_tmp2;
  unsigned int llvm_cbe_tmp3;
  unsigned char *llvm_cbe_tmp4;
  unsigned int llvm_cbe_tmp5;
  unsigned int llvm_cbe_tmp6;
  unsigned char *llvm_cbe_tmp7;
  unsigned int llvm_cbe_tmp8;

#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_this_2e_addr) = llvm_cbe_this;
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_messageId_2e_addr) = llvm_cbe_messageId;
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_serviceType_2e_addr) = llvm_cbe_serviceType;
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_instanceId_2e_addr) = llvm_cbe_instanceId;
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_payLoadPtr_2e_addr) = llvm_cbe_payLoadPtr;
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_payloadSize_2e_addr) = llvm_cbe_payloadSize;
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_asn1PayLoadId_2e_addr) = llvm_cbe_asn1PayLoadId;
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_asn1PayLoadPtr_2e_addr) = llvm_cbe_asn1PayLoadPtr;
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_asn1PayloadSize_2e_addr) = llvm_cbe_asn1PayloadSize;
#line 0 "LLVM INTERNAL"
  llvm_cbe_this1 = *(&llvm_cbe_this_2e_addr);
#line 0 "LLVM INTERNAL"
  llvm_cbe_tmp = *(&llvm_cbe_messageId_2e_addr);
#line 0 "LLVM INTERNAL"
  llvm_cbe_tmp2 = *(&llvm_cbe_serviceType_2e_addr);
#line 0 "LLVM INTERNAL"
  llvm_cbe_tmp3 = *(&llvm_cbe_instanceId_2e_addr);
#line 0 "LLVM INTERNAL"
  llvm_cbe_tmp4 = *(&llvm_cbe_payLoadPtr_2e_addr);
#line 0 "LLVM INTERNAL"
  llvm_cbe_tmp5 = *(&llvm_cbe_payloadSize_2e_addr);
#line 0 "LLVM INTERNAL"
  llvm_cbe_tmp6 = *(&llvm_cbe_asn1PayLoadId_2e_addr);
#line 0 "LLVM INTERNAL"
  llvm_cbe_tmp7 = *(&llvm_cbe_asn1PayLoadPtr_2e_addr);
#line 0 "LLVM INTERNAL"
  llvm_cbe_tmp8 = *(&llvm_cbe_asn1PayloadSize_2e_addr);
#line 0 "LLVM INTERNAL"
  _ZN3Uec8UecEventC2EjNS_15EUecServiceTypeEjPvjiS2_j(llvm_cbe_this1, llvm_cbe_tmp, llvm_cbe_tmp2, llvm_cbe_tmp3, llvm_cbe_tmp4, llvm_cbe_tmp5, llvm_cbe_tmp6, llvm_cbe_tmp7, llvm_cbe_tmp8);
#line 101 "UecEvent.hpp"
  return;
}


#line 0 "LLVM INTERNAL"
void _ZN3Uec8UecEventD1Ev(struct l_class_OC_Uec_KD__KD_UecEvent *llvm_cbe_this) {
  struct l_class_OC_Uec_KD__KD_UecEvent *llvm_cbe_this_2e_addr;    /* Address-exposed local */
  struct l_class_OC_Uec_KD__KD_UecEvent *llvm_cbe_this1;

#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_this_2e_addr) = llvm_cbe_this;
#line 0 "LLVM INTERNAL"
  llvm_cbe_this1 = *(&llvm_cbe_this_2e_addr);
#line 0 "LLVM INTERNAL"
  _ZN3Uec8UecEventD2Ev(llvm_cbe_this1);
#line 111 "UecEvent.hpp"
  return;
}


unsigned int _ZN3Uec17UecDirectTransfer22setTupL3MessageRequestER16TUP_L3MessageReq(struct l_class_OC_Uec_KD__KD_UecDirectTransfer *llvm_cbe_this, struct l_struct_OC_TUP_L3MessageReq *llvm_cbe_p_payLoad);

#line 0 "LLVM INTERNAL"
unsigned int _ZN3Uec17UecDirectTransfer24sendS1ApInitialUeMessageEPNS_8UecEventE(struct l_class_OC_Uec_KD__KD_UecDirectTransfer *llvm_cbe_this, struct l_class_OC_Uec_KD__KD_UecEvent *llvm_cbe_p_uecEvent_ptr) {
  unsigned int llvm_cbe_retval;    /* Address-exposed local */
  struct l_class_OC_Uec_KD__KD_UecDirectTransfer *llvm_cbe_this_2e_addr;    /* Address-exposed local */
  struct l_class_OC_Uec_KD__KD_UecEvent *llvm_cbe_p_uecEvent_ptr_2e_addr;    /* Address-exposed local */
  struct l_class_OC_std_KD__KD_basic_string llvm_cbe__currentFunction_;    /* Address-exposed local */
  struct l_class_OC_BaseEvent llvm_cbe_agg_2e_tmp;    /* Address-exposed local */
  struct l_class_OC_Uec_KD__KD_UecDebugCallstack llvm_cbe__callstackTmp_;    /* Address-exposed local */
  unsigned int llvm_cbe_l_success;    /* Address-exposed local */
  struct l_class_OC_Uec_KD__KD_UecUeContextData *llvm_cbe_l_uecUeContextDataWrite_ptr;    /* Address-exposed local */
  struct l_struct_OC_TUP_L3MessageReq llvm_cbe_l_payLoad;    /* Address-exposed local */
  struct l_struct_OC_SS1apUFInitialUEMessage llvm_cbe_l_asn1PayLoad;    /* Address-exposed local */
  struct l_class_OC_Uec_KD__KD_UecEvent llvm_cbe_l_uecEvent;    /* Address-exposed local */
  unsigned int llvm_cbe_cleanup_2e_dst;    /* Address-exposed local */
  unsigned int llvm_cbe_cleanup_2e_dst76;    /* Address-exposed local */
  unsigned int llvm_cbe_cleanup_2e_dst83;    /* Address-exposed local */
  struct l_class_OC_Uec_KD__KD_UecDirectTransfer *llvm_cbe_this1;
  struct l_class_OC_Uec_KD__KD_UecBaseData *llvm_cbe_call;
  struct l_class_OC_Uec_KD__KD_UecBaseData * (**llvm_cbe_tmp__87) (struct l_class_OC_Uec_KD__KD_UecBaseData *, unsigned int );
  struct l_class_OC_Uec_KD__KD_UecBaseData * (*llvm_cbe_tmp__88) (struct l_class_OC_Uec_KD__KD_UecBaseData *, unsigned int );
  struct l_class_OC_Uec_KD__KD_UecBaseData *llvm_cbe_call15;
  unsigned int llvm_cbe_call17;
  struct l_class_OC_Uec_KD__KD_UecEvent *llvm_cbe_tmp;
  unsigned int llvm_cbe_call19;
  struct l_class_OC_BaseEvent *llvm_cbe_call21;
  struct l_class_OC_BaseEvent *llvm_cbe_call23;
  unsigned int llvm_cbe_tmp25;
  struct l_class_OC_BaseEvent *llvm_cbe_call27;
  struct l_class_OC_BaseEvent *llvm_cbe_call29;
  struct l_class_OC_BaseEvent *llvm_cbe_call31;
  unsigned int llvm_cbe_tmp34;
  struct l_class_OC_BaseEvent *llvm_cbe_call36;
  struct l_class_OC_Uec_KD__KD_UecUeContextData *llvm_cbe_tmp38;
  unsigned int llvm_cbe_call40;
  struct l_class_OC_Uec_KD__KD_UecBaseData *llvm_cbe_call49;
  struct l_class_OC_Uec_KD__KD_UecBaseData * (**llvm_cbe_tmp__89) (struct l_class_OC_Uec_KD__KD_UecBaseData *, unsigned int );
  struct l_class_OC_Uec_KD__KD_UecBaseData * (*llvm_cbe_tmp__90) (struct l_class_OC_Uec_KD__KD_UecBaseData *, unsigned int );
  struct l_class_OC_Uec_KD__KD_UecBaseData *llvm_cbe_call52;
  unsigned int  (**llvm_cbe_tmp__91) (struct l_class_OC_Uec_KD__KD_UecBaseData *, struct l_class_OC_Uec_KD__KD_UecEvent *);
  unsigned int  (*llvm_cbe_tmp__92) (struct l_class_OC_Uec_KD__KD_UecBaseData *, struct l_class_OC_Uec_KD__KD_UecEvent *);
  unsigned int llvm_cbe_call55;
  unsigned int llvm_cbe_tmp56;
  struct l_class_OC_BaseEvent *llvm_cbe_call58;
  struct l_class_OC_BaseEvent *llvm_cbe_call60;
  struct l_class_OC_Uec_KD__KD_UecManagerBase *llvm_cbe_call62;
  unsigned int llvm_cbe_call64;
  struct l_class_OC_BaseEvent *llvm_cbe_call66;
  struct l_class_OC_BaseEvent *llvm_cbe_call68;
  struct l_class_OC_BaseEvent *llvm_cbe_call70;
  unsigned int llvm_cbe_tmp71;
  struct l_class_OC_BaseEvent *llvm_cbe_call73;
  unsigned int llvm_cbe_tmp75;
  unsigned int llvm_cbe_tmp80;
  unsigned int llvm_cbe_tmp85;
  unsigned int llvm_cbe_tmp__93;

#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_this_2e_addr) = llvm_cbe_this;
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_p_uecEvent_ptr_2e_addr) = llvm_cbe_p_uecEvent_ptr;
#line 0 "LLVM INTERNAL"
  llvm_cbe_this1 = *(&llvm_cbe_this_2e_addr);
#line 512 "UecDirectTransfer.cpp"
  _ZNSaIcEC1Ev((&llvm_cbe_agg_2e_tmp));
#line 512 "UecDirectTransfer.cpp"
  _ZNSsC1EPKcRKSaIcE((&llvm_cbe__currentFunction_), ((&__PRETTY_FUNCTION___OC__ZN3Uec17UecDirectTransfer24sendS1ApInitialUeMessageEPNS_8UecEventE.array[((signed int )0u)])), (&llvm_cbe_agg_2e_tmp));
#line 512 "UecDirectTransfer.cpp"
  _ZNSaIcED1Ev((&llvm_cbe_agg_2e_tmp));
#line 512 "UecDirectTransfer.cpp"
  _ZN3Uec17UecDebugCallstackC1ERKSs((&llvm_cbe__callstackTmp_), (&llvm_cbe__currentFunction_));
#line 514 "UecDirectTransfer.cpp"
  *(&llvm_cbe_l_success) = 0u;
#line 517 "UecDirectTransfer.cpp"
  llvm_cbe_call = _ZNK3Uec14UecServiceBase16getUecDataAccessEv((((struct l_class_OC_Uec_KD__KD_UecServiceBase *)llvm_cbe_this1)));
#line 517 "UecDirectTransfer.cpp"
  llvm_cbe_tmp__87 = *(((struct l_class_OC_Uec_KD__KD_UecBaseData * (***) (struct l_class_OC_Uec_KD__KD_UecBaseData *, unsigned int ))llvm_cbe_call));
#line 517 "UecDirectTransfer.cpp"
  llvm_cbe_tmp__88 = *((&llvm_cbe_tmp__87[((signed long long )2ull)]));
#line 517 "UecDirectTransfer.cpp"
  llvm_cbe_call15 = llvm_cbe_tmp__88(llvm_cbe_call, 1u);
#line 517 "UecDirectTransfer.cpp"
  *(&llvm_cbe_l_uecUeContextDataWrite_ptr) = (((struct l_class_OC_Uec_KD__KD_UecUeContextData *)llvm_cbe_call15));
#line 523 "UecDirectTransfer.cpp"
  llvm_cbe_call17 = _ZN3Uec17UecDirectTransfer22setTupL3MessageRequestER16TUP_L3MessageReq(llvm_cbe_this1, (&llvm_cbe_l_payLoad));
#line 526 "UecDirectTransfer.cpp"
  llvm_cbe_tmp = *(&llvm_cbe_p_uecEvent_ptr_2e_addr);
#line 526 "UecDirectTransfer.cpp"
  llvm_cbe_call19 = _ZN3Uec17UecDirectTransfer23setS1ApInitialUeMessageEPNS_8UecEventER23SS1apUFInitialUEMessage(llvm_cbe_this1, llvm_cbe_tmp, (&llvm_cbe_l_asn1PayLoad));
#line 528 "UecDirectTransfer.cpp"
  llvm_cbe_call21 = _ZN11DummyStreamlsIPKcEERS_T_((&mout), ((&_OC_str49.array[((signed int )0u)])));
#line 528 "UecDirectTransfer.cpp"
  llvm_cbe_call23 = _ZN11DummyStreamlsIPFRSt8ios_baseS2_EEERS_T_(llvm_cbe_call21, _ZSt3decRSt8ios_base);
#line 528 "UecDirectTransfer.cpp"
  llvm_cbe_tmp25 = *((&llvm_cbe_l_payLoad.field3));
#line 528 "UecDirectTransfer.cpp"
  llvm_cbe_call27 = _ZN11DummyStreamlsIjEERS_T_(llvm_cbe_call23, llvm_cbe_tmp25);
#line 528 "UecDirectTransfer.cpp"
  llvm_cbe_call29 = _ZN11DummyStreamlsIPKcEERS_T_(llvm_cbe_call27, ((&_OC_str50.array[((signed int )0u)])));
#line 528 "UecDirectTransfer.cpp"
  llvm_cbe_call31 = _ZN11DummyStreamlsIPFRSt8ios_baseS2_EEERS_T_(llvm_cbe_call29, _ZSt3decRSt8ios_base);
#line 528 "UecDirectTransfer.cpp"
  llvm_cbe_tmp34 = *((&((&llvm_cbe_l_asn1PayLoad.field1))->field0));
#line 528 "UecDirectTransfer.cpp"
  llvm_cbe_call36 = _ZN11DummyStreamlsIjEERS_T_(llvm_cbe_call31, llvm_cbe_tmp34);
#line 542 "UecDirectTransfer.cpp"
  llvm_cbe_tmp38 = *(&llvm_cbe_l_uecUeContextDataWrite_ptr);
#line 542 "UecDirectTransfer.cpp"
  llvm_cbe_call40 = _ZNK3Uec16UecUeContextData9getCellIdEv(llvm_cbe_tmp38);
#line 542 "UecDirectTransfer.cpp"
  _ZN3Uec8UecEventC1EjNS_15EUecServiceTypeEjPvjiS2_j((&llvm_cbe_l_uecEvent), 10207u, 2u, llvm_cbe_call40, (((unsigned char *)(&llvm_cbe_l_payLoad))), 40u, 3u, (((unsigned char *)(&llvm_cbe_l_asn1PayLoad))), 104u);
#line 545 "UecDirectTransfer.cpp"
  llvm_cbe_call49 = _ZNK3Uec14UecServiceBase23getUecServiceControllerEv((((struct l_class_OC_Uec_KD__KD_UecServiceBase *)llvm_cbe_this1)));
#line 545 "UecDirectTransfer.cpp"
  llvm_cbe_tmp__89 = *(((struct l_class_OC_Uec_KD__KD_UecBaseData * (***) (struct l_class_OC_Uec_KD__KD_UecBaseData *, unsigned int ))llvm_cbe_call49));
#line 545 "UecDirectTransfer.cpp"
  llvm_cbe_tmp__90 = *((&(*llvm_cbe_tmp__89)));
#line 545 "UecDirectTransfer.cpp"
  llvm_cbe_call52 = llvm_cbe_tmp__90(llvm_cbe_call49, 3u);
#line 545 "UecDirectTransfer.cpp"
  llvm_cbe_tmp__91 = *(((unsigned int  (***) (struct l_class_OC_Uec_KD__KD_UecBaseData *, struct l_class_OC_Uec_KD__KD_UecEvent *))llvm_cbe_call52));
#line 545 "UecDirectTransfer.cpp"
  llvm_cbe_tmp__92 = *((&(*llvm_cbe_tmp__91)));
#line 545 "UecDirectTransfer.cpp"
  llvm_cbe_call55 = llvm_cbe_tmp__92(llvm_cbe_call52, (&llvm_cbe_l_uecEvent));
#line 545 "UecDirectTransfer.cpp"
  *(&llvm_cbe_l_success) = llvm_cbe_call55;
#line 545 "UecDirectTransfer.cpp"
  llvm_cbe_tmp56 = *(&llvm_cbe_l_success);
#line 545 "UecDirectTransfer.cpp"
  if ((1u == llvm_cbe_tmp56)) {    goto llvm_cbe_if_2e_end;  } else {    goto llvm_cbe_if_2e_then;  }


llvm_cbe_if_2e_then:
#line 547 "UecDirectTransfer.cpp"
  llvm_cbe_call58 = _ZN11DummyStreamlsIPKcEERS_T_((&mout), ((&_OC_str49.array[((signed int )0u)])));
#line 547 "UecDirectTransfer.cpp"
  llvm_cbe_call60 = _ZN11DummyStreamlsIPFRSt8ios_baseS2_EEERS_T_(llvm_cbe_call58, _ZSt3decRSt8ios_base);
#line 547 "UecDirectTransfer.cpp"
  llvm_cbe_call62 = _ZNK3Uec14UecServiceBase13getUecManagerEv((((struct l_class_OC_Uec_KD__KD_UecServiceBase *)llvm_cbe_this1)));
#line 547 "UecDirectTransfer.cpp"
  llvm_cbe_call64 = _ZNK3Uec14UecManagerBase13getInstanceIdEv(llvm_cbe_call62);
#line 547 "UecDirectTransfer.cpp"
  llvm_cbe_call66 = _ZN11DummyStreamlsIjEERS_T_(llvm_cbe_call60, llvm_cbe_call64);
#line 547 "UecDirectTransfer.cpp"
  llvm_cbe_call68 = _ZN11DummyStreamlsIPKcEERS_T_(llvm_cbe_call66, ((&_OC_str48.array[((signed int )0u)])));
#line 547 "UecDirectTransfer.cpp"
  llvm_cbe_call70 = _ZN11DummyStreamlsIPFRSt8ios_baseS2_EEERS_T_(llvm_cbe_call68, _ZSt3decRSt8ios_base);
#line 547 "UecDirectTransfer.cpp"
  llvm_cbe_tmp71 = *(&llvm_cbe_l_success);
#line 547 "UecDirectTransfer.cpp"
  llvm_cbe_call73 = _ZN11DummyStreamlsIiEERS_T_(llvm_cbe_call70, llvm_cbe_tmp71);
#line 551 "UecDirectTransfer.cpp"
  *(&llvm_cbe_retval) = 0u;
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_cleanup_2e_dst) = 1u;
#line 551 "UecDirectTransfer.cpp"
  goto llvm_cbe_cleanup;

llvm_cbe_if_2e_end:
#line 554 "UecDirectTransfer.cpp"
  *(&llvm_cbe_retval) = 1u;
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_cleanup_2e_dst) = 2u;
#line 554 "UecDirectTransfer.cpp"
  goto llvm_cbe_cleanup;

llvm_cbe_cleanup_2e_pad:
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_cleanup_2e_dst76) = 1u;
#line 0 "LLVM INTERNAL"
  goto llvm_cbe_cleanup79;

llvm_cbe_cleanup_2e_pad74:
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_cleanup_2e_dst76) = 2u;
#line 0 "LLVM INTERNAL"
  goto llvm_cbe_cleanup79;

llvm_cbe_cleanup:
#line 555 "UecDirectTransfer.cpp"
  _ZN3Uec8UecEventD1Ev((&llvm_cbe_l_uecEvent));
#line 555 "UecDirectTransfer.cpp"
  llvm_cbe_tmp75 = *(&llvm_cbe_cleanup_2e_dst);
#line 555 "UecDirectTransfer.cpp"
  switch (llvm_cbe_tmp75) {
  default:
    goto llvm_cbe_cleanup_2e_end;;
  case 1u:
    goto llvm_cbe_cleanup_2e_pad;  case 2u:
    goto llvm_cbe_cleanup_2e_pad74;  }

llvm_cbe_cleanup_2e_end:
#line 555 "UecDirectTransfer.cpp"
  *(&llvm_cbe_cleanup_2e_dst76) = 0u;
#line 555 "UecDirectTransfer.cpp"
  goto llvm_cbe_cleanup79;

llvm_cbe_cleanup79:
#line 555 "UecDirectTransfer.cpp"
  _ZN3Uec17UecDebugCallstackD1Ev((&llvm_cbe__callstackTmp_));
#line 555 "UecDirectTransfer.cpp"
  llvm_cbe_tmp80 = *(&llvm_cbe_cleanup_2e_dst76);
#line 555 "UecDirectTransfer.cpp"
  switch (llvm_cbe_tmp80) {
  default:
    goto llvm_cbe_cleanup_2e_end82;;
  case 1u:
    goto llvm_cbe_cleanup_2e_pad77;  case 2u:
    goto llvm_cbe_cleanup_2e_pad78;  }

llvm_cbe_cleanup_2e_end82:
#line 555 "UecDirectTransfer.cpp"
  *(&llvm_cbe_cleanup_2e_dst83) = 0u;
#line 555 "UecDirectTransfer.cpp"
  goto llvm_cbe_cleanup84;

llvm_cbe_cleanup84:
#line 555 "UecDirectTransfer.cpp"
  _ZNSsD1Ev((&llvm_cbe__currentFunction_));
#line 555 "UecDirectTransfer.cpp"
  llvm_cbe_tmp85 = *(&llvm_cbe_cleanup_2e_dst83);
#line 555 "UecDirectTransfer.cpp"
  llvm_cbe_tmp__93 = *(&llvm_cbe_retval);
#line 555 "UecDirectTransfer.cpp"
  return llvm_cbe_tmp__93;

llvm_cbe_cleanup_2e_pad77:
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_cleanup_2e_dst83) = 1u;
#line 0 "LLVM INTERNAL"
  goto llvm_cbe_cleanup84;

llvm_cbe_cleanup_2e_pad78:
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_cleanup_2e_dst83) = 1u;
#line 0 "LLVM INTERNAL"
  goto llvm_cbe_cleanup84;
}


#line 0 "LLVM INTERNAL"
unsigned int _ZN3Uec17UecDirectTransfer22setTupL3MessageRequestER16TUP_L3MessageReq(struct l_class_OC_Uec_KD__KD_UecDirectTransfer *llvm_cbe_this, struct l_struct_OC_TUP_L3MessageReq *llvm_cbe_p_payLoad) {
  unsigned int llvm_cbe_retval;    /* Address-exposed local */
  struct l_class_OC_Uec_KD__KD_UecDirectTransfer *llvm_cbe_this_2e_addr;    /* Address-exposed local */
  struct l_struct_OC_TUP_L3MessageReq *llvm_cbe_p_payLoad_2e_addr;    /* Address-exposed local */
  struct l_class_OC_std_KD__KD_basic_string llvm_cbe__currentFunction_;    /* Address-exposed local */
  struct l_class_OC_BaseEvent llvm_cbe_agg_2e_tmp;    /* Address-exposed local */
  struct l_class_OC_Uec_KD__KD_UecDebugCallstack llvm_cbe__callstackTmp_;    /* Address-exposed local */
  struct l_class_OC_Uec_KD__KD_UecUeContextData *llvm_cbe_l_uecUeContextDataWrite_ptr;    /* Address-exposed local */
  unsigned int llvm_cbe_cleanup_2e_dst;    /* Address-exposed local */
  unsigned int llvm_cbe_cleanup_2e_dst49;    /* Address-exposed local */
  struct l_class_OC_Uec_KD__KD_UecDirectTransfer *llvm_cbe_this1;
  struct l_struct_OC_TUP_L3MessageReq *llvm_cbe_tmp;
  unsigned char *llvm_cbe_tmp__94;
  struct l_class_OC_Uec_KD__KD_UecBaseData *llvm_cbe_call;
  struct l_class_OC_Uec_KD__KD_UecBaseData * (**llvm_cbe_tmp__95) (struct l_class_OC_Uec_KD__KD_UecBaseData *, unsigned int );
  struct l_class_OC_Uec_KD__KD_UecBaseData * (*llvm_cbe_tmp__96) (struct l_class_OC_Uec_KD__KD_UecBaseData *, unsigned int );
  struct l_class_OC_Uec_KD__KD_UecBaseData *llvm_cbe_call16;
  struct l_class_OC_Uec_KD__KD_UecUeContextData *llvm_cbe_tmp17;
  unsigned int llvm_cbe_call19;
  struct l_struct_OC_TUP_L3MessageReq *llvm_cbe_tmp20;
  struct l_struct_OC_TUP_L3MessageReq *llvm_cbe_tmp22;
  struct l_class_OC_Uec_KD__KD_UecUeContextData *llvm_cbe_tmp24;
  unsigned int llvm_cbe_call26;
  struct l_struct_OC_TUP_L3MessageReq *llvm_cbe_tmp27;
  struct l_class_OC_Uec_KD__KD_UecManagerBase *llvm_cbe_call30;
  unsigned int llvm_cbe_call32;
  struct l_struct_OC_TUP_L3MessageReq *llvm_cbe_tmp33;
  struct l_class_OC_Uec_KD__KD_UecUeContextData *llvm_cbe_tmp35;
  bool llvm_cbe_call37;
  struct l_struct_OC_TUP_L3MessageReq *llvm_cbe_tmp38;
  struct l_class_OC_Uec_KD__KD_UecUeContextData *llvm_cbe_tmp40;
  unsigned int llvm_cbe_call42;
  struct l_struct_OC_TUP_L3MessageReq *llvm_cbe_tmp44;
  struct l_struct_OC_TUP_L3MessageReq *llvm_cbe_tmp46;
  unsigned int llvm_cbe_tmp48;
  unsigned int llvm_cbe_tmp51;
  unsigned int llvm_cbe_tmp__97;

#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_this_2e_addr) = llvm_cbe_this;
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_p_payLoad_2e_addr) = llvm_cbe_p_payLoad;
#line 0 "LLVM INTERNAL"
  llvm_cbe_this1 = *(&llvm_cbe_this_2e_addr);
#line 963 "UecDirectTransfer.cpp"
  _ZNSaIcEC1Ev((&llvm_cbe_agg_2e_tmp));
#line 963 "UecDirectTransfer.cpp"
  _ZNSsC1EPKcRKSaIcE((&llvm_cbe__currentFunction_), ((&__PRETTY_FUNCTION___OC__ZN3Uec17UecDirectTransfer22setTupL3MessageRequestER16TUP_L3MessageReq.array[((signed int )0u)])), (&llvm_cbe_agg_2e_tmp));
#line 963 "UecDirectTransfer.cpp"
  _ZNSaIcED1Ev((&llvm_cbe_agg_2e_tmp));
#line 963 "UecDirectTransfer.cpp"
  _ZN3Uec17UecDebugCallstackC1ERKSs((&llvm_cbe__callstackTmp_), (&llvm_cbe__currentFunction_));
#line 965 "UecDirectTransfer.cpp"
  llvm_cbe_tmp = *(&llvm_cbe_p_payLoad_2e_addr);
#line 0 "LLVM INTERNAL"
  llvm_cbe_tmp__94 = memset((((unsigned char *)llvm_cbe_tmp)), 0u, 40ull);
#line 968 "UecDirectTransfer.cpp"
  llvm_cbe_call = _ZNK3Uec14UecServiceBase16getUecDataAccessEv((((struct l_class_OC_Uec_KD__KD_UecServiceBase *)llvm_cbe_this1)));
#line 968 "UecDirectTransfer.cpp"
  llvm_cbe_tmp__95 = *(((struct l_class_OC_Uec_KD__KD_UecBaseData * (***) (struct l_class_OC_Uec_KD__KD_UecBaseData *, unsigned int ))llvm_cbe_call));
#line 968 "UecDirectTransfer.cpp"
  llvm_cbe_tmp__96 = *((&llvm_cbe_tmp__95[((signed long long )2ull)]));
#line 968 "UecDirectTransfer.cpp"
  llvm_cbe_call16 = llvm_cbe_tmp__96(llvm_cbe_call, 1u);
#line 968 "UecDirectTransfer.cpp"
  *(&llvm_cbe_l_uecUeContextDataWrite_ptr) = (((struct l_class_OC_Uec_KD__KD_UecUeContextData *)llvm_cbe_call16));
#line 970 "UecDirectTransfer.cpp"
  llvm_cbe_tmp17 = *(&llvm_cbe_l_uecUeContextDataWrite_ptr);
#line 970 "UecDirectTransfer.cpp"
  llvm_cbe_call19 = _ZNK3Uec16UecUeContextData11getS1LinkIdEv(llvm_cbe_tmp17);
#line 970 "UecDirectTransfer.cpp"
  llvm_cbe_tmp20 = *(&llvm_cbe_p_payLoad_2e_addr);
#line 970 "UecDirectTransfer.cpp"
  *((&llvm_cbe_tmp20->field0)) = llvm_cbe_call19;
#line 971 "UecDirectTransfer.cpp"
  llvm_cbe_tmp22 = *(&llvm_cbe_p_payLoad_2e_addr);
#line 971 "UecDirectTransfer.cpp"
  *((&llvm_cbe_tmp22->field1)) = 1u;
#line 972 "UecDirectTransfer.cpp"
  llvm_cbe_tmp24 = *(&llvm_cbe_l_uecUeContextDataWrite_ptr);
#line 972 "UecDirectTransfer.cpp"
  llvm_cbe_call26 = _ZNK3Uec16UecUeContextData12getEnbUeS1IdEv(llvm_cbe_tmp24);
#line 972 "UecDirectTransfer.cpp"
  llvm_cbe_tmp27 = *(&llvm_cbe_p_payLoad_2e_addr);
#line 972 "UecDirectTransfer.cpp"
  *((&llvm_cbe_tmp27->field2)) = llvm_cbe_call26;
#line 973 "UecDirectTransfer.cpp"
  llvm_cbe_call30 = _ZNK3Uec14UecServiceBase13getUecManagerEv((((struct l_class_OC_Uec_KD__KD_UecServiceBase *)llvm_cbe_this1)));
#line 973 "UecDirectTransfer.cpp"
  llvm_cbe_call32 = _ZNK3Uec14UecManagerBase13getInstanceIdEv(llvm_cbe_call30);
#line 973 "UecDirectTransfer.cpp"
  llvm_cbe_tmp33 = *(&llvm_cbe_p_payLoad_2e_addr);
#line 973 "UecDirectTransfer.cpp"
  *((&llvm_cbe_tmp33->field3)) = llvm_cbe_call32;
#line 975 "UecDirectTransfer.cpp"
  llvm_cbe_tmp35 = *(&llvm_cbe_l_uecUeContextDataWrite_ptr);
#line 975 "UecDirectTransfer.cpp"
  llvm_cbe_call37 = ((_ZNK3Uec16UecUeContextData16isValidMmeUeS1IdEv(llvm_cbe_tmp35))&1);
#line 975 "UecDirectTransfer.cpp"
  llvm_cbe_tmp38 = *(&llvm_cbe_p_payLoad_2e_addr);
#line 975 "UecDirectTransfer.cpp"
  *((&llvm_cbe_tmp38->field4)) = (((llvm_cbe_call37) ? (1u) : (0u)));
#line 976 "UecDirectTransfer.cpp"
  llvm_cbe_tmp40 = *(&llvm_cbe_l_uecUeContextDataWrite_ptr);
#line 976 "UecDirectTransfer.cpp"
  llvm_cbe_call42 = _ZNK3Uec16UecUeContextData12getMmeUeS1IdEv(llvm_cbe_tmp40);
#line 976 "UecDirectTransfer.cpp"
  llvm_cbe_tmp44 = *(&llvm_cbe_p_payLoad_2e_addr);
#line 976 "UecDirectTransfer.cpp"
  *((&llvm_cbe_tmp44->field6)) = (((unsigned long long )(unsigned int )llvm_cbe_call42));
#line 977 "UecDirectTransfer.cpp"
  llvm_cbe_tmp46 = *(&llvm_cbe_p_payLoad_2e_addr);
#line 977 "UecDirectTransfer.cpp"
  *((&llvm_cbe_tmp46->field7)) = 0u;
#line 979 "UecDirectTransfer.cpp"
  *(&llvm_cbe_retval) = 1u;
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_cleanup_2e_dst) = 1u;
#line 980 "UecDirectTransfer.cpp"
  _ZN3Uec17UecDebugCallstackD1Ev((&llvm_cbe__callstackTmp_));
#line 980 "UecDirectTransfer.cpp"
  llvm_cbe_tmp48 = *(&llvm_cbe_cleanup_2e_dst);
#line 0 "LLVM INTERNAL"
  if ((llvm_cbe_tmp48 == 1u)) {    goto llvm_cbe_cleanup_2e_pad;  } else {    goto llvm_cbe_cleanup_2e_end;  }


llvm_cbe_cleanup_2e_pad:
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_cleanup_2e_dst49) = 1u;
#line 0 "LLVM INTERNAL"
  goto llvm_cbe_cleanup50;

llvm_cbe_cleanup_2e_end:
#line 980 "UecDirectTransfer.cpp"
  *(&llvm_cbe_cleanup_2e_dst49) = 0u;
#line 980 "UecDirectTransfer.cpp"
  goto llvm_cbe_cleanup50;

llvm_cbe_cleanup50:
#line 980 "UecDirectTransfer.cpp"
  _ZNSsD1Ev((&llvm_cbe__currentFunction_));
#line 980 "UecDirectTransfer.cpp"
  llvm_cbe_tmp51 = *(&llvm_cbe_cleanup_2e_dst49);
#line 980 "UecDirectTransfer.cpp"
  llvm_cbe_tmp__97 = *(&llvm_cbe_retval);
#line 980 "UecDirectTransfer.cpp"
  return llvm_cbe_tmp__97;
}


#line 0 "LLVM INTERNAL"
unsigned int _ZN3Uec17UecDirectTransfer23setS1ApInitialUeMessageEPNS_8UecEventER23SS1apUFInitialUEMessage(struct l_class_OC_Uec_KD__KD_UecDirectTransfer *llvm_cbe_this, struct l_class_OC_Uec_KD__KD_UecEvent *llvm_cbe_p_uecEvent_ptr, struct l_struct_OC_SS1apUFInitialUEMessage *llvm_cbe_p_asn1PayLoad) {
  unsigned int llvm_cbe_retval;    /* Address-exposed local */
  struct l_class_OC_Uec_KD__KD_UecDirectTransfer *llvm_cbe_this_2e_addr;    /* Address-exposed local */
  struct l_class_OC_Uec_KD__KD_UecEvent *llvm_cbe_p_uecEvent_ptr_2e_addr;    /* Address-exposed local */
  struct l_struct_OC_SS1apUFInitialUEMessage *llvm_cbe_p_asn1PayLoad_2e_addr;    /* Address-exposed local */
  struct l_class_OC_std_KD__KD_basic_string llvm_cbe__currentFunction_;    /* Address-exposed local */
  struct l_class_OC_BaseEvent llvm_cbe_agg_2e_tmp;    /* Address-exposed local */
  struct l_class_OC_Uec_KD__KD_UecDebugCallstack llvm_cbe__callstackTmp_;    /* Address-exposed local */
  struct l_struct_OC_Uec_KD__KD_UEC_DTF_DirectTransfer *llvm_cbe_l_payLoad_ptr;    /* Address-exposed local */
  struct l_class_OC_Uec_KD__KD_UecUeContextData *llvm_cbe_l_uecUeContextDataWrite_ptr;    /* Address-exposed local */
  struct l_class_OC_Uec_KD__KD_UecCellData *llvm_cbe_l_uecCellDataRead_ptr;    /* Address-exposed local */
  struct l_struct_OC_SAmRlcPbTab llvm_cbe_l_plmnId;    /* Address-exposed local */
  struct l_struct_OC_SS1apGUMMEI llvm_cbe_coerce;    /* Address-exposed local */
  struct l_struct_OC_UErrcInitialUEIdentity llvm_cbe_l_initialUeIdentity;    /* Address-exposed local */
  unsigned int llvm_cbe_cleanup_2e_dst;    /* Address-exposed local */
  unsigned int llvm_cbe_cleanup_2e_dst179;    /* Address-exposed local */
  struct l_class_OC_Uec_KD__KD_UecDirectTransfer *llvm_cbe_this1;
  struct l_class_OC_Uec_KD__KD_UecEvent *llvm_cbe_tmp;
  unsigned char *llvm_cbe_call;
  struct l_class_OC_Uec_KD__KD_UecBaseData *llvm_cbe_call16;
  struct l_class_OC_Uec_KD__KD_UecBaseData * (**llvm_cbe_tmp__98) (struct l_class_OC_Uec_KD__KD_UecBaseData *, unsigned int );
  struct l_class_OC_Uec_KD__KD_UecBaseData * (*llvm_cbe_tmp__99) (struct l_class_OC_Uec_KD__KD_UecBaseData *, unsigned int );
  struct l_class_OC_Uec_KD__KD_UecBaseData *llvm_cbe_call18;
  struct l_class_OC_Uec_KD__KD_UecBaseData *llvm_cbe_call21;
  struct l_class_OC_Uec_KD__KD_UecCellData * (**llvm_cbe_tmp__100) (struct l_class_OC_Uec_KD__KD_UecBaseData *, unsigned int );
  struct l_class_OC_Uec_KD__KD_UecCellData * (*llvm_cbe_tmp__101) (struct l_class_OC_Uec_KD__KD_UecBaseData *, unsigned int );
  struct l_class_OC_Uec_KD__KD_UecUeContextData *llvm_cbe_tmp23;
  unsigned int llvm_cbe_call25;
  struct l_class_OC_Uec_KD__KD_UecCellData *llvm_cbe_call27;
  struct l_struct_OC_SS1apUFInitialUEMessage *llvm_cbe_tmp28;
  unsigned char *llvm_cbe_tmp__102;
  struct l_class_OC_Uec_KD__KD_UecUeContextData *llvm_cbe_tmp29;
  unsigned int llvm_cbe_call31;
  struct l_struct_OC_SS1apUFInitialUEMessage *llvm_cbe_tmp32;
  struct l_struct_OC_Uec_KD__KD_UEC_DTF_DirectTransfer *llvm_cbe_tmp34;
  unsigned int llvm_cbe_tmp37;
  unsigned int llvm_cbe_tmp42;
  unsigned char *llvm_cbe_call45;
  unsigned char *llvm_cbe_tmp50;
  struct l_struct_OC_Uec_KD__KD_UEC_DTF_DirectTransfer *llvm_cbe_tmp51;
  unsigned char *llvm_cbe_tmp54;
  struct l_struct_OC_Uec_KD__KD_UEC_DTF_DirectTransfer *llvm_cbe_tmp55;
  unsigned int llvm_cbe_tmp58;
  unsigned char *llvm_cbe_tmp__103;
  struct l_struct_OC_SS1apUFInitialUEMessage *llvm_cbe_tmp60;
  unsigned char *llvm_cbe_tmp__104;
  struct l_class_OC_Uec_KD__KD_UecCellData *llvm_cbe_tmp65;
  unsigned int llvm_cbe_call67;
  struct l_struct_OC_SS1apUFInitialUEMessage *llvm_cbe_tmp69;
  struct l_class_OC_Uec_KD__KD_UecCellData *llvm_cbe_tmp73;
  unsigned int llvm_cbe_call75;
  struct l_struct_OC_SS1apUFInitialUEMessage *llvm_cbe_tmp78;
  struct l_class_OC_Uec_KD__KD_UecUeContextData *llvm_cbe_tmp85;
  struct l_struct_OC_SS1apUEAggregateMaximumBitrate llvm_cbe_call87;
  unsigned char *llvm_cbe_tmp__105;
  unsigned int llvm_cbe_tmp92;
  struct l_struct_OC_SS1apUFInitialUEMessage *llvm_cbe_tmp93;
  unsigned int llvm_cbe_tmp98;
  struct l_struct_OC_SS1apUFInitialUEMessage *llvm_cbe_tmp99;
  unsigned int llvm_cbe_tmp104;
  struct l_struct_OC_SS1apUFInitialUEMessage *llvm_cbe_tmp105;
  struct l_class_OC_Uec_KD__KD_UecCellData *llvm_cbe_tmp109;
  unsigned int llvm_cbe_call111;
  struct l_struct_OC_SS1apUFInitialUEMessage *llvm_cbe_tmp112;
  struct l_class_OC_Uec_KD__KD_UecCellData *llvm_cbe_tmp115;
  struct l_struct_OC_SAmRlcPbTab *llvm_cbe_call117;
  unsigned int llvm_cbe_tmp119;
  struct l_struct_OC_SS1apUFInitialUEMessage *llvm_cbe_tmp120;
  struct l_class_OC_Uec_KD__KD_UecCellData *llvm_cbe_tmp124;
  struct l_struct_OC_SAmRlcPbTab *llvm_cbe_call126;
  unsigned int llvm_cbe_tmp128;
  struct l_struct_OC_SS1apUFInitialUEMessage *llvm_cbe_tmp129;
  struct l_class_OC_Uec_KD__KD_UecCellData *llvm_cbe_tmp133;
  struct l_struct_OC_SAmRlcPbTab *llvm_cbe_call135;
  unsigned int llvm_cbe_tmp137;
  struct l_struct_OC_SS1apUFInitialUEMessage *llvm_cbe_tmp138;
  struct l_struct_OC_SS1apUFInitialUEMessage *llvm_cbe_tmp142;
  struct l_class_OC_Uec_KD__KD_UecUeContextData *llvm_cbe_tmp144;
  bool llvm_cbe_call146;
  struct l_class_OC_Uec_KD__KD_UecUeContextData *llvm_cbe_tmp148;
  struct l_struct_OC_UErrcInitialUEIdentity *llvm_cbe_call150;
  unsigned char *llvm_cbe_tmp__106;
  unsigned int llvm_cbe_tmp154;
  struct l_struct_OC_SS1apUFInitialUEMessage *llvm_cbe_tmp156;
  struct l_struct_OC_SS1apUFInitialUEMessage *llvm_cbe_tmp158;
  struct l_class_OC_Uec_KD__KD_UecUeContextData *llvm_cbe_tmp160;
  struct l_struct_OC_UErrcInitialUEIdentity *llvm_cbe_call162;
  unsigned char *llvm_cbe_tmp__107;
  struct l_struct_OC_SS1apUFInitialUEMessage *llvm_cbe_tmp168;
  struct l_class_OC_Uec_KD__KD_UecUeContextData *llvm_cbe_tmp170;
  unsigned int *llvm_cbe_call172;
  unsigned int llvm_cbe_tmp173;
  unsigned int llvm_cbe_call175;
  struct l_struct_OC_SS1apUFInitialUEMessage *llvm_cbe_tmp176;
  unsigned int llvm_cbe_tmp178;
  unsigned int llvm_cbe_tmp181;
  unsigned int llvm_cbe_tmp__108;

#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_this_2e_addr) = llvm_cbe_this;
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_p_uecEvent_ptr_2e_addr) = llvm_cbe_p_uecEvent_ptr;
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_p_asn1PayLoad_2e_addr) = llvm_cbe_p_asn1PayLoad;
#line 0 "LLVM INTERNAL"
  llvm_cbe_this1 = *(&llvm_cbe_this_2e_addr);
#line 762 "UecDirectTransfer.cpp"
  _ZNSaIcEC1Ev((&llvm_cbe_agg_2e_tmp));
#line 762 "UecDirectTransfer.cpp"
  _ZNSsC1EPKcRKSaIcE((&llvm_cbe__currentFunction_), ((&__PRETTY_FUNCTION___OC__ZN3Uec17UecDirectTransfer23setS1ApInitialUeMessageEPNS_8UecEventER23SS1apUFInitialUEMessage.array[((signed int )0u)])), (&llvm_cbe_agg_2e_tmp));
#line 762 "UecDirectTransfer.cpp"
  _ZNSaIcED1Ev((&llvm_cbe_agg_2e_tmp));
#line 762 "UecDirectTransfer.cpp"
  _ZN3Uec17UecDebugCallstackC1ERKSs((&llvm_cbe__callstackTmp_), (&llvm_cbe__currentFunction_));
#line 765 "UecDirectTransfer.cpp"
  llvm_cbe_tmp = *(&llvm_cbe_p_uecEvent_ptr_2e_addr);
#line 765 "UecDirectTransfer.cpp"
  llvm_cbe_call = _ZNK3Uec8UecEvent10getPayLoadEv(llvm_cbe_tmp);
#line 765 "UecDirectTransfer.cpp"
  *(&llvm_cbe_l_payLoad_ptr) = (((struct l_struct_OC_Uec_KD__KD_UEC_DTF_DirectTransfer *)llvm_cbe_call));
#line 768 "UecDirectTransfer.cpp"
  llvm_cbe_call16 = _ZNK3Uec14UecServiceBase16getUecDataAccessEv((((struct l_class_OC_Uec_KD__KD_UecServiceBase *)llvm_cbe_this1)));
#line 768 "UecDirectTransfer.cpp"
  llvm_cbe_tmp__98 = *(((struct l_class_OC_Uec_KD__KD_UecBaseData * (***) (struct l_class_OC_Uec_KD__KD_UecBaseData *, unsigned int ))llvm_cbe_call16));
#line 768 "UecDirectTransfer.cpp"
  llvm_cbe_tmp__99 = *((&llvm_cbe_tmp__98[((signed long long )2ull)]));
#line 768 "UecDirectTransfer.cpp"
  llvm_cbe_call18 = llvm_cbe_tmp__99(llvm_cbe_call16, 1u);
#line 768 "UecDirectTransfer.cpp"
  *(&llvm_cbe_l_uecUeContextDataWrite_ptr) = (((struct l_class_OC_Uec_KD__KD_UecUeContextData *)llvm_cbe_call18));
#line 769 "UecDirectTransfer.cpp"
  llvm_cbe_call21 = _ZNK3Uec14UecServiceBase23getUecServiceControllerEv((((struct l_class_OC_Uec_KD__KD_UecServiceBase *)llvm_cbe_this1)));
#line 769 "UecDirectTransfer.cpp"
  llvm_cbe_tmp__100 = *(((struct l_class_OC_Uec_KD__KD_UecCellData * (***) (struct l_class_OC_Uec_KD__KD_UecBaseData *, unsigned int ))llvm_cbe_call21));
#line 769 "UecDirectTransfer.cpp"
  llvm_cbe_tmp__101 = *((&llvm_cbe_tmp__100[((signed long long )12ull)]));
#line 769 "UecDirectTransfer.cpp"
  llvm_cbe_tmp23 = *(&llvm_cbe_l_uecUeContextDataWrite_ptr);
#line 769 "UecDirectTransfer.cpp"
  llvm_cbe_call25 = _ZNK3Uec16UecUeContextData9getCellIdEv(llvm_cbe_tmp23);
#line 769 "UecDirectTransfer.cpp"
  llvm_cbe_call27 = llvm_cbe_tmp__101(llvm_cbe_call21, llvm_cbe_call25);
#line 769 "UecDirectTransfer.cpp"
  *(&llvm_cbe_l_uecCellDataRead_ptr) = llvm_cbe_call27;
#line 771 "UecDirectTransfer.cpp"
  llvm_cbe_tmp28 = *(&llvm_cbe_p_asn1PayLoad_2e_addr);
#line 0 "LLVM INTERNAL"
  llvm_cbe_tmp__102 = memset((((unsigned char *)llvm_cbe_tmp28)), 0u, 104ull);
#line 774 "UecDirectTransfer.cpp"
  llvm_cbe_tmp29 = *(&llvm_cbe_l_uecUeContextDataWrite_ptr);
#line 774 "UecDirectTransfer.cpp"
  llvm_cbe_call31 = _ZNK3Uec16UecUeContextData12getEnbUeS1IdEv(llvm_cbe_tmp29);
#line 774 "UecDirectTransfer.cpp"
  llvm_cbe_tmp32 = *(&llvm_cbe_p_asn1PayLoad_2e_addr);
#line 774 "UecDirectTransfer.cpp"
  *((&llvm_cbe_tmp32->field0)) = llvm_cbe_call31;
#line 777 "UecDirectTransfer.cpp"
  llvm_cbe_tmp34 = *(&llvm_cbe_l_payLoad_ptr);
#line 777 "UecDirectTransfer.cpp"
  llvm_cbe_tmp37 = *((&((&llvm_cbe_tmp34->field2))->field0));
#line 777 "UecDirectTransfer.cpp"
  *((&((&llvm_cbe_this1->field2))->field0)) = llvm_cbe_tmp37;
#line 778 "UecDirectTransfer.cpp"
  llvm_cbe_tmp42 = *((&((&llvm_cbe_this1->field2))->field0));
#line 778 "UecDirectTransfer.cpp"
  llvm_cbe_call45 = _Znam((((unsigned long long )(((unsigned long long )(((unsigned long long )(unsigned int )llvm_cbe_tmp42))) * ((unsigned long long )1ull)))));
#line 778 "UecDirectTransfer.cpp"
  *((&((&llvm_cbe_this1->field2))->field1)) = llvm_cbe_call45;
#line 780 "UecDirectTransfer.cpp"
  llvm_cbe_tmp50 = *((&((&llvm_cbe_this1->field2))->field1));
#line 780 "UecDirectTransfer.cpp"
  llvm_cbe_tmp51 = *(&llvm_cbe_l_payLoad_ptr);
#line 780 "UecDirectTransfer.cpp"
  llvm_cbe_tmp54 = *((&((&llvm_cbe_tmp51->field2))->field1));
#line 780 "UecDirectTransfer.cpp"
  llvm_cbe_tmp55 = *(&llvm_cbe_l_payLoad_ptr);
#line 780 "UecDirectTransfer.cpp"
  llvm_cbe_tmp58 = *((&((&llvm_cbe_tmp55->field2))->field0));
#line 0 "LLVM INTERNAL"
  llvm_cbe_tmp__103 = memcpy(llvm_cbe_tmp50, llvm_cbe_tmp54, (((unsigned long long )(unsigned int )llvm_cbe_tmp58)));
#line 782 "UecDirectTransfer.cpp"
  llvm_cbe_tmp60 = *(&llvm_cbe_p_asn1PayLoad_2e_addr);
#line 0 "LLVM INTERNAL"
  llvm_cbe_tmp__104 = memcpy((((unsigned char *)((&llvm_cbe_tmp60->field1)))), (((unsigned char *)((&llvm_cbe_this1->field2)))), 16ull);
#line 785 "UecDirectTransfer.cpp"
  llvm_cbe_tmp65 = *(&llvm_cbe_l_uecCellDataRead_ptr);
#line 785 "UecDirectTransfer.cpp"
  llvm_cbe_call67 = _ZNK3Uec11UecCellData6getTacEv(llvm_cbe_tmp65);
#line 785 "UecDirectTransfer.cpp"
  llvm_cbe_tmp69 = *(&llvm_cbe_p_asn1PayLoad_2e_addr);
#line 785 "UecDirectTransfer.cpp"
  *((&(*((&(*((&((&((&llvm_cbe_tmp69->field2))->field1))->field0))).array[((signed int )0u)]))))) = (((unsigned char )((((unsigned int )(((unsigned int )llvm_cbe_call67) >> ((unsigned int )8u)))) & 255u)));
#line 786 "UecDirectTransfer.cpp"
  llvm_cbe_tmp73 = *(&llvm_cbe_l_uecCellDataRead_ptr);
#line 786 "UecDirectTransfer.cpp"
  llvm_cbe_call75 = _ZNK3Uec11UecCellData6getTacEv(llvm_cbe_tmp73);
#line 786 "UecDirectTransfer.cpp"
  llvm_cbe_tmp78 = *(&llvm_cbe_p_asn1PayLoad_2e_addr);
#line 786 "UecDirectTransfer.cpp"
  *((&((&(*((&((&((&llvm_cbe_tmp78->field2))->field1))->field0))).array[((signed int )0u)]))[((signed long long )1ull)])) = (((unsigned char )(llvm_cbe_call75 & 255u)));
#line 788 "UecDirectTransfer.cpp"
  llvm_cbe_tmp85 = *(&llvm_cbe_l_uecUeContextDataWrite_ptr);
#line 788 "UecDirectTransfer.cpp"
  llvm_cbe_call87 = _ZNK3Uec16UecUeContextData11getS1GummeiEv(llvm_cbe_tmp85);
#line 788 "UecDirectTransfer.cpp"
  ((struct __attribute__ ((packed, aligned(1))) {struct l_struct_OC_SS1apUEAggregateMaximumBitrate data; } *)(((struct l_struct_OC_SS1apUEAggregateMaximumBitrate *)(&llvm_cbe_coerce))))->data = llvm_cbe_call87;
#line 0 "LLVM INTERNAL"
  llvm_cbe_tmp__105 = memcpy((((unsigned char *)(&llvm_cbe_l_plmnId))), (((unsigned char *)((&llvm_cbe_coerce.field0)))), 12ull);
#line 790 "UecDirectTransfer.cpp"
  llvm_cbe_tmp92 = *((&llvm_cbe_l_plmnId.field2));
#line 790 "UecDirectTransfer.cpp"
  llvm_cbe_tmp93 = *(&llvm_cbe_p_asn1PayLoad_2e_addr);
#line 790 "UecDirectTransfer.cpp"
  *((&((&((&llvm_cbe_tmp93->field2))->field0))->field2)) = llvm_cbe_tmp92;
#line 791 "UecDirectTransfer.cpp"
  llvm_cbe_tmp98 = *((&llvm_cbe_l_plmnId.field1));
#line 791 "UecDirectTransfer.cpp"
  llvm_cbe_tmp99 = *(&llvm_cbe_p_asn1PayLoad_2e_addr);
#line 791 "UecDirectTransfer.cpp"
  *((&((&((&llvm_cbe_tmp99->field2))->field0))->field1)) = llvm_cbe_tmp98;
#line 792 "UecDirectTransfer.cpp"
  llvm_cbe_tmp104 = *((&llvm_cbe_l_plmnId.field0));
#line 792 "UecDirectTransfer.cpp"
  llvm_cbe_tmp105 = *(&llvm_cbe_p_asn1PayLoad_2e_addr);
#line 792 "UecDirectTransfer.cpp"
  *((&((&((&llvm_cbe_tmp105->field2))->field0))->field0)) = llvm_cbe_tmp104;
#line 795 "UecDirectTransfer.cpp"
  llvm_cbe_tmp109 = *(&llvm_cbe_l_uecCellDataRead_ptr);
#line 795 "UecDirectTransfer.cpp"
  llvm_cbe_call111 = _ZNK3Uec11UecCellData9getCellIdEv(llvm_cbe_tmp109);
#line 795 "UecDirectTransfer.cpp"
  llvm_cbe_tmp112 = *(&llvm_cbe_p_asn1PayLoad_2e_addr);
#line 795 "UecDirectTransfer.cpp"
  *((&((&llvm_cbe_tmp112->field3))->field1)) = llvm_cbe_call111;
#line 796 "UecDirectTransfer.cpp"
  llvm_cbe_tmp115 = *(&llvm_cbe_l_uecCellDataRead_ptr);
#line 796 "UecDirectTransfer.cpp"
  llvm_cbe_call117 = _ZNK3Uec11UecCellData12getPlmnRestLEj(llvm_cbe_tmp115, 0u);
#line 796 "UecDirectTransfer.cpp"
  llvm_cbe_tmp119 = *((&llvm_cbe_call117->field0));
#line 796 "UecDirectTransfer.cpp"
  llvm_cbe_tmp120 = *(&llvm_cbe_p_asn1PayLoad_2e_addr);
#line 796 "UecDirectTransfer.cpp"
  *((&((&((&llvm_cbe_tmp120->field3))->field0))->field2)) = llvm_cbe_tmp119;
#line 797 "UecDirectTransfer.cpp"
  llvm_cbe_tmp124 = *(&llvm_cbe_l_uecCellDataRead_ptr);
#line 797 "UecDirectTransfer.cpp"
  llvm_cbe_call126 = _ZNK3Uec11UecCellData12getPlmnRestLEj(llvm_cbe_tmp124, 0u);
#line 797 "UecDirectTransfer.cpp"
  llvm_cbe_tmp128 = *((&llvm_cbe_call126->field1));
#line 797 "UecDirectTransfer.cpp"
  llvm_cbe_tmp129 = *(&llvm_cbe_p_asn1PayLoad_2e_addr);
#line 797 "UecDirectTransfer.cpp"
  *((&((&((&llvm_cbe_tmp129->field3))->field0))->field1)) = llvm_cbe_tmp128;
#line 798 "UecDirectTransfer.cpp"
  llvm_cbe_tmp133 = *(&llvm_cbe_l_uecCellDataRead_ptr);
#line 798 "UecDirectTransfer.cpp"
  llvm_cbe_call135 = _ZNK3Uec11UecCellData12getPlmnRestLEj(llvm_cbe_tmp133, 0u);
#line 798 "UecDirectTransfer.cpp"
  llvm_cbe_tmp137 = *((&llvm_cbe_call135->field2));
#line 798 "UecDirectTransfer.cpp"
  llvm_cbe_tmp138 = *(&llvm_cbe_p_asn1PayLoad_2e_addr);
#line 798 "UecDirectTransfer.cpp"
  *((&((&((&llvm_cbe_tmp138->field3))->field0))->field0)) = llvm_cbe_tmp137;
#line 801 "UecDirectTransfer.cpp"
  llvm_cbe_tmp142 = *(&llvm_cbe_p_asn1PayLoad_2e_addr);
#line 801 "UecDirectTransfer.cpp"
  *((&llvm_cbe_tmp142->field5)) = 0u;
#line 803 "UecDirectTransfer.cpp"
  llvm_cbe_tmp144 = *(&llvm_cbe_l_uecUeContextDataWrite_ptr);
#line 803 "UecDirectTransfer.cpp"
  llvm_cbe_call146 = ((_ZNK3Uec16UecUeContextData24isValidInitialUeIdentityEv(llvm_cbe_tmp144))&1);
#line 803 "UecDirectTransfer.cpp"
  if (llvm_cbe_call146) {    goto llvm_cbe_if_2e_then;  } else {    goto llvm_cbe_if_2e_end167;  }


llvm_cbe_if_2e_then:
#line 805 "UecDirectTransfer.cpp"
  llvm_cbe_tmp148 = *(&llvm_cbe_l_uecUeContextDataWrite_ptr);
#line 805 "UecDirectTransfer.cpp"
  llvm_cbe_call150 = _ZNK3Uec16UecUeContextData20getInitialUeIdentityEv(llvm_cbe_tmp148);
#line 0 "LLVM INTERNAL"
  llvm_cbe_tmp__106 = memcpy((((unsigned char *)(&llvm_cbe_l_initialUeIdentity))), (((unsigned char *)llvm_cbe_call150)), 12ull);
#line 807 "UecDirectTransfer.cpp"
  llvm_cbe_tmp154 = *((&llvm_cbe_l_initialUeIdentity.field0));
#line 807 "UecDirectTransfer.cpp"
  if ((0u == llvm_cbe_tmp154)) {    goto llvm_cbe_if_2e_then155;  } else {    goto llvm_cbe_if_2e_end167;  }


llvm_cbe_if_2e_then155:
#line 809 "UecDirectTransfer.cpp"
  llvm_cbe_tmp156 = *(&llvm_cbe_p_asn1PayLoad_2e_addr);
#line 809 "UecDirectTransfer.cpp"
  *((&llvm_cbe_tmp156->field5)) = 1u;
#line 810 "UecDirectTransfer.cpp"
  llvm_cbe_tmp158 = *(&llvm_cbe_p_asn1PayLoad_2e_addr);
#line 810 "UecDirectTransfer.cpp"
  llvm_cbe_tmp160 = *(&llvm_cbe_l_uecUeContextDataWrite_ptr);
#line 810 "UecDirectTransfer.cpp"
  llvm_cbe_call162 = _ZNK3Uec16UecUeContextData20getInitialUeIdentityEv(llvm_cbe_tmp160);
#line 0 "LLVM INTERNAL"
  llvm_cbe_tmp__107 = memcpy((((unsigned char *)((&llvm_cbe_tmp158->field6)))), (((unsigned char *)((&((&llvm_cbe_call162->field1))->field0)))), 8ull);
#line 811 "UecDirectTransfer.cpp"
  goto llvm_cbe_if_2e_end167;

llvm_cbe_if_2e_end167:
#line 814 "UecDirectTransfer.cpp"
  llvm_cbe_tmp168 = *(&llvm_cbe_p_asn1PayLoad_2e_addr);
#line 814 "UecDirectTransfer.cpp"
  *((&llvm_cbe_tmp168->field7)) = 0u;
#line 815 "UecDirectTransfer.cpp"
  llvm_cbe_tmp170 = *(&llvm_cbe_l_uecUeContextDataWrite_ptr);
#line 815 "UecDirectTransfer.cpp"
  llvm_cbe_call172 = _ZNK3Uec16UecUeContextData21getEstablishmentCauseEv(llvm_cbe_tmp170);
#line 815 "UecDirectTransfer.cpp"
  llvm_cbe_tmp173 = *llvm_cbe_call172;
#line 815 "UecDirectTransfer.cpp"
  llvm_cbe_call175 = _ZN3Uec12UecParamConv26convErrcEstablishmentCauseE23EErrcEstablishmentCause(llvm_cbe_tmp173);
#line 815 "UecDirectTransfer.cpp"
  llvm_cbe_tmp176 = *(&llvm_cbe_p_asn1PayLoad_2e_addr);
#line 815 "UecDirectTransfer.cpp"
  *((&llvm_cbe_tmp176->field4)) = llvm_cbe_call175;
#line 817 "UecDirectTransfer.cpp"
  *(&llvm_cbe_retval) = 1u;
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_cleanup_2e_dst) = 1u;
#line 818 "UecDirectTransfer.cpp"
  _ZN3Uec17UecDebugCallstackD1Ev((&llvm_cbe__callstackTmp_));
#line 818 "UecDirectTransfer.cpp"
  llvm_cbe_tmp178 = *(&llvm_cbe_cleanup_2e_dst);
#line 0 "LLVM INTERNAL"
  if ((llvm_cbe_tmp178 == 1u)) {    goto llvm_cbe_cleanup_2e_pad;  } else {    goto llvm_cbe_cleanup_2e_end;  }


llvm_cbe_cleanup_2e_pad:
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_cleanup_2e_dst179) = 1u;
#line 0 "LLVM INTERNAL"
  goto llvm_cbe_cleanup180;

llvm_cbe_cleanup_2e_end:
#line 818 "UecDirectTransfer.cpp"
  *(&llvm_cbe_cleanup_2e_dst179) = 0u;
#line 818 "UecDirectTransfer.cpp"
  goto llvm_cbe_cleanup180;

llvm_cbe_cleanup180:
#line 818 "UecDirectTransfer.cpp"
  _ZNSsD1Ev((&llvm_cbe__currentFunction_));
#line 818 "UecDirectTransfer.cpp"
  llvm_cbe_tmp181 = *(&llvm_cbe_cleanup_2e_dst179);
#line 818 "UecDirectTransfer.cpp"
  llvm_cbe_tmp__108 = *(&llvm_cbe_retval);
#line 818 "UecDirectTransfer.cpp"
  return llvm_cbe_tmp__108;
}


#line 0 "LLVM INTERNAL"
unsigned int _ZN3Uec17UecDirectTransfer32sendS1ApNasNonDeliveryIndicationEPNS_8UecEventE(struct l_class_OC_Uec_KD__KD_UecDirectTransfer *llvm_cbe_this, struct l_class_OC_Uec_KD__KD_UecEvent *llvm_cbe_p_uecEvent_ptr) {
  unsigned int llvm_cbe_retval;    /* Address-exposed local */
  struct l_class_OC_Uec_KD__KD_UecDirectTransfer *llvm_cbe_this_2e_addr;    /* Address-exposed local */
  struct l_class_OC_Uec_KD__KD_UecEvent *llvm_cbe_p_uecEvent_ptr_2e_addr;    /* Address-exposed local */
  struct l_class_OC_std_KD__KD_basic_string llvm_cbe__currentFunction_;    /* Address-exposed local */
  struct l_class_OC_BaseEvent llvm_cbe_agg_2e_tmp;    /* Address-exposed local */
  struct l_class_OC_Uec_KD__KD_UecDebugCallstack llvm_cbe__callstackTmp_;    /* Address-exposed local */
  unsigned int llvm_cbe_l_success;    /* Address-exposed local */
  struct l_class_OC_Uec_KD__KD_UecUeContextData *llvm_cbe_l_uecUeContextDataWrite_ptr;    /* Address-exposed local */
  struct l_struct_OC_TUP_L3MessageReq llvm_cbe_l_payLoad;    /* Address-exposed local */
  struct l_struct_OC_SS1apUFNASNonDeliveryIndication llvm_cbe_l_asn1PayLoad;    /* Address-exposed local */
  struct l_class_OC_Uec_KD__KD_UecEvent llvm_cbe_l_uecEvent;    /* Address-exposed local */
  unsigned int llvm_cbe_cleanup_2e_dst;    /* Address-exposed local */
  unsigned int llvm_cbe_cleanup_2e_dst106;    /* Address-exposed local */
  unsigned int llvm_cbe_cleanup_2e_dst114;    /* Address-exposed local */
  struct l_class_OC_Uec_KD__KD_UecDirectTransfer *llvm_cbe_this1;
  struct l_class_OC_Uec_KD__KD_UecBaseData *llvm_cbe_call;
  struct l_class_OC_Uec_KD__KD_UecBaseData * (**llvm_cbe_tmp__109) (struct l_class_OC_Uec_KD__KD_UecBaseData *, unsigned int );
  struct l_class_OC_Uec_KD__KD_UecBaseData * (*llvm_cbe_tmp__110) (struct l_class_OC_Uec_KD__KD_UecBaseData *, unsigned int );
  struct l_class_OC_Uec_KD__KD_UecBaseData *llvm_cbe_call15;
  struct l_class_OC_Uec_KD__KD_UecUeContextData *llvm_cbe_tmp;
  bool llvm_cbe_call17;
  struct l_class_OC_BaseEvent *llvm_cbe_call19;
  struct l_class_OC_BaseEvent *llvm_cbe_call21;
  struct l_class_OC_Uec_KD__KD_UecEvent *llvm_cbe_tmp22;
  unsigned int llvm_cbe_call24;
  struct l_class_OC_BaseEvent *llvm_cbe_call26;
  struct l_class_OC_BaseEvent *llvm_cbe_call28;
  struct l_class_OC_BaseEvent *llvm_cbe_call30;
  struct l_class_OC_Uec_KD__KD_UecUeContextData *llvm_cbe_tmp31;
  bool llvm_cbe_call33;
  struct l_class_OC_BaseEvent *llvm_cbe_call35;
  unsigned int llvm_cbe_call39;
  struct l_class_OC_Uec_KD__KD_UecEvent *llvm_cbe_tmp40;
  unsigned int llvm_cbe_call42;
  unsigned int llvm_cbe_tmp43;
  unsigned int llvm_cbe_tmp45;
  struct l_class_OC_BaseEvent *llvm_cbe_call48;
  struct l_class_OC_BaseEvent *llvm_cbe_call50;
  unsigned int llvm_cbe_tmp52;
  struct l_class_OC_BaseEvent *llvm_cbe_call54;
  struct l_class_OC_BaseEvent *llvm_cbe_call56;
  struct l_class_OC_BaseEvent *llvm_cbe_call58;
  unsigned int llvm_cbe_tmp61;
  struct l_class_OC_BaseEvent *llvm_cbe_call63;
  struct l_class_OC_Uec_KD__KD_UecUeContextData *llvm_cbe_tmp65;
  unsigned int llvm_cbe_call67;
  struct l_class_OC_Uec_KD__KD_UecBaseData *llvm_cbe_call76;
  struct l_class_OC_Uec_KD__KD_UecBaseData * (**llvm_cbe_tmp__111) (struct l_class_OC_Uec_KD__KD_UecBaseData *, unsigned int );
  struct l_class_OC_Uec_KD__KD_UecBaseData * (*llvm_cbe_tmp__112) (struct l_class_OC_Uec_KD__KD_UecBaseData *, unsigned int );
  struct l_class_OC_Uec_KD__KD_UecBaseData *llvm_cbe_call79;
  unsigned int  (**llvm_cbe_tmp__113) (struct l_class_OC_Uec_KD__KD_UecBaseData *, struct l_class_OC_Uec_KD__KD_UecEvent *);
  unsigned int  (*llvm_cbe_tmp__114) (struct l_class_OC_Uec_KD__KD_UecBaseData *, struct l_class_OC_Uec_KD__KD_UecEvent *);
  unsigned int llvm_cbe_call82;
  unsigned int llvm_cbe_tmp83;
  struct l_class_OC_BaseEvent *llvm_cbe_call87;
  struct l_class_OC_BaseEvent *llvm_cbe_call89;
  struct l_class_OC_Uec_KD__KD_UecManagerBase *llvm_cbe_call91;
  unsigned int llvm_cbe_call93;
  struct l_class_OC_BaseEvent *llvm_cbe_call95;
  struct l_class_OC_BaseEvent *llvm_cbe_call97;
  struct l_class_OC_BaseEvent *llvm_cbe_call99;
  unsigned int llvm_cbe_tmp100;
  struct l_class_OC_BaseEvent *llvm_cbe_call102;
  unsigned int llvm_cbe_tmp104;
  unsigned int llvm_cbe_tmp105;
  unsigned int llvm_cbe_tmp111;
  unsigned int llvm_cbe_tmp116;
  unsigned int llvm_cbe_tmp__115;

#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_this_2e_addr) = llvm_cbe_this;
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_p_uecEvent_ptr_2e_addr) = llvm_cbe_p_uecEvent_ptr;
#line 0 "LLVM INTERNAL"
  llvm_cbe_this1 = *(&llvm_cbe_this_2e_addr);
#line 563 "UecDirectTransfer.cpp"
  _ZNSaIcEC1Ev((&llvm_cbe_agg_2e_tmp));
#line 563 "UecDirectTransfer.cpp"
  _ZNSsC1EPKcRKSaIcE((&llvm_cbe__currentFunction_), ((&__PRETTY_FUNCTION___OC__ZN3Uec17UecDirectTransfer32sendS1ApNasNonDeliveryIndicationEPNS_8UecEventE.array[((signed int )0u)])), (&llvm_cbe_agg_2e_tmp));
#line 563 "UecDirectTransfer.cpp"
  _ZNSaIcED1Ev((&llvm_cbe_agg_2e_tmp));
#line 563 "UecDirectTransfer.cpp"
  _ZN3Uec17UecDebugCallstackC1ERKSs((&llvm_cbe__callstackTmp_), (&llvm_cbe__currentFunction_));
#line 565 "UecDirectTransfer.cpp"
  *(&llvm_cbe_l_success) = 0u;
#line 567 "UecDirectTransfer.cpp"
  llvm_cbe_call = _ZNK3Uec14UecServiceBase16getUecDataAccessEv((((struct l_class_OC_Uec_KD__KD_UecServiceBase *)llvm_cbe_this1)));
#line 567 "UecDirectTransfer.cpp"
  llvm_cbe_tmp__109 = *(((struct l_class_OC_Uec_KD__KD_UecBaseData * (***) (struct l_class_OC_Uec_KD__KD_UecBaseData *, unsigned int ))llvm_cbe_call));
#line 567 "UecDirectTransfer.cpp"
  llvm_cbe_tmp__110 = *((&llvm_cbe_tmp__109[((signed long long )2ull)]));
#line 567 "UecDirectTransfer.cpp"
  llvm_cbe_call15 = llvm_cbe_tmp__110(llvm_cbe_call, 1u);
#line 567 "UecDirectTransfer.cpp"
  *(&llvm_cbe_l_uecUeContextDataWrite_ptr) = (((struct l_class_OC_Uec_KD__KD_UecUeContextData *)llvm_cbe_call15));
#line 569 "UecDirectTransfer.cpp"
  llvm_cbe_tmp = *(&llvm_cbe_l_uecUeContextDataWrite_ptr);
#line 569 "UecDirectTransfer.cpp"
  llvm_cbe_call17 = ((_ZNK3Uec16UecUeContextData16isValidMmeUeS1IdEv(llvm_cbe_tmp))&1);
#line 569 "UecDirectTransfer.cpp"
  if (llvm_cbe_call17) {    goto llvm_cbe_if_2e_end;  } else {    goto llvm_cbe_if_2e_then;  }


llvm_cbe_if_2e_then:
#line 571 "UecDirectTransfer.cpp"
  llvm_cbe_call19 = _ZN11DummyStreamlsIPKcEERS_T_((&mout), ((&_OC_str51.array[((signed int )0u)])));
#line 571 "UecDirectTransfer.cpp"
  llvm_cbe_call21 = _ZN11DummyStreamlsIPFRSt8ios_baseS2_EEERS_T_(llvm_cbe_call19, _ZSt3decRSt8ios_base);
#line 571 "UecDirectTransfer.cpp"
  llvm_cbe_tmp22 = *(&llvm_cbe_p_uecEvent_ptr_2e_addr);
#line 571 "UecDirectTransfer.cpp"
  llvm_cbe_call24 = _ZNK3Uec8UecEvent13getInstanceIdEv(llvm_cbe_tmp22);
#line 571 "UecDirectTransfer.cpp"
  llvm_cbe_call26 = _ZN11DummyStreamlsIjEERS_T_(llvm_cbe_call21, llvm_cbe_call24);
#line 571 "UecDirectTransfer.cpp"
  llvm_cbe_call28 = _ZN11DummyStreamlsIPKcEERS_T_(llvm_cbe_call26, ((&_OC_str52.array[((signed int )0u)])));
#line 571 "UecDirectTransfer.cpp"
  llvm_cbe_call30 = _ZN11DummyStreamlsIPFRSt8ios_baseS2_EEERS_T_(llvm_cbe_call28, _ZSt3decRSt8ios_base);
#line 571 "UecDirectTransfer.cpp"
  llvm_cbe_tmp31 = *(&llvm_cbe_l_uecUeContextDataWrite_ptr);
#line 571 "UecDirectTransfer.cpp"
  llvm_cbe_call33 = ((_ZNK3Uec16UecUeContextData16isValidMmeUeS1IdEv(llvm_cbe_tmp31))&1);
#line 571 "UecDirectTransfer.cpp"
  llvm_cbe_call35 = _ZN11DummyStreamlsIbEERS_T_(llvm_cbe_call30, llvm_cbe_call33);
#line 576 "UecDirectTransfer.cpp"
  *(&llvm_cbe_retval) = 0u;
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_cleanup_2e_dst106) = 1u;
#line 576 "UecDirectTransfer.cpp"
  goto llvm_cbe_cleanup110;

llvm_cbe_if_2e_end:
#line 583 "UecDirectTransfer.cpp"
  llvm_cbe_call39 = _ZN3Uec17UecDirectTransfer22setTupL3MessageRequestER16TUP_L3MessageReq(llvm_cbe_this1, (&llvm_cbe_l_payLoad));
#line 586 "UecDirectTransfer.cpp"
  llvm_cbe_tmp40 = *(&llvm_cbe_p_uecEvent_ptr_2e_addr);
#line 586 "UecDirectTransfer.cpp"
  llvm_cbe_call42 = _ZN3Uec17UecDirectTransfer31setS1ApNasNonDeliveryIndicationEPNS_8UecEventER31SS1apUFNASNonDeliveryIndication(llvm_cbe_this1, llvm_cbe_tmp40, (&llvm_cbe_l_asn1PayLoad));
#line 586 "UecDirectTransfer.cpp"
  *(&llvm_cbe_l_success) = llvm_cbe_call42;
#line 586 "UecDirectTransfer.cpp"
  llvm_cbe_tmp43 = *(&llvm_cbe_l_success);
#line 586 "UecDirectTransfer.cpp"
  if ((1u == llvm_cbe_tmp43)) {    goto llvm_cbe_if_2e_end46;  } else {    goto llvm_cbe_if_2e_then44;  }


llvm_cbe_if_2e_then44:
#line 589 "UecDirectTransfer.cpp"
  llvm_cbe_tmp45 = *(&llvm_cbe_l_success);
#line 589 "UecDirectTransfer.cpp"
  *(&llvm_cbe_retval) = llvm_cbe_tmp45;
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_cleanup_2e_dst106) = 2u;
#line 589 "UecDirectTransfer.cpp"
  goto llvm_cbe_cleanup110;

llvm_cbe_if_2e_end46:
#line 601 "UecDirectTransfer.cpp"
  llvm_cbe_call48 = _ZN11DummyStreamlsIPKcEERS_T_((&mout), ((&_OC_str51.array[((signed int )0u)])));
#line 601 "UecDirectTransfer.cpp"
  llvm_cbe_call50 = _ZN11DummyStreamlsIPFRSt8ios_baseS2_EEERS_T_(llvm_cbe_call48, _ZSt3decRSt8ios_base);
#line 601 "UecDirectTransfer.cpp"
  llvm_cbe_tmp52 = *((&llvm_cbe_l_payLoad.field3));
#line 601 "UecDirectTransfer.cpp"
  llvm_cbe_call54 = _ZN11DummyStreamlsIjEERS_T_(llvm_cbe_call50, llvm_cbe_tmp52);
#line 601 "UecDirectTransfer.cpp"
  llvm_cbe_call56 = _ZN11DummyStreamlsIPKcEERS_T_(llvm_cbe_call54, ((&_OC_str53.array[((signed int )0u)])));
#line 601 "UecDirectTransfer.cpp"
  llvm_cbe_call58 = _ZN11DummyStreamlsIPFRSt8ios_baseS2_EEERS_T_(llvm_cbe_call56, _ZSt3decRSt8ios_base);
#line 601 "UecDirectTransfer.cpp"
  llvm_cbe_tmp61 = *((&((&llvm_cbe_l_asn1PayLoad.field2))->field0));
#line 601 "UecDirectTransfer.cpp"
  llvm_cbe_call63 = _ZN11DummyStreamlsIjEERS_T_(llvm_cbe_call58, llvm_cbe_tmp61);
#line 605 "UecDirectTransfer.cpp"
  llvm_cbe_tmp65 = *(&llvm_cbe_l_uecUeContextDataWrite_ptr);
#line 605 "UecDirectTransfer.cpp"
  llvm_cbe_call67 = _ZNK3Uec16UecUeContextData9getCellIdEv(llvm_cbe_tmp65);
#line 605 "UecDirectTransfer.cpp"
  _ZN3Uec8UecEventC1EjNS_15EUecServiceTypeEjPvjiS2_j((&llvm_cbe_l_uecEvent), 10207u, 2u, llvm_cbe_call67, (((unsigned char *)(&llvm_cbe_l_payLoad))), 40u, 4u, (((unsigned char *)(&llvm_cbe_l_asn1PayLoad))), 48u);
#line 608 "UecDirectTransfer.cpp"
  llvm_cbe_call76 = _ZNK3Uec14UecServiceBase23getUecServiceControllerEv((((struct l_class_OC_Uec_KD__KD_UecServiceBase *)llvm_cbe_this1)));
#line 608 "UecDirectTransfer.cpp"
  llvm_cbe_tmp__111 = *(((struct l_class_OC_Uec_KD__KD_UecBaseData * (***) (struct l_class_OC_Uec_KD__KD_UecBaseData *, unsigned int ))llvm_cbe_call76));
#line 608 "UecDirectTransfer.cpp"
  llvm_cbe_tmp__112 = *((&(*llvm_cbe_tmp__111)));
#line 608 "UecDirectTransfer.cpp"
  llvm_cbe_call79 = llvm_cbe_tmp__112(llvm_cbe_call76, 3u);
#line 608 "UecDirectTransfer.cpp"
  llvm_cbe_tmp__113 = *(((unsigned int  (***) (struct l_class_OC_Uec_KD__KD_UecBaseData *, struct l_class_OC_Uec_KD__KD_UecEvent *))llvm_cbe_call79));
#line 608 "UecDirectTransfer.cpp"
  llvm_cbe_tmp__114 = *((&(*llvm_cbe_tmp__113)));
#line 608 "UecDirectTransfer.cpp"
  llvm_cbe_call82 = llvm_cbe_tmp__114(llvm_cbe_call79, (&llvm_cbe_l_uecEvent));
#line 608 "UecDirectTransfer.cpp"
  *(&llvm_cbe_l_success) = llvm_cbe_call82;
#line 608 "UecDirectTransfer.cpp"
  llvm_cbe_tmp83 = *(&llvm_cbe_l_success);
#line 608 "UecDirectTransfer.cpp"
  if ((1u == llvm_cbe_tmp83)) {    goto llvm_cbe_if_2e_end103;  } else {    goto llvm_cbe_if_2e_then85;  }


llvm_cbe_if_2e_then85:
#line 610 "UecDirectTransfer.cpp"
  llvm_cbe_call87 = _ZN11DummyStreamlsIPKcEERS_T_((&mout), ((&_OC_str51.array[((signed int )0u)])));
#line 610 "UecDirectTransfer.cpp"
  llvm_cbe_call89 = _ZN11DummyStreamlsIPFRSt8ios_baseS2_EEERS_T_(llvm_cbe_call87, _ZSt3decRSt8ios_base);
#line 610 "UecDirectTransfer.cpp"
  llvm_cbe_call91 = _ZNK3Uec14UecServiceBase13getUecManagerEv((((struct l_class_OC_Uec_KD__KD_UecServiceBase *)llvm_cbe_this1)));
#line 610 "UecDirectTransfer.cpp"
  llvm_cbe_call93 = _ZNK3Uec14UecManagerBase13getInstanceIdEv(llvm_cbe_call91);
#line 610 "UecDirectTransfer.cpp"
  llvm_cbe_call95 = _ZN11DummyStreamlsIjEERS_T_(llvm_cbe_call89, llvm_cbe_call93);
#line 610 "UecDirectTransfer.cpp"
  llvm_cbe_call97 = _ZN11DummyStreamlsIPKcEERS_T_(llvm_cbe_call95, ((&_OC_str48.array[((signed int )0u)])));
#line 610 "UecDirectTransfer.cpp"
  llvm_cbe_call99 = _ZN11DummyStreamlsIPFRSt8ios_baseS2_EEERS_T_(llvm_cbe_call97, _ZSt3decRSt8ios_base);
#line 610 "UecDirectTransfer.cpp"
  llvm_cbe_tmp100 = *(&llvm_cbe_l_success);
#line 610 "UecDirectTransfer.cpp"
  llvm_cbe_call102 = _ZN11DummyStreamlsIiEERS_T_(llvm_cbe_call99, llvm_cbe_tmp100);
#line 0 "LLVM INTERNAL"
  goto llvm_cbe_if_2e_end103;

llvm_cbe_if_2e_end103:
#line 615 "UecDirectTransfer.cpp"
  llvm_cbe_tmp104 = *(&llvm_cbe_l_success);
#line 615 "UecDirectTransfer.cpp"
  *(&llvm_cbe_retval) = llvm_cbe_tmp104;
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_cleanup_2e_dst) = 1u;
#line 616 "UecDirectTransfer.cpp"
  _ZN3Uec8UecEventD1Ev((&llvm_cbe_l_uecEvent));
#line 616 "UecDirectTransfer.cpp"
  llvm_cbe_tmp105 = *(&llvm_cbe_cleanup_2e_dst);
#line 0 "LLVM INTERNAL"
  if ((llvm_cbe_tmp105 == 1u)) {    goto llvm_cbe_cleanup_2e_pad;  } else {    goto llvm_cbe_cleanup_2e_end;  }


llvm_cbe_cleanup_2e_pad:
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_cleanup_2e_dst106) = 3u;
#line 0 "LLVM INTERNAL"
  goto llvm_cbe_cleanup110;

llvm_cbe_cleanup_2e_end:
#line 616 "UecDirectTransfer.cpp"
  *(&llvm_cbe_cleanup_2e_dst106) = 0u;
#line 616 "UecDirectTransfer.cpp"
  goto llvm_cbe_cleanup110;

llvm_cbe_cleanup110:
#line 616 "UecDirectTransfer.cpp"
  _ZN3Uec17UecDebugCallstackD1Ev((&llvm_cbe__callstackTmp_));
#line 616 "UecDirectTransfer.cpp"
  llvm_cbe_tmp111 = *(&llvm_cbe_cleanup_2e_dst106);
#line 616 "UecDirectTransfer.cpp"
  switch (llvm_cbe_tmp111) {
  default:
    goto llvm_cbe_cleanup_2e_end113;;
  case 1u:
    goto llvm_cbe_cleanup_2e_pad107;  case 2u:
    goto llvm_cbe_cleanup_2e_pad108;  case 3u:
    goto llvm_cbe_cleanup_2e_pad109;  }

llvm_cbe_cleanup_2e_end113:
#line 616 "UecDirectTransfer.cpp"
  *(&llvm_cbe_cleanup_2e_dst114) = 0u;
#line 616 "UecDirectTransfer.cpp"
  goto llvm_cbe_cleanup115;

llvm_cbe_cleanup115:
#line 616 "UecDirectTransfer.cpp"
  _ZNSsD1Ev((&llvm_cbe__currentFunction_));
#line 616 "UecDirectTransfer.cpp"
  llvm_cbe_tmp116 = *(&llvm_cbe_cleanup_2e_dst114);
#line 616 "UecDirectTransfer.cpp"
  llvm_cbe_tmp__115 = *(&llvm_cbe_retval);
#line 616 "UecDirectTransfer.cpp"
  return llvm_cbe_tmp__115;

llvm_cbe_cleanup_2e_pad107:
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_cleanup_2e_dst114) = 1u;
#line 0 "LLVM INTERNAL"
  goto llvm_cbe_cleanup115;

llvm_cbe_cleanup_2e_pad108:
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_cleanup_2e_dst114) = 1u;
#line 0 "LLVM INTERNAL"
  goto llvm_cbe_cleanup115;

llvm_cbe_cleanup_2e_pad109:
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_cleanup_2e_dst114) = 1u;
#line 0 "LLVM INTERNAL"
  goto llvm_cbe_cleanup115;
}


#line 0 "LLVM INTERNAL"
struct l_class_OC_BaseEvent *_ZN11DummyStreamlsIbEERS_T_(struct l_class_OC_BaseEvent *llvm_cbe_this, bool llvm_cbe_arg) {
  struct l_class_OC_BaseEvent *llvm_cbe_retval;    /* Address-exposed local */
  struct l_class_OC_BaseEvent *llvm_cbe_this_2e_addr;    /* Address-exposed local */
  unsigned char llvm_cbe_arg_2e_addr;    /* Address-exposed local */
  struct l_class_OC_BaseEvent *llvm_cbe_this1;
  struct l_class_OC_BaseEvent *llvm_cbe_tmp__116;

#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_this_2e_addr) = llvm_cbe_this;
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_arg_2e_addr) = (((unsigned char )(bool )llvm_cbe_arg));
#line 0 "LLVM INTERNAL"
  llvm_cbe_this1 = *(&llvm_cbe_this_2e_addr);
#line 30 "BaseTypes.hpp"
  *(&llvm_cbe_retval) = llvm_cbe_this1;
#line 31 "BaseTypes.hpp"
  llvm_cbe_tmp__116 = *(&llvm_cbe_retval);
#line 31 "BaseTypes.hpp"
  return llvm_cbe_tmp__116;
}


#line 0 "LLVM INTERNAL"
unsigned int _ZN3Uec17UecDirectTransfer31setS1ApNasNonDeliveryIndicationEPNS_8UecEventER31SS1apUFNASNonDeliveryIndication(struct l_class_OC_Uec_KD__KD_UecDirectTransfer *llvm_cbe_this, struct l_class_OC_Uec_KD__KD_UecEvent *llvm_cbe_p_uecEvent_ptr, struct l_struct_OC_SS1apUFNASNonDeliveryIndication *llvm_cbe_p_asn1PayLoad) {
  unsigned int llvm_cbe_retval;    /* Address-exposed local */
  struct l_class_OC_Uec_KD__KD_UecDirectTransfer *llvm_cbe_this_2e_addr;    /* Address-exposed local */
  struct l_class_OC_Uec_KD__KD_UecEvent *llvm_cbe_p_uecEvent_ptr_2e_addr;    /* Address-exposed local */
  struct l_struct_OC_SS1apUFNASNonDeliveryIndication *llvm_cbe_p_asn1PayLoad_2e_addr;    /* Address-exposed local */
  struct l_class_OC_std_KD__KD_basic_string llvm_cbe__currentFunction_;    /* Address-exposed local */
  struct l_class_OC_BaseEvent llvm_cbe_agg_2e_tmp;    /* Address-exposed local */
  struct l_class_OC_Uec_KD__KD_UecDebugCallstack llvm_cbe__callstackTmp_;    /* Address-exposed local */
  struct l_class_OC_Uec_KD__KD_UecUeContextData *llvm_cbe_l_uecUeContextDataWrite_ptr;    /* Address-exposed local */
  struct l_struct_OC_SErrcDLInformationTransfer *llvm_cbe_l_asn1PayLoad_ptr;    /* Address-exposed local */
  struct l_struct_OC_SS1apUFDownlinkNASTransport *llvm_cbe_l_asn1PayLoad_ptr70;    /* Address-exposed local */
  unsigned int llvm_cbe_cleanup_2e_dst;    /* Address-exposed local */
  unsigned int llvm_cbe_cleanup_2e_dst155;    /* Address-exposed local */
  struct l_class_OC_Uec_KD__KD_UecDirectTransfer *llvm_cbe_this1;
  struct l_class_OC_Uec_KD__KD_UecBaseData *llvm_cbe_call;
  struct l_class_OC_Uec_KD__KD_UecBaseData * (**llvm_cbe_tmp__117) (struct l_class_OC_Uec_KD__KD_UecBaseData *, unsigned int );
  struct l_class_OC_Uec_KD__KD_UecBaseData * (*llvm_cbe_tmp__118) (struct l_class_OC_Uec_KD__KD_UecBaseData *, unsigned int );
  struct l_class_OC_Uec_KD__KD_UecBaseData *llvm_cbe_call15;
  struct l_class_OC_Uec_KD__KD_UecEvent *llvm_cbe_tmp;
  unsigned int llvm_cbe_call17;
  struct l_class_OC_Uec_KD__KD_UecEvent *llvm_cbe_tmp19;
  unsigned char *llvm_cbe_call21;
  struct l_struct_OC_SErrcDLInformationTransfer *llvm_cbe_tmp22;
  unsigned int llvm_cbe_tmp32;
  unsigned int llvm_cbe_tmp37;
  unsigned char *llvm_cbe_call39;
  unsigned char *llvm_cbe_tmp44;
  struct l_struct_OC_SErrcDLInformationTransfer *llvm_cbe_tmp45;
  unsigned char *llvm_cbe_tmp55;
  struct l_struct_OC_SErrcDLInformationTransfer *llvm_cbe_tmp56;
  unsigned int llvm_cbe_tmp66;
  unsigned char *llvm_cbe_tmp__119;
  struct l_class_OC_Uec_KD__KD_UecEvent *llvm_cbe_tmp71;
  unsigned char *llvm_cbe_call73;
  struct l_struct_OC_SS1apUFDownlinkNASTransport *llvm_cbe_tmp74;
  unsigned int llvm_cbe_tmp77;
  unsigned int llvm_cbe_tmp82;
  unsigned char *llvm_cbe_call85;
  unsigned char *llvm_cbe_tmp90;
  struct l_struct_OC_SS1apUFDownlinkNASTransport *llvm_cbe_tmp91;
  unsigned char *llvm_cbe_tmp94;
  struct l_struct_OC_SS1apUFDownlinkNASTransport *llvm_cbe_tmp95;
  unsigned int llvm_cbe_tmp98;
  unsigned char *llvm_cbe_tmp__120;
  struct l_class_OC_BaseEvent *llvm_cbe_call101;
  struct l_class_OC_BaseEvent *llvm_cbe_call103;
  struct l_class_OC_Uec_KD__KD_UecEvent *llvm_cbe_tmp104;
  unsigned int llvm_cbe_call106;
  struct l_class_OC_BaseEvent *llvm_cbe_call108;
  struct l_class_OC_BaseEvent *llvm_cbe_call110;
  struct l_class_OC_BaseEvent *llvm_cbe_call112;
  struct l_class_OC_Uec_KD__KD_UecEvent *llvm_cbe_tmp113;
  unsigned int llvm_cbe_call115;
  struct l_class_OC_BaseEvent *llvm_cbe_call117;
  struct l_struct_OC_SS1apUFNASNonDeliveryIndication *llvm_cbe_tmp118;
  unsigned char *llvm_cbe_tmp__121;
  struct l_class_OC_Uec_KD__KD_UecUeContextData *llvm_cbe_tmp120;
  unsigned int llvm_cbe_call122;
  struct l_struct_OC_SS1apUFNASNonDeliveryIndication *llvm_cbe_tmp123;
  struct l_class_OC_Uec_KD__KD_UecUeContextData *llvm_cbe_tmp125;
  unsigned int llvm_cbe_call127;
  struct l_struct_OC_SS1apUFNASNonDeliveryIndication *llvm_cbe_tmp129;
  struct l_struct_OC_SS1apUFNASNonDeliveryIndication *llvm_cbe_tmp131;
  unsigned char *llvm_cbe_tmp__122;
  struct l_class_OC_Uec_KD__KD_UecUeContextData *llvm_cbe_tmp136;
  unsigned int llvm_cbe_call138;
  struct l_struct_OC_SS1apUFNASNonDeliveryIndication *llvm_cbe_tmp139;
  unsigned int *llvm_cbe_tmp141;
  struct l_struct_OC_SS1apUFNASNonDeliveryIndication *llvm_cbe_tmp142;
  struct l_struct_OC_SS1apUFNASNonDeliveryIndication *llvm_cbe_tmp149;
  unsigned int llvm_cbe_tmp154;
  unsigned int llvm_cbe_tmp157;
  unsigned int llvm_cbe_tmp__123;

#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_this_2e_addr) = llvm_cbe_this;
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_p_uecEvent_ptr_2e_addr) = llvm_cbe_p_uecEvent_ptr;
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_p_asn1PayLoad_2e_addr) = llvm_cbe_p_asn1PayLoad;
#line 0 "LLVM INTERNAL"
  llvm_cbe_this1 = *(&llvm_cbe_this_2e_addr);
#line 826 "UecDirectTransfer.cpp"
  _ZNSaIcEC1Ev((&llvm_cbe_agg_2e_tmp));
#line 826 "UecDirectTransfer.cpp"
  _ZNSsC1EPKcRKSaIcE((&llvm_cbe__currentFunction_), ((&__PRETTY_FUNCTION___OC__ZN3Uec17UecDirectTransfer31setS1ApNasNonDeliveryIndicationEPNS_8UecEventER31SS1apUFNASNonDeliveryIndication.array[((signed int )0u)])), (&llvm_cbe_agg_2e_tmp));
#line 826 "UecDirectTransfer.cpp"
  _ZNSaIcED1Ev((&llvm_cbe_agg_2e_tmp));
#line 826 "UecDirectTransfer.cpp"
  _ZN3Uec17UecDebugCallstackC1ERKSs((&llvm_cbe__callstackTmp_), (&llvm_cbe__currentFunction_));
#line 829 "UecDirectTransfer.cpp"
  llvm_cbe_call = _ZNK3Uec14UecServiceBase16getUecDataAccessEv((((struct l_class_OC_Uec_KD__KD_UecServiceBase *)llvm_cbe_this1)));
#line 829 "UecDirectTransfer.cpp"
  llvm_cbe_tmp__117 = *(((struct l_class_OC_Uec_KD__KD_UecBaseData * (***) (struct l_class_OC_Uec_KD__KD_UecBaseData *, unsigned int ))llvm_cbe_call));
#line 829 "UecDirectTransfer.cpp"
  llvm_cbe_tmp__118 = *((&llvm_cbe_tmp__117[((signed long long )2ull)]));
#line 829 "UecDirectTransfer.cpp"
  llvm_cbe_call15 = llvm_cbe_tmp__118(llvm_cbe_call, 1u);
#line 829 "UecDirectTransfer.cpp"
  *(&llvm_cbe_l_uecUeContextDataWrite_ptr) = (((struct l_class_OC_Uec_KD__KD_UecUeContextData *)llvm_cbe_call15));
#line 831 "UecDirectTransfer.cpp"
  llvm_cbe_tmp = *(&llvm_cbe_p_uecEvent_ptr_2e_addr);
#line 831 "UecDirectTransfer.cpp"
  llvm_cbe_call17 = _ZNK3Uec8UecEvent16getAsn1PayLoadIdEv(llvm_cbe_tmp);
#line 831 "UecDirectTransfer.cpp"
  switch (llvm_cbe_call17) {
  default:
    goto llvm_cbe_sw_2e_default;;
  case 2u:
    goto llvm_cbe_sw_2e_bb;    break;
  case 1u:
    goto llvm_cbe_sw_2e_bb68;  }

llvm_cbe_sw_2e_bb:
#line 836 "UecDirectTransfer.cpp"
  llvm_cbe_tmp19 = *(&llvm_cbe_p_uecEvent_ptr_2e_addr);
#line 836 "UecDirectTransfer.cpp"
  llvm_cbe_call21 = _ZNK3Uec8UecEvent14getAsn1PayLoadEv(llvm_cbe_tmp19);
#line 836 "UecDirectTransfer.cpp"
  *(&llvm_cbe_l_asn1PayLoad_ptr) = (((struct l_struct_OC_SErrcDLInformationTransfer *)llvm_cbe_call21));
#line 839 "UecDirectTransfer.cpp"
  llvm_cbe_tmp22 = *(&llvm_cbe_l_asn1PayLoad_ptr);
#line 839 "UecDirectTransfer.cpp"
  llvm_cbe_tmp32 = *((&((&((&((&((&((&((&((&((&llvm_cbe_tmp22->field4))->field1))->field0))->field1))->field0))->field0))->field1))->field0))->field0));
#line 839 "UecDirectTransfer.cpp"
  *((&((&llvm_cbe_this1->field2))->field0)) = llvm_cbe_tmp32;
#line 840 "UecDirectTransfer.cpp"
  llvm_cbe_tmp37 = *((&((&llvm_cbe_this1->field2))->field0));
#line 840 "UecDirectTransfer.cpp"
  llvm_cbe_call39 = _Znam((((unsigned long long )(((unsigned long long )(((unsigned long long )(unsigned int )llvm_cbe_tmp37))) * ((unsigned long long )1ull)))));
#line 840 "UecDirectTransfer.cpp"
  *((&((&llvm_cbe_this1->field2))->field1)) = llvm_cbe_call39;
#line 841 "UecDirectTransfer.cpp"
  llvm_cbe_tmp44 = *((&((&llvm_cbe_this1->field2))->field1));
#line 841 "UecDirectTransfer.cpp"
  llvm_cbe_tmp45 = *(&llvm_cbe_l_asn1PayLoad_ptr);
#line 841 "UecDirectTransfer.cpp"
  llvm_cbe_tmp55 = *((&((&((&((&((&((&((&((&((&llvm_cbe_tmp45->field4))->field1))->field0))->field1))->field0))->field0))->field1))->field0))->field1));
#line 841 "UecDirectTransfer.cpp"
  llvm_cbe_tmp56 = *(&llvm_cbe_l_asn1PayLoad_ptr);
#line 841 "UecDirectTransfer.cpp"
  llvm_cbe_tmp66 = *((&((&((&((&((&((&((&((&((&llvm_cbe_tmp56->field4))->field1))->field0))->field1))->field0))->field0))->field1))->field0))->field0));
#line 0 "LLVM INTERNAL"
  llvm_cbe_tmp__119 = memcpy(llvm_cbe_tmp44, llvm_cbe_tmp55, (((unsigned long long )(unsigned int )llvm_cbe_tmp66)));
#line 843 "UecDirectTransfer.cpp"
  goto llvm_cbe_sw_2e_epilog;

llvm_cbe_sw_2e_bb68:
#line 848 "UecDirectTransfer.cpp"
  llvm_cbe_tmp71 = *(&llvm_cbe_p_uecEvent_ptr_2e_addr);
#line 848 "UecDirectTransfer.cpp"
  llvm_cbe_call73 = _ZNK3Uec8UecEvent14getAsn1PayLoadEv(llvm_cbe_tmp71);
#line 848 "UecDirectTransfer.cpp"
  *(&llvm_cbe_l_asn1PayLoad_ptr70) = (((struct l_struct_OC_SS1apUFDownlinkNASTransport *)llvm_cbe_call73));
#line 851 "UecDirectTransfer.cpp"
  llvm_cbe_tmp74 = *(&llvm_cbe_l_asn1PayLoad_ptr70);
#line 851 "UecDirectTransfer.cpp"
  llvm_cbe_tmp77 = *((&((&llvm_cbe_tmp74->field2))->field0));
#line 851 "UecDirectTransfer.cpp"
  *((&((&llvm_cbe_this1->field2))->field0)) = llvm_cbe_tmp77;
#line 852 "UecDirectTransfer.cpp"
  llvm_cbe_tmp82 = *((&((&llvm_cbe_this1->field2))->field0));
#line 852 "UecDirectTransfer.cpp"
  llvm_cbe_call85 = _Znam((((unsigned long long )(((unsigned long long )(((unsigned long long )(unsigned int )llvm_cbe_tmp82))) * ((unsigned long long )1ull)))));
#line 852 "UecDirectTransfer.cpp"
  *((&((&llvm_cbe_this1->field2))->field1)) = llvm_cbe_call85;
#line 853 "UecDirectTransfer.cpp"
  llvm_cbe_tmp90 = *((&((&llvm_cbe_this1->field2))->field1));
#line 853 "UecDirectTransfer.cpp"
  llvm_cbe_tmp91 = *(&llvm_cbe_l_asn1PayLoad_ptr70);
#line 853 "UecDirectTransfer.cpp"
  llvm_cbe_tmp94 = *((&((&llvm_cbe_tmp91->field2))->field1));
#line 853 "UecDirectTransfer.cpp"
  llvm_cbe_tmp95 = *(&llvm_cbe_l_asn1PayLoad_ptr70);
#line 853 "UecDirectTransfer.cpp"
  llvm_cbe_tmp98 = *((&((&llvm_cbe_tmp95->field2))->field0));
#line 0 "LLVM INTERNAL"
  llvm_cbe_tmp__120 = memcpy(llvm_cbe_tmp90, llvm_cbe_tmp94, (((unsigned long long )(unsigned int )llvm_cbe_tmp98)));
#line 855 "UecDirectTransfer.cpp"
  goto llvm_cbe_sw_2e_epilog;

llvm_cbe_sw_2e_default:
#line 861 "UecDirectTransfer.cpp"
  llvm_cbe_call101 = _ZN11DummyStreamlsIPKcEERS_T_((&mout), ((&_OC_str62.array[((signed int )0u)])));
#line 861 "UecDirectTransfer.cpp"
  llvm_cbe_call103 = _ZN11DummyStreamlsIPFRSt8ios_baseS2_EEERS_T_(llvm_cbe_call101, _ZSt3decRSt8ios_base);
#line 861 "UecDirectTransfer.cpp"
  llvm_cbe_tmp104 = *(&llvm_cbe_p_uecEvent_ptr_2e_addr);
#line 861 "UecDirectTransfer.cpp"
  llvm_cbe_call106 = _ZNK3Uec8UecEvent13getInstanceIdEv(llvm_cbe_tmp104);
#line 861 "UecDirectTransfer.cpp"
  llvm_cbe_call108 = _ZN11DummyStreamlsIjEERS_T_(llvm_cbe_call103, llvm_cbe_call106);
#line 861 "UecDirectTransfer.cpp"
  llvm_cbe_call110 = _ZN11DummyStreamlsIPKcEERS_T_(llvm_cbe_call108, ((&_OC_str63.array[((signed int )0u)])));
#line 861 "UecDirectTransfer.cpp"
  llvm_cbe_call112 = _ZN11DummyStreamlsIPFRSt8ios_baseS2_EEERS_T_(llvm_cbe_call110, _ZSt3decRSt8ios_base);
#line 861 "UecDirectTransfer.cpp"
  llvm_cbe_tmp113 = *(&llvm_cbe_p_uecEvent_ptr_2e_addr);
#line 861 "UecDirectTransfer.cpp"
  llvm_cbe_call115 = _ZNK3Uec8UecEvent16getAsn1PayLoadIdEv(llvm_cbe_tmp113);
#line 861 "UecDirectTransfer.cpp"
  llvm_cbe_call117 = _ZN11DummyStreamlsIiEERS_T_(llvm_cbe_call112, llvm_cbe_call115);
#line 864 "UecDirectTransfer.cpp"
  *(&llvm_cbe_retval) = 0u;
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_cleanup_2e_dst) = 2u;
#line 864 "UecDirectTransfer.cpp"
  goto llvm_cbe_cleanup;

llvm_cbe_sw_2e_epilog:
#line 868 "UecDirectTransfer.cpp"
  llvm_cbe_tmp118 = *(&llvm_cbe_p_asn1PayLoad_2e_addr);
#line 0 "LLVM INTERNAL"
  llvm_cbe_tmp__121 = memset((((unsigned char *)llvm_cbe_tmp118)), 0u, 48ull);
#line 871 "UecDirectTransfer.cpp"
  llvm_cbe_tmp120 = *(&llvm_cbe_l_uecUeContextDataWrite_ptr);
#line 871 "UecDirectTransfer.cpp"
  llvm_cbe_call122 = _ZNK3Uec16UecUeContextData12getEnbUeS1IdEv(llvm_cbe_tmp120);
#line 871 "UecDirectTransfer.cpp"
  llvm_cbe_tmp123 = *(&llvm_cbe_p_asn1PayLoad_2e_addr);
#line 871 "UecDirectTransfer.cpp"
  *((&llvm_cbe_tmp123->field1)) = llvm_cbe_call122;
#line 872 "UecDirectTransfer.cpp"
  llvm_cbe_tmp125 = *(&llvm_cbe_l_uecUeContextDataWrite_ptr);
#line 872 "UecDirectTransfer.cpp"
  llvm_cbe_call127 = _ZNK3Uec16UecUeContextData12getMmeUeS1IdEv(llvm_cbe_tmp125);
#line 872 "UecDirectTransfer.cpp"
  llvm_cbe_tmp129 = *(&llvm_cbe_p_asn1PayLoad_2e_addr);
#line 872 "UecDirectTransfer.cpp"
  *((&llvm_cbe_tmp129->field0)) = (((unsigned long long )(unsigned int )llvm_cbe_call127));
#line 875 "UecDirectTransfer.cpp"
  llvm_cbe_tmp131 = *(&llvm_cbe_p_asn1PayLoad_2e_addr);
#line 0 "LLVM INTERNAL"
  llvm_cbe_tmp__122 = memcpy((((unsigned char *)((&llvm_cbe_tmp131->field2)))), (((unsigned char *)((&llvm_cbe_this1->field2)))), 16ull);
#line 878 "UecDirectTransfer.cpp"
  llvm_cbe_tmp136 = *(&llvm_cbe_l_uecUeContextDataWrite_ptr);
#line 878 "UecDirectTransfer.cpp"
  llvm_cbe_call138 = _ZNK3Uec16UecUeContextData14getNasDeliveryEv(llvm_cbe_tmp136);
#line 881 "UecDirectTransfer.cpp"
  llvm_cbe_tmp139 = *(&llvm_cbe_p_asn1PayLoad_2e_addr);
#line 881 "UecDirectTransfer.cpp"
  llvm_cbe_tmp141 = (&((&llvm_cbe_tmp139->field3))->field0);
#line 878 "UecDirectTransfer.cpp"
  if ((1u == llvm_cbe_call138)) {    goto llvm_cbe_if_2e_then;  } else {    goto llvm_cbe_if_2e_else;  }


llvm_cbe_if_2e_then:
#line 881 "UecDirectTransfer.cpp"
  *llvm_cbe_tmp141 = 0u;
#line 882 "UecDirectTransfer.cpp"
  llvm_cbe_tmp142 = *(&llvm_cbe_p_asn1PayLoad_2e_addr);
#line 882 "UecDirectTransfer.cpp"
  *((&((&((&llvm_cbe_tmp142->field3))->field1))->field0)) = 35u;
#line 883 "UecDirectTransfer.cpp"
  goto llvm_cbe_if_2e_end;

llvm_cbe_if_2e_else:
#line 886 "UecDirectTransfer.cpp"
  *llvm_cbe_tmp141 = 4u;
#line 887 "UecDirectTransfer.cpp"
  llvm_cbe_tmp149 = *(&llvm_cbe_p_asn1PayLoad_2e_addr);
#line 887 "UecDirectTransfer.cpp"
  *((&((&((&llvm_cbe_tmp149->field3))->field1))->field0)) = 4u;
#line 888 "UecDirectTransfer.cpp"
  goto llvm_cbe_if_2e_end;

llvm_cbe_if_2e_end:
#line 890 "UecDirectTransfer.cpp"
  *(&llvm_cbe_retval) = 1u;
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_cleanup_2e_dst) = 1u;
#line 890 "UecDirectTransfer.cpp"
  goto llvm_cbe_cleanup;

llvm_cbe_cleanup_2e_pad:
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_cleanup_2e_dst155) = 1u;
#line 0 "LLVM INTERNAL"
  goto llvm_cbe_cleanup156;

llvm_cbe_cleanup_2e_pad153:
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_cleanup_2e_dst155) = 1u;
#line 0 "LLVM INTERNAL"
  goto llvm_cbe_cleanup156;

llvm_cbe_cleanup:
#line 891 "UecDirectTransfer.cpp"
  _ZN3Uec17UecDebugCallstackD1Ev((&llvm_cbe__callstackTmp_));
#line 891 "UecDirectTransfer.cpp"
  llvm_cbe_tmp154 = *(&llvm_cbe_cleanup_2e_dst);
#line 891 "UecDirectTransfer.cpp"
  switch (llvm_cbe_tmp154) {
  default:
    goto llvm_cbe_cleanup_2e_end;;
  case 1u:
    goto llvm_cbe_cleanup_2e_pad;  case 2u:
    goto llvm_cbe_cleanup_2e_pad153;  }

llvm_cbe_cleanup_2e_end:
#line 891 "UecDirectTransfer.cpp"
  *(&llvm_cbe_cleanup_2e_dst155) = 0u;
#line 891 "UecDirectTransfer.cpp"
  goto llvm_cbe_cleanup156;

llvm_cbe_cleanup156:
#line 891 "UecDirectTransfer.cpp"
  _ZNSsD1Ev((&llvm_cbe__currentFunction_));
#line 891 "UecDirectTransfer.cpp"
  llvm_cbe_tmp157 = *(&llvm_cbe_cleanup_2e_dst155);
#line 891 "UecDirectTransfer.cpp"
  llvm_cbe_tmp__123 = *(&llvm_cbe_retval);
#line 891 "UecDirectTransfer.cpp"
  return llvm_cbe_tmp__123;
}


#line 0 "LLVM INTERNAL"
unsigned int _ZN3Uec17UecDirectTransfer22sendS1ApUlNasTransportEPNS_8UecEventE(struct l_class_OC_Uec_KD__KD_UecDirectTransfer *llvm_cbe_this, struct l_class_OC_Uec_KD__KD_UecEvent *llvm_cbe_p_uecEvent_ptr) {
  unsigned int llvm_cbe_retval;    /* Address-exposed local */
  struct l_class_OC_Uec_KD__KD_UecDirectTransfer *llvm_cbe_this_2e_addr;    /* Address-exposed local */
  struct l_class_OC_Uec_KD__KD_UecEvent *llvm_cbe_p_uecEvent_ptr_2e_addr;    /* Address-exposed local */
  struct l_class_OC_std_KD__KD_basic_string llvm_cbe__currentFunction_;    /* Address-exposed local */
  struct l_class_OC_BaseEvent llvm_cbe_agg_2e_tmp;    /* Address-exposed local */
  struct l_class_OC_Uec_KD__KD_UecDebugCallstack llvm_cbe__callstackTmp_;    /* Address-exposed local */
  unsigned int llvm_cbe_l_success;    /* Address-exposed local */
  struct l_class_OC_Uec_KD__KD_UecUeContextData *llvm_cbe_l_uecUeContextDataWrite_ptr;    /* Address-exposed local */
  struct l_struct_OC_SErrcULInformationTransfer *llvm_cbe_l_asn1PayLoad_ptr;    /* Address-exposed local */
  struct l_struct_OC_SAsnDynstr llvm_cbe_l_nasPdu;    /* Address-exposed local */
  struct l_struct_OC_TUP_L3MessageReq llvm_cbe_l_payLoad;    /* Address-exposed local */
  struct l_struct_OC_SS1apUFUplinkNASTransport llvm_cbe_l_asn1PayLoad;    /* Address-exposed local */
  struct l_class_OC_Uec_KD__KD_UecEvent llvm_cbe_l_uecEvent;    /* Address-exposed local */
  unsigned int llvm_cbe_cleanup_2e_dst;    /* Address-exposed local */
  unsigned int llvm_cbe_cleanup_2e_dst161;    /* Address-exposed local */
  unsigned int llvm_cbe_cleanup_2e_dst172;    /* Address-exposed local */
  struct l_class_OC_Uec_KD__KD_UecDirectTransfer *llvm_cbe_this1;
  struct l_class_OC_Uec_KD__KD_UecBaseData *llvm_cbe_call;
  struct l_class_OC_Uec_KD__KD_UecBaseData * (**llvm_cbe_tmp__124) (struct l_class_OC_Uec_KD__KD_UecBaseData *, unsigned int );
  struct l_class_OC_Uec_KD__KD_UecBaseData * (*llvm_cbe_tmp__125) (struct l_class_OC_Uec_KD__KD_UecBaseData *, unsigned int );
  struct l_class_OC_Uec_KD__KD_UecBaseData *llvm_cbe_call15;
  struct l_class_OC_Uec_KD__KD_UecUeContextData *llvm_cbe_tmp;
  bool llvm_cbe_call17;
  struct l_class_OC_Uec_KD__KD_UecEvent *llvm_cbe_tmp19;
  unsigned char *llvm_cbe_call21;
  struct l_struct_OC_SErrcULInformationTransfer *llvm_cbe_tmp22;
  unsigned int llvm_cbe_tmp30;
  struct l_class_OC_BaseEvent *llvm_cbe_call33;
  struct l_class_OC_BaseEvent *llvm_cbe_call35;
  struct l_class_OC_Uec_KD__KD_UecEvent *llvm_cbe_tmp36;
  unsigned int llvm_cbe_call38;
  struct l_class_OC_BaseEvent *llvm_cbe_call40;
  struct l_class_OC_BaseEvent *llvm_cbe_call42;
  struct l_class_OC_BaseEvent *llvm_cbe_call53;
  struct l_struct_OC_SErrcULInformationTransfer *llvm_cbe_tmp55;
  unsigned int llvm_cbe_tmp65;
  struct l_struct_OC_SErrcULInformationTransfer *llvm_cbe_tmp67;
  unsigned char *llvm_cbe_tmp77;
  struct l_class_OC_Uec_KD__KD_UecUeContextData *llvm_cbe_tmp79;
  unsigned int llvm_cbe_call85;
  struct l_class_OC_Uec_KD__KD_UecEvent *llvm_cbe_tmp86;
  unsigned int llvm_cbe_call88;
  unsigned int llvm_cbe_tmp89;
  unsigned int llvm_cbe_tmp92;
  struct l_class_OC_BaseEvent *llvm_cbe_call95;
  struct l_class_OC_BaseEvent *llvm_cbe_call97;
  unsigned int llvm_cbe_tmp99;
  struct l_class_OC_BaseEvent *llvm_cbe_call101;
  struct l_class_OC_BaseEvent *llvm_cbe_call103;
  struct l_class_OC_BaseEvent *llvm_cbe_call105;
  unsigned int llvm_cbe_tmp108;
  struct l_class_OC_BaseEvent *llvm_cbe_call110;
  struct l_class_OC_Uec_KD__KD_UecUeContextData *llvm_cbe_tmp112;
  unsigned int llvm_cbe_call114;
  struct l_class_OC_Uec_KD__KD_UecUeContextData *llvm_cbe_tmp122;
  bool llvm_cbe_call124;
  struct l_class_OC_Uec_KD__KD_UecBaseData *llvm_cbe_call127;
  struct l_class_OC_Uec_KD__KD_UecBaseData * (**llvm_cbe_tmp__126) (struct l_class_OC_Uec_KD__KD_UecBaseData *, unsigned int );
  struct l_class_OC_Uec_KD__KD_UecBaseData * (*llvm_cbe_tmp__127) (struct l_class_OC_Uec_KD__KD_UecBaseData *, unsigned int );
  struct l_class_OC_Uec_KD__KD_UecBaseData *llvm_cbe_call130;
  unsigned int  (**llvm_cbe_tmp__128) (struct l_class_OC_Uec_KD__KD_UecBaseData *, struct l_class_OC_Uec_KD__KD_UecEvent *);
  unsigned int  (*llvm_cbe_tmp__129) (struct l_class_OC_Uec_KD__KD_UecBaseData *, struct l_class_OC_Uec_KD__KD_UecEvent *);
  unsigned int llvm_cbe_call133;
  unsigned int llvm_cbe_tmp134;
  struct l_class_OC_BaseEvent *llvm_cbe_call138;
  struct l_class_OC_BaseEvent *llvm_cbe_call140;
  struct l_class_OC_Uec_KD__KD_UecManagerBase *llvm_cbe_call142;
  unsigned int llvm_cbe_call144;
  struct l_class_OC_BaseEvent *llvm_cbe_call146;
  struct l_class_OC_BaseEvent *llvm_cbe_call148;
  struct l_class_OC_BaseEvent *llvm_cbe_call150;
  unsigned int llvm_cbe_tmp151;
  struct l_class_OC_BaseEvent *llvm_cbe_call153;
  struct l_class_OC_BaseEvent *llvm_cbe_call156;
  unsigned int llvm_cbe_tmp160;
  unsigned int llvm_cbe_tmp169;
  unsigned int llvm_cbe_tmp174;
  unsigned int llvm_cbe_tmp__130;

#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_this_2e_addr) = llvm_cbe_this;
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_p_uecEvent_ptr_2e_addr) = llvm_cbe_p_uecEvent_ptr;
#line 0 "LLVM INTERNAL"
  llvm_cbe_this1 = *(&llvm_cbe_this_2e_addr);
#line 624 "UecDirectTransfer.cpp"
  _ZNSaIcEC1Ev((&llvm_cbe_agg_2e_tmp));
#line 624 "UecDirectTransfer.cpp"
  _ZNSsC1EPKcRKSaIcE((&llvm_cbe__currentFunction_), ((&__PRETTY_FUNCTION___OC__ZN3Uec17UecDirectTransfer22sendS1ApUlNasTransportEPNS_8UecEventE.array[((signed int )0u)])), (&llvm_cbe_agg_2e_tmp));
#line 624 "UecDirectTransfer.cpp"
  _ZNSaIcED1Ev((&llvm_cbe_agg_2e_tmp));
#line 624 "UecDirectTransfer.cpp"
  _ZN3Uec17UecDebugCallstackC1ERKSs((&llvm_cbe__callstackTmp_), (&llvm_cbe__currentFunction_));
#line 626 "UecDirectTransfer.cpp"
  *(&llvm_cbe_l_success) = 0u;
#line 629 "UecDirectTransfer.cpp"
  llvm_cbe_call = _ZNK3Uec14UecServiceBase16getUecDataAccessEv((((struct l_class_OC_Uec_KD__KD_UecServiceBase *)llvm_cbe_this1)));
#line 629 "UecDirectTransfer.cpp"
  llvm_cbe_tmp__124 = *(((struct l_class_OC_Uec_KD__KD_UecBaseData * (***) (struct l_class_OC_Uec_KD__KD_UecBaseData *, unsigned int ))llvm_cbe_call));
#line 629 "UecDirectTransfer.cpp"
  llvm_cbe_tmp__125 = *((&llvm_cbe_tmp__124[((signed long long )2ull)]));
#line 629 "UecDirectTransfer.cpp"
  llvm_cbe_call15 = llvm_cbe_tmp__125(llvm_cbe_call, 1u);
#line 629 "UecDirectTransfer.cpp"
  *(&llvm_cbe_l_uecUeContextDataWrite_ptr) = (((struct l_class_OC_Uec_KD__KD_UecUeContextData *)llvm_cbe_call15));
#line 631 "UecDirectTransfer.cpp"
  llvm_cbe_tmp = *(&llvm_cbe_l_uecUeContextDataWrite_ptr);
#line 631 "UecDirectTransfer.cpp"
  llvm_cbe_call17 = ((_ZNK3Uec16UecUeContextData16isValidMmeUeS1IdEv(llvm_cbe_tmp))&1);
#line 631 "UecDirectTransfer.cpp"
  if (llvm_cbe_call17) {    goto llvm_cbe_if_2e_end81;  } else {    goto llvm_cbe_if_2e_then;  }


llvm_cbe_if_2e_then:
#line 633 "UecDirectTransfer.cpp"
  llvm_cbe_tmp19 = *(&llvm_cbe_p_uecEvent_ptr_2e_addr);
#line 633 "UecDirectTransfer.cpp"
  llvm_cbe_call21 = _ZNK3Uec8UecEvent14getAsn1PayLoadEv(llvm_cbe_tmp19);
#line 633 "UecDirectTransfer.cpp"
  *(&llvm_cbe_l_asn1PayLoad_ptr) = (((struct l_struct_OC_SErrcULInformationTransfer *)llvm_cbe_call21));
#line 635 "UecDirectTransfer.cpp"
  llvm_cbe_tmp22 = *(&llvm_cbe_l_asn1PayLoad_ptr);
#line 635 "UecDirectTransfer.cpp"
  llvm_cbe_tmp30 = *((&((&((&((&((&((&((&llvm_cbe_tmp22->field0))->field1))->field0))->field1))->field0))->field0))->field0));
#line 637 "UecDirectTransfer.cpp"
  llvm_cbe_call33 = _ZN11DummyStreamlsIPKcEERS_T_((&mout), ((&_OC_str54.array[((signed int )0u)])));
#line 637 "UecDirectTransfer.cpp"
  llvm_cbe_call35 = _ZN11DummyStreamlsIPFRSt8ios_baseS2_EEERS_T_(llvm_cbe_call33, _ZSt3decRSt8ios_base);
#line 637 "UecDirectTransfer.cpp"
  llvm_cbe_tmp36 = *(&llvm_cbe_p_uecEvent_ptr_2e_addr);
#line 637 "UecDirectTransfer.cpp"
  llvm_cbe_call38 = _ZNK3Uec8UecEvent13getInstanceIdEv(llvm_cbe_tmp36);
#line 637 "UecDirectTransfer.cpp"
  llvm_cbe_call40 = _ZN11DummyStreamlsIjEERS_T_(llvm_cbe_call35, llvm_cbe_call38);
#line 635 "UecDirectTransfer.cpp"
  if ((llvm_cbe_tmp30 != 0u)) {    goto llvm_cbe_if_2e_then31;  } else {    goto llvm_cbe_invoke_2e_cont43;  }


llvm_cbe_if_2e_then31:
#line 637 "UecDirectTransfer.cpp"
  llvm_cbe_call42 = _ZN11DummyStreamlsIPKcEERS_T_(llvm_cbe_call40, ((&_OC_str55.array[((signed int )0u)])));
#line 639 "UecDirectTransfer.cpp"
  *(&llvm_cbe_retval) = 0u;
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_cleanup_2e_dst161) = 1u;
#line 639 "UecDirectTransfer.cpp"
  goto llvm_cbe_cleanup168;

llvm_cbe_invoke_2e_cont43:
#line 642 "UecDirectTransfer.cpp"
  llvm_cbe_call53 = _ZN11DummyStreamlsIPKcEERS_T_(llvm_cbe_call40, ((&_OC_str56.array[((signed int )0u)])));
#line 646 "UecDirectTransfer.cpp"
  llvm_cbe_tmp55 = *(&llvm_cbe_l_asn1PayLoad_ptr);
#line 646 "UecDirectTransfer.cpp"
  llvm_cbe_tmp65 = *((&((&((&((&((&((&((&((&((&llvm_cbe_tmp55->field0))->field1))->field0))->field1))->field0))->field0))->field1))->field0))->field0));
#line 646 "UecDirectTransfer.cpp"
  *((&llvm_cbe_l_nasPdu.field0)) = llvm_cbe_tmp65;
#line 647 "UecDirectTransfer.cpp"
  llvm_cbe_tmp67 = *(&llvm_cbe_l_asn1PayLoad_ptr);
#line 647 "UecDirectTransfer.cpp"
  llvm_cbe_tmp77 = *((&((&((&((&((&((&((&((&((&llvm_cbe_tmp67->field0))->field1))->field0))->field1))->field0))->field0))->field1))->field0))->field1));
#line 647 "UecDirectTransfer.cpp"
  *((&llvm_cbe_l_nasPdu.field1)) = llvm_cbe_tmp77;
#line 649 "UecDirectTransfer.cpp"
  llvm_cbe_tmp79 = *(&llvm_cbe_l_uecUeContextDataWrite_ptr);
#line 649 "UecDirectTransfer.cpp"
  _ZN3Uec16UecUeContextData12insertNasPduERK10SAsnDynstr(llvm_cbe_tmp79, (&llvm_cbe_l_nasPdu));
#line 651 "UecDirectTransfer.cpp"
  *(&llvm_cbe_retval) = 1u;
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_cleanup_2e_dst161) = 2u;
#line 651 "UecDirectTransfer.cpp"
  goto llvm_cbe_cleanup168;

llvm_cbe_if_2e_end81:
#line 658 "UecDirectTransfer.cpp"
  llvm_cbe_call85 = _ZN3Uec17UecDirectTransfer22setTupL3MessageRequestER16TUP_L3MessageReq(llvm_cbe_this1, (&llvm_cbe_l_payLoad));
#line 661 "UecDirectTransfer.cpp"
  llvm_cbe_tmp86 = *(&llvm_cbe_p_uecEvent_ptr_2e_addr);
#line 661 "UecDirectTransfer.cpp"
  llvm_cbe_call88 = _ZN3Uec17UecDirectTransfer21setS1ApUlNasTransportEPNS_8UecEventER25SS1apUFUplinkNASTransport(llvm_cbe_this1, llvm_cbe_tmp86, (&llvm_cbe_l_asn1PayLoad));
#line 661 "UecDirectTransfer.cpp"
  *(&llvm_cbe_l_success) = llvm_cbe_call88;
#line 661 "UecDirectTransfer.cpp"
  llvm_cbe_tmp89 = *(&llvm_cbe_l_success);
#line 661 "UecDirectTransfer.cpp"
  if ((1u == llvm_cbe_tmp89)) {    goto llvm_cbe_if_2e_end93;  } else {    goto llvm_cbe_if_2e_then91;  }


llvm_cbe_if_2e_then91:
#line 664 "UecDirectTransfer.cpp"
  llvm_cbe_tmp92 = *(&llvm_cbe_l_success);
#line 664 "UecDirectTransfer.cpp"
  *(&llvm_cbe_retval) = llvm_cbe_tmp92;
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_cleanup_2e_dst161) = 3u;
#line 664 "UecDirectTransfer.cpp"
  goto llvm_cbe_cleanup168;

llvm_cbe_if_2e_end93:
#line 676 "UecDirectTransfer.cpp"
  llvm_cbe_call95 = _ZN11DummyStreamlsIPKcEERS_T_((&mout), ((&_OC_str54.array[((signed int )0u)])));
#line 676 "UecDirectTransfer.cpp"
  llvm_cbe_call97 = _ZN11DummyStreamlsIPFRSt8ios_baseS2_EEERS_T_(llvm_cbe_call95, _ZSt3decRSt8ios_base);
#line 676 "UecDirectTransfer.cpp"
  llvm_cbe_tmp99 = *((&llvm_cbe_l_payLoad.field3));
#line 676 "UecDirectTransfer.cpp"
  llvm_cbe_call101 = _ZN11DummyStreamlsIjEERS_T_(llvm_cbe_call97, llvm_cbe_tmp99);
#line 676 "UecDirectTransfer.cpp"
  llvm_cbe_call103 = _ZN11DummyStreamlsIPKcEERS_T_(llvm_cbe_call101, ((&_OC_str57.array[((signed int )0u)])));
#line 676 "UecDirectTransfer.cpp"
  llvm_cbe_call105 = _ZN11DummyStreamlsIPFRSt8ios_baseS2_EEERS_T_(llvm_cbe_call103, _ZSt3decRSt8ios_base);
#line 676 "UecDirectTransfer.cpp"
  llvm_cbe_tmp108 = *((&((&llvm_cbe_l_asn1PayLoad.field2))->field0));
#line 676 "UecDirectTransfer.cpp"
  llvm_cbe_call110 = _ZN11DummyStreamlsIjEERS_T_(llvm_cbe_call105, llvm_cbe_tmp108);
#line 681 "UecDirectTransfer.cpp"
  llvm_cbe_tmp112 = *(&llvm_cbe_l_uecUeContextDataWrite_ptr);
#line 681 "UecDirectTransfer.cpp"
  llvm_cbe_call114 = _ZNK3Uec16UecUeContextData9getCellIdEv(llvm_cbe_tmp112);
#line 681 "UecDirectTransfer.cpp"
  _ZN3Uec8UecEventC1EjNS_15EUecServiceTypeEjPvjiS2_j((&llvm_cbe_l_uecEvent), 10207u, 2u, llvm_cbe_call114, (((unsigned char *)(&llvm_cbe_l_payLoad))), 40u, 5u, (((unsigned char *)(&llvm_cbe_l_asn1PayLoad))), 72u);
#line 685 "UecDirectTransfer.cpp"
  llvm_cbe_tmp122 = *(&llvm_cbe_l_uecUeContextDataWrite_ptr);
#line 685 "UecDirectTransfer.cpp"
  llvm_cbe_call124 = ((_ZN3Uec16UecUeContextData14isMmeConnectedEv(llvm_cbe_tmp122))&1);
#line 685 "UecDirectTransfer.cpp"
  if (llvm_cbe_call124) {    goto llvm_cbe_if_2e_then125;  } else {    goto llvm_cbe_if_2e_else;  }


llvm_cbe_if_2e_then125:
#line 687 "UecDirectTransfer.cpp"
  llvm_cbe_call127 = _ZNK3Uec14UecServiceBase23getUecServiceControllerEv((((struct l_class_OC_Uec_KD__KD_UecServiceBase *)llvm_cbe_this1)));
#line 687 "UecDirectTransfer.cpp"
  llvm_cbe_tmp__126 = *(((struct l_class_OC_Uec_KD__KD_UecBaseData * (***) (struct l_class_OC_Uec_KD__KD_UecBaseData *, unsigned int ))llvm_cbe_call127));
#line 687 "UecDirectTransfer.cpp"
  llvm_cbe_tmp__127 = *((&(*llvm_cbe_tmp__126)));
#line 687 "UecDirectTransfer.cpp"
  llvm_cbe_call130 = llvm_cbe_tmp__127(llvm_cbe_call127, 3u);
#line 687 "UecDirectTransfer.cpp"
  llvm_cbe_tmp__128 = *(((unsigned int  (***) (struct l_class_OC_Uec_KD__KD_UecBaseData *, struct l_class_OC_Uec_KD__KD_UecEvent *))llvm_cbe_call130));
#line 687 "UecDirectTransfer.cpp"
  llvm_cbe_tmp__129 = *((&(*llvm_cbe_tmp__128)));
#line 687 "UecDirectTransfer.cpp"
  llvm_cbe_call133 = llvm_cbe_tmp__129(llvm_cbe_call130, (&llvm_cbe_l_uecEvent));
#line 687 "UecDirectTransfer.cpp"
  *(&llvm_cbe_l_success) = llvm_cbe_call133;
#line 687 "UecDirectTransfer.cpp"
  llvm_cbe_tmp134 = *(&llvm_cbe_l_success);
#line 687 "UecDirectTransfer.cpp"
  if ((1u == llvm_cbe_tmp134)) {    goto llvm_cbe_if_2e_end157;  } else {    goto llvm_cbe_if_2e_then136;  }


llvm_cbe_if_2e_then136:
#line 689 "UecDirectTransfer.cpp"
  llvm_cbe_call138 = _ZN11DummyStreamlsIPKcEERS_T_((&mout), ((&_OC_str54.array[((signed int )0u)])));
#line 689 "UecDirectTransfer.cpp"
  llvm_cbe_call140 = _ZN11DummyStreamlsIPFRSt8ios_baseS2_EEERS_T_(llvm_cbe_call138, _ZSt3decRSt8ios_base);
#line 689 "UecDirectTransfer.cpp"
  llvm_cbe_call142 = _ZNK3Uec14UecServiceBase13getUecManagerEv((((struct l_class_OC_Uec_KD__KD_UecServiceBase *)llvm_cbe_this1)));
#line 689 "UecDirectTransfer.cpp"
  llvm_cbe_call144 = _ZNK3Uec14UecManagerBase13getInstanceIdEv(llvm_cbe_call142);
#line 689 "UecDirectTransfer.cpp"
  llvm_cbe_call146 = _ZN11DummyStreamlsIjEERS_T_(llvm_cbe_call140, llvm_cbe_call144);
#line 689 "UecDirectTransfer.cpp"
  llvm_cbe_call148 = _ZN11DummyStreamlsIPKcEERS_T_(llvm_cbe_call146, ((&_OC_str48.array[((signed int )0u)])));
#line 689 "UecDirectTransfer.cpp"
  llvm_cbe_call150 = _ZN11DummyStreamlsIPFRSt8ios_baseS2_EEERS_T_(llvm_cbe_call148, _ZSt3decRSt8ios_base);
#line 689 "UecDirectTransfer.cpp"
  llvm_cbe_tmp151 = *(&llvm_cbe_l_success);
#line 689 "UecDirectTransfer.cpp"
  llvm_cbe_call153 = _ZN11DummyStreamlsIiEERS_T_(llvm_cbe_call150, llvm_cbe_tmp151);
#line 693 "UecDirectTransfer.cpp"
  *(&llvm_cbe_retval) = 0u;
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_cleanup_2e_dst) = 1u;
#line 693 "UecDirectTransfer.cpp"
  goto llvm_cbe_cleanup;

llvm_cbe_if_2e_else:
#line 698 "UecDirectTransfer.cpp"
  llvm_cbe_call156 = _ZN11DummyStreamlsIPKcEERS_T_((&mout), ((&_OC_str58.array[((signed int )0u)])));
#line 699 "UecDirectTransfer.cpp"
  *(&llvm_cbe_retval) = 1u;
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_cleanup_2e_dst) = 2u;
#line 699 "UecDirectTransfer.cpp"
  goto llvm_cbe_cleanup;

llvm_cbe_if_2e_end157:
#line 702 "UecDirectTransfer.cpp"
  *(&llvm_cbe_retval) = 1u;
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_cleanup_2e_dst) = 3u;
#line 702 "UecDirectTransfer.cpp"
  goto llvm_cbe_cleanup;

llvm_cbe_cleanup_2e_pad:
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_cleanup_2e_dst161) = 4u;
#line 0 "LLVM INTERNAL"
  goto llvm_cbe_cleanup168;

llvm_cbe_cleanup_2e_pad158:
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_cleanup_2e_dst161) = 5u;
#line 0 "LLVM INTERNAL"
  goto llvm_cbe_cleanup168;

llvm_cbe_cleanup_2e_pad159:
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_cleanup_2e_dst161) = 6u;
#line 0 "LLVM INTERNAL"
  goto llvm_cbe_cleanup168;

llvm_cbe_cleanup:
#line 703 "UecDirectTransfer.cpp"
  _ZN3Uec8UecEventD1Ev((&llvm_cbe_l_uecEvent));
#line 703 "UecDirectTransfer.cpp"
  llvm_cbe_tmp160 = *(&llvm_cbe_cleanup_2e_dst);
#line 703 "UecDirectTransfer.cpp"
  switch (llvm_cbe_tmp160) {
  default:
    goto llvm_cbe_cleanup_2e_end;;
  case 1u:
    goto llvm_cbe_cleanup_2e_pad;  case 2u:
    goto llvm_cbe_cleanup_2e_pad158;  case 3u:
    goto llvm_cbe_cleanup_2e_pad159;  }

llvm_cbe_cleanup_2e_end:
#line 703 "UecDirectTransfer.cpp"
  *(&llvm_cbe_cleanup_2e_dst161) = 0u;
#line 703 "UecDirectTransfer.cpp"
  goto llvm_cbe_cleanup168;

llvm_cbe_cleanup168:
#line 703 "UecDirectTransfer.cpp"
  _ZN3Uec17UecDebugCallstackD1Ev((&llvm_cbe__callstackTmp_));
#line 703 "UecDirectTransfer.cpp"
  llvm_cbe_tmp169 = *(&llvm_cbe_cleanup_2e_dst161);
#line 703 "UecDirectTransfer.cpp"
  switch (llvm_cbe_tmp169) {
  default:
    goto llvm_cbe_cleanup_2e_end171;;
  case 1u:
    goto llvm_cbe_cleanup_2e_pad162;  case 2u:
    goto llvm_cbe_cleanup_2e_pad163;  case 3u:
    goto llvm_cbe_cleanup_2e_pad164;  case 4u:
    goto llvm_cbe_cleanup_2e_pad165;  case 5u:
    goto llvm_cbe_cleanup_2e_pad166;  case 6u:
    goto llvm_cbe_cleanup_2e_pad167;  }

llvm_cbe_cleanup_2e_end171:
#line 703 "UecDirectTransfer.cpp"
  *(&llvm_cbe_cleanup_2e_dst172) = 0u;
#line 703 "UecDirectTransfer.cpp"
  goto llvm_cbe_cleanup173;

llvm_cbe_cleanup173:
#line 703 "UecDirectTransfer.cpp"
  _ZNSsD1Ev((&llvm_cbe__currentFunction_));
#line 703 "UecDirectTransfer.cpp"
  llvm_cbe_tmp174 = *(&llvm_cbe_cleanup_2e_dst172);
#line 703 "UecDirectTransfer.cpp"
  llvm_cbe_tmp__130 = *(&llvm_cbe_retval);
#line 703 "UecDirectTransfer.cpp"
  return llvm_cbe_tmp__130;

llvm_cbe_cleanup_2e_pad162:
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_cleanup_2e_dst172) = 1u;
#line 0 "LLVM INTERNAL"
  goto llvm_cbe_cleanup173;

llvm_cbe_cleanup_2e_pad163:
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_cleanup_2e_dst172) = 1u;
#line 0 "LLVM INTERNAL"
  goto llvm_cbe_cleanup173;

llvm_cbe_cleanup_2e_pad164:
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_cleanup_2e_dst172) = 1u;
#line 0 "LLVM INTERNAL"
  goto llvm_cbe_cleanup173;

llvm_cbe_cleanup_2e_pad165:
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_cleanup_2e_dst172) = 1u;
#line 0 "LLVM INTERNAL"
  goto llvm_cbe_cleanup173;

llvm_cbe_cleanup_2e_pad166:
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_cleanup_2e_dst172) = 1u;
#line 0 "LLVM INTERNAL"
  goto llvm_cbe_cleanup173;

llvm_cbe_cleanup_2e_pad167:
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_cleanup_2e_dst172) = 1u;
#line 0 "LLVM INTERNAL"
  goto llvm_cbe_cleanup173;
}


void _ZNSt4pairISt17_Rb_tree_iteratorIS_IKj10SAsnDynstrEEbEC1Ev(struct l_struct_OC_std_KD__KD_pair *llvm_cbe_this);

#line 0 "LLVM INTERNAL"
void _ZN3Uec16UecUeContextData12insertNasPduERK10SAsnDynstr(struct l_class_OC_Uec_KD__KD_UecUeContextData *llvm_cbe_this, struct l_struct_OC_SAsnDynstr *llvm_cbe_p_nasPdu) {
  struct l_class_OC_Uec_KD__KD_UecUeContextData *llvm_cbe_this_2e_addr;    /* Address-exposed local */
  struct l_struct_OC_SAsnDynstr *llvm_cbe_p_nasPdu_2e_addr;    /* Address-exposed local */
  struct l_struct_OC_SAsnDynstr llvm_cbe_l_nasPdu;    /* Address-exposed local */
  struct l_struct_OC_std_KD__KD_pair llvm_cbe_l_mapReturn;    /* Address-exposed local */
  struct l_unnamed78 llvm_cbe_agg_2e_tmp;    /* Address-exposed local */
  struct l_unnamed78 llvm_cbe_agg_2e_tmp20;    /* Address-exposed local */
  struct l_struct_OC_std_KD__KD_pair llvm_cbe_coerce;    /* Address-exposed local */
  struct l_class_OC_Uec_KD__KD_UecUeContextData *llvm_cbe_this1;
  struct l_struct_OC_SAsnDynstr *llvm_cbe_tmp;
  unsigned int llvm_cbe_tmp3;
  struct l_struct_OC_SAsnDynstr *llvm_cbe_tmp5;
  unsigned int llvm_cbe_tmp7;
  unsigned char *llvm_cbe_call;
  unsigned char *llvm_cbe_tmp10;
  struct l_struct_OC_SAsnDynstr *llvm_cbe_tmp11;
  unsigned char *llvm_cbe_tmp13;
  struct l_struct_OC_SAsnDynstr *llvm_cbe_tmp14;
  unsigned int llvm_cbe_tmp16;
  unsigned char *llvm_cbe_tmp__131;
  struct l_struct_OC_SS1apUEAggregateMaximumBitrate llvm_cbe_call22;
  unsigned char *llvm_cbe_tmp__132;
  unsigned char llvm_cbe_tmp26;
  unsigned int llvm_cbe_tmp29;
  struct l_class_OC_BaseEvent *llvm_cbe_call30;
  struct l_unnamed78 *llvm_cbe_call32;
  unsigned int llvm_cbe_tmp34;
  struct l_class_OC_BaseEvent *llvm_cbe_call35;
  struct l_class_OC_BaseEvent *llvm_cbe_call36;
  struct l_unnamed78 *llvm_cbe_call38;
  unsigned int llvm_cbe_tmp41;
  struct l_class_OC_BaseEvent *llvm_cbe_call42;
  unsigned int *llvm_cbe_tmp43;
  unsigned int llvm_cbe_tmp44;

#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_this_2e_addr) = llvm_cbe_this;
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_p_nasPdu_2e_addr) = llvm_cbe_p_nasPdu;
#line 0 "LLVM INTERNAL"
  llvm_cbe_this1 = *(&llvm_cbe_this_2e_addr);
#line 2411 "UecUeContextData.hpp"
  llvm_cbe_tmp = *(&llvm_cbe_p_nasPdu_2e_addr);
#line 2411 "UecUeContextData.hpp"
  llvm_cbe_tmp3 = *((&llvm_cbe_tmp->field0));
#line 2411 "UecUeContextData.hpp"
  *((&llvm_cbe_l_nasPdu.field0)) = llvm_cbe_tmp3;
#line 2412 "UecUeContextData.hpp"
  llvm_cbe_tmp5 = *(&llvm_cbe_p_nasPdu_2e_addr);
#line 2412 "UecUeContextData.hpp"
  llvm_cbe_tmp7 = *((&llvm_cbe_tmp5->field0));
#line 2412 "UecUeContextData.hpp"
  llvm_cbe_call = _Znam((((unsigned long long )(((unsigned long long )(((unsigned long long )(unsigned int )llvm_cbe_tmp7))) * ((unsigned long long )1ull)))));
#line 2412 "UecUeContextData.hpp"
  *((&llvm_cbe_l_nasPdu.field1)) = llvm_cbe_call;
#line 2413 "UecUeContextData.hpp"
  llvm_cbe_tmp10 = *((&llvm_cbe_l_nasPdu.field1));
#line 2413 "UecUeContextData.hpp"
  llvm_cbe_tmp11 = *(&llvm_cbe_p_nasPdu_2e_addr);
#line 2413 "UecUeContextData.hpp"
  llvm_cbe_tmp13 = *((&llvm_cbe_tmp11->field1));
#line 2413 "UecUeContextData.hpp"
  llvm_cbe_tmp14 = *(&llvm_cbe_p_nasPdu_2e_addr);
#line 2413 "UecUeContextData.hpp"
  llvm_cbe_tmp16 = *((&llvm_cbe_tmp14->field0));
#line 0 "LLVM INTERNAL"
  llvm_cbe_tmp__131 = memcpy(llvm_cbe_tmp10, llvm_cbe_tmp13, (((unsigned long long )(unsigned int )llvm_cbe_tmp16)));
#line 2425 "UecUeContextData.hpp"
  _ZNSt4pairISt17_Rb_tree_iteratorIS_IKj10SAsnDynstrEEbEC1Ev((&llvm_cbe_l_mapReturn));
#line 2427 "UecUeContextData.hpp"
  _ZNSt4pairIj10SAsnDynstrEC1ERKjRKS0_((&llvm_cbe_agg_2e_tmp20), ((&llvm_cbe_this1->field3)), (&llvm_cbe_l_nasPdu));
#line 2427 "UecUeContextData.hpp"
  _ZNSt4pairIKj10SAsnDynstrEC1IjS1_EERKS_IT_T0_E((&llvm_cbe_agg_2e_tmp), (&llvm_cbe_agg_2e_tmp20));
#line 2427 "UecUeContextData.hpp"
  llvm_cbe_call22 = _ZNSt3mapIj10SAsnDynstrSt4lessIjESaISt4pairIKjS0_EEE6insertERKS5_(((&llvm_cbe_this1->field4)), (&llvm_cbe_agg_2e_tmp));
#line 2427 "UecUeContextData.hpp"
  ((struct __attribute__ ((packed, aligned(1))) {struct l_struct_OC_SS1apUEAggregateMaximumBitrate data; } *)(((struct l_struct_OC_SS1apUEAggregateMaximumBitrate *)(&llvm_cbe_coerce))))->data = llvm_cbe_call22;
#line 0 "LLVM INTERNAL"
  llvm_cbe_tmp__132 = memcpy((((unsigned char *)(&llvm_cbe_l_mapReturn))), (((unsigned char *)(&llvm_cbe_coerce))), 16ull);
#line 2428 "UecUeContextData.hpp"
  llvm_cbe_tmp26 = *((&llvm_cbe_l_mapReturn.field1));
#line 2428 "UecUeContextData.hpp"
  if (((((((((bool )llvm_cbe_tmp26&1u))&1)) ^ 1)&1))) {    goto llvm_cbe_if_2e_then;  } else {    goto llvm_cbe_if_2e_end;  }


llvm_cbe_if_2e_then:
#line 2428 "UecUeContextData.hpp"
  llvm_cbe_tmp29 = *((&llvm_cbe_this1->field3));
#line 2428 "UecUeContextData.hpp"
  _ZN3Uec9uecAssertEiPKcS1_z(2428u, ((&_OC_str96.array[((signed int )0u)])), ((&_OC_str97.array[((signed int )0u)])), llvm_cbe_tmp29);
#line 2428 "UecUeContextData.hpp"
  /*UNREACHABLE*/;

llvm_cbe_if_2e_end:
#line 2431 "UecUeContextData.hpp"
  llvm_cbe_call30 = _ZN11DummyStreamlsIPKcEERS_T_((&mout), ((&_OC_str98.array[((signed int )0u)])));
#line 2431 "UecUeContextData.hpp"
  llvm_cbe_call32 = _ZNKSt17_Rb_tree_iteratorISt4pairIKj10SAsnDynstrEEptEv(((&llvm_cbe_l_mapReturn.field0)));
#line 2431 "UecUeContextData.hpp"
  llvm_cbe_tmp34 = *((&llvm_cbe_call32->field0));
#line 2431 "UecUeContextData.hpp"
  llvm_cbe_call35 = _ZN11DummyStreamlsIjEERS_T_(llvm_cbe_call30, llvm_cbe_tmp34);
#line 2431 "UecUeContextData.hpp"
  llvm_cbe_call36 = _ZN11DummyStreamlsIPKcEERS_T_(llvm_cbe_call35, ((&_OC_str99.array[((signed int )0u)])));
#line 2431 "UecUeContextData.hpp"
  llvm_cbe_call38 = _ZNKSt17_Rb_tree_iteratorISt4pairIKj10SAsnDynstrEEptEv(((&llvm_cbe_l_mapReturn.field0)));
#line 2431 "UecUeContextData.hpp"
  llvm_cbe_tmp41 = *((&((&llvm_cbe_call38->field1))->field0));
#line 2431 "UecUeContextData.hpp"
  llvm_cbe_call42 = _ZN11DummyStreamlsIjEERS_T_(llvm_cbe_call36, llvm_cbe_tmp41);
#line 2435 "UecUeContextData.hpp"
  llvm_cbe_tmp43 = (&llvm_cbe_this1->field3);
#line 2435 "UecUeContextData.hpp"
  llvm_cbe_tmp44 = *llvm_cbe_tmp43;
#line 2435 "UecUeContextData.hpp"
  *llvm_cbe_tmp43 = (((unsigned int )(((unsigned int )llvm_cbe_tmp44) + ((unsigned int )1u))));
#line 2436 "UecUeContextData.hpp"
  return;
}


#line 0 "LLVM INTERNAL"
unsigned int _ZN3Uec17UecDirectTransfer21setS1ApUlNasTransportEPNS_8UecEventER25SS1apUFUplinkNASTransport(struct l_class_OC_Uec_KD__KD_UecDirectTransfer *llvm_cbe_this, struct l_class_OC_Uec_KD__KD_UecEvent *llvm_cbe_p_uecEvent_ptr, struct l_struct_OC_SS1apUFUplinkNASTransport *llvm_cbe_p_asn1PayLoad) {
  unsigned int llvm_cbe_retval;    /* Address-exposed local */
  struct l_class_OC_Uec_KD__KD_UecDirectTransfer *llvm_cbe_this_2e_addr;    /* Address-exposed local */
  struct l_class_OC_Uec_KD__KD_UecEvent *llvm_cbe_p_uecEvent_ptr_2e_addr;    /* Address-exposed local */
  struct l_struct_OC_SS1apUFUplinkNASTransport *llvm_cbe_p_asn1PayLoad_2e_addr;    /* Address-exposed local */
  struct l_class_OC_std_KD__KD_basic_string llvm_cbe__currentFunction_;    /* Address-exposed local */
  struct l_class_OC_BaseEvent llvm_cbe_agg_2e_tmp;    /* Address-exposed local */
  struct l_class_OC_Uec_KD__KD_UecDebugCallstack llvm_cbe__callstackTmp_;    /* Address-exposed local */
  struct l_class_OC_Uec_KD__KD_UecUeContextData *llvm_cbe_l_uecUeContextDataWrite_ptr;    /* Address-exposed local */
  struct l_class_OC_Uec_KD__KD_UecCellData *llvm_cbe_l_uecCellDataRead_ptr;    /* Address-exposed local */
  struct l_struct_OC_SErrcULInformationTransfer *llvm_cbe_l_asn1PayLoad_ptr;    /* Address-exposed local */
  unsigned int llvm_cbe_shift;    /* Address-exposed local */
  unsigned int llvm_cbe_u32Val;    /* Address-exposed local */
  struct l_struct_OC_SAmRlcPbTab llvm_cbe_l_plmnId;    /* Address-exposed local */
  struct l_struct_OC_SS1apGUMMEI llvm_cbe_coerce;    /* Address-exposed local */
  unsigned int llvm_cbe_cleanup_2e_dst;    /* Address-exposed local */
  unsigned int llvm_cbe_cleanup_2e_dst199;    /* Address-exposed local */
  struct l_class_OC_Uec_KD__KD_UecDirectTransfer *llvm_cbe_this1;
  struct l_class_OC_Uec_KD__KD_UecBaseData *llvm_cbe_call;
  struct l_class_OC_Uec_KD__KD_UecBaseData * (**llvm_cbe_tmp__133) (struct l_class_OC_Uec_KD__KD_UecBaseData *, unsigned int );
  struct l_class_OC_Uec_KD__KD_UecBaseData * (*llvm_cbe_tmp__134) (struct l_class_OC_Uec_KD__KD_UecBaseData *, unsigned int );
  struct l_class_OC_Uec_KD__KD_UecBaseData *llvm_cbe_call15;
  struct l_class_OC_Uec_KD__KD_UecBaseData *llvm_cbe_call17;
  struct l_class_OC_Uec_KD__KD_UecCellData * (**llvm_cbe_tmp__135) (struct l_class_OC_Uec_KD__KD_UecBaseData *, unsigned int );
  struct l_class_OC_Uec_KD__KD_UecCellData * (*llvm_cbe_tmp__136) (struct l_class_OC_Uec_KD__KD_UecBaseData *, unsigned int );
  struct l_class_OC_Uec_KD__KD_UecUeContextData *llvm_cbe_tmp;
  unsigned int llvm_cbe_call20;
  struct l_class_OC_Uec_KD__KD_UecCellData *llvm_cbe_call22;
  struct l_struct_OC_SS1apUFUplinkNASTransport *llvm_cbe_tmp23;
  unsigned char *llvm_cbe_tmp__137;
  struct l_class_OC_Uec_KD__KD_UecUeContextData *llvm_cbe_tmp24;
  unsigned int llvm_cbe_call26;
  struct l_struct_OC_SS1apUFUplinkNASTransport *llvm_cbe_tmp27;
  struct l_class_OC_Uec_KD__KD_UecUeContextData *llvm_cbe_tmp29;
  unsigned int llvm_cbe_call31;
  struct l_struct_OC_SS1apUFUplinkNASTransport *llvm_cbe_tmp33;
  struct l_class_OC_Uec_KD__KD_UecEvent *llvm_cbe_tmp36;
  unsigned char *llvm_cbe_call38;
  struct l_struct_OC_SErrcULInformationTransfer *llvm_cbe_tmp39;
  unsigned int llvm_cbe_tmp47;
  struct l_struct_OC_SErrcULInformationTransfer *llvm_cbe_tmp48;
  unsigned int llvm_cbe_tmp58;
  unsigned int llvm_cbe_tmp63;
  unsigned char *llvm_cbe_call66;
  unsigned char *llvm_cbe_tmp71;
  struct l_struct_OC_SErrcULInformationTransfer *llvm_cbe_tmp72;
  unsigned char *llvm_cbe_tmp82;
  unsigned int llvm_cbe_tmp85;
  unsigned char *llvm_cbe_tmp__138;
  struct l_struct_OC_SS1apUFUplinkNASTransport *llvm_cbe_tmp87;
  unsigned char *llvm_cbe_tmp__139;
  struct l_class_OC_Uec_KD__KD_UecCellData *llvm_cbe_tmp117;
  unsigned int llvm_cbe_call119;
  struct l_struct_OC_SS1apUFUplinkNASTransport *llvm_cbe_tmp120;
  struct l_class_OC_Uec_KD__KD_UecCellData *llvm_cbe_tmp123;
  struct l_struct_OC_SAmRlcPbTab *llvm_cbe_call125;
  unsigned int llvm_cbe_tmp127;
  struct l_struct_OC_SS1apUFUplinkNASTransport *llvm_cbe_tmp128;
  struct l_class_OC_Uec_KD__KD_UecCellData *llvm_cbe_tmp132;
  struct l_struct_OC_SAmRlcPbTab *llvm_cbe_call134;
  unsigned int llvm_cbe_tmp136;
  struct l_struct_OC_SS1apUFUplinkNASTransport *llvm_cbe_tmp137;
  struct l_class_OC_Uec_KD__KD_UecCellData *llvm_cbe_tmp141;
  struct l_struct_OC_SAmRlcPbTab *llvm_cbe_call143;
  unsigned int llvm_cbe_tmp145;
  struct l_struct_OC_SS1apUFUplinkNASTransport *llvm_cbe_tmp146;
  struct l_class_OC_Uec_KD__KD_UecCellData *llvm_cbe_tmp152;
  unsigned int llvm_cbe_call154;
  unsigned int llvm_cbe_tmp155;
  unsigned int llvm_cbe_tmp156;
  unsigned int llvm_cbe_tmp157;
  struct l_struct_OC_SS1apUFUplinkNASTransport *llvm_cbe_tmp159;
  unsigned int llvm_cbe_tmp163;
  struct l_struct_OC_SS1apUFUplinkNASTransport *llvm_cbe_tmp166;
  struct l_class_OC_Uec_KD__KD_UecUeContextData *llvm_cbe_tmp173;
  struct l_struct_OC_SS1apUEAggregateMaximumBitrate llvm_cbe_call175;
  unsigned char *llvm_cbe_tmp__140;
  unsigned int llvm_cbe_tmp180;
  struct l_struct_OC_SS1apUFUplinkNASTransport *llvm_cbe_tmp181;
  unsigned int llvm_cbe_tmp186;
  struct l_struct_OC_SS1apUFUplinkNASTransport *llvm_cbe_tmp187;
  unsigned int llvm_cbe_tmp192;
  struct l_struct_OC_SS1apUFUplinkNASTransport *llvm_cbe_tmp193;
  struct l_class_OC_BaseEvent *llvm_cbe_call93;
  struct l_class_OC_BaseEvent *llvm_cbe_call95;
  struct l_class_OC_Uec_KD__KD_UecManagerBase *llvm_cbe_call97;
  unsigned int llvm_cbe_call99;
  struct l_class_OC_BaseEvent *llvm_cbe_call101;
  struct l_class_OC_BaseEvent *llvm_cbe_call103;
  struct l_class_OC_BaseEvent *llvm_cbe_call105;
  struct l_struct_OC_SErrcULInformationTransfer *llvm_cbe_tmp106;
  unsigned int llvm_cbe_tmp114;
  struct l_class_OC_BaseEvent *llvm_cbe_call116;
  unsigned int llvm_cbe_tmp198;
  unsigned int llvm_cbe_tmp201;
  unsigned int llvm_cbe_tmp__141;

#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_this_2e_addr) = llvm_cbe_this;
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_p_uecEvent_ptr_2e_addr) = llvm_cbe_p_uecEvent_ptr;
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_p_asn1PayLoad_2e_addr) = llvm_cbe_p_asn1PayLoad;
#line 0 "LLVM INTERNAL"
  llvm_cbe_this1 = *(&llvm_cbe_this_2e_addr);
#line 899 "UecDirectTransfer.cpp"
  _ZNSaIcEC1Ev((&llvm_cbe_agg_2e_tmp));
#line 899 "UecDirectTransfer.cpp"
  _ZNSsC1EPKcRKSaIcE((&llvm_cbe__currentFunction_), ((&__PRETTY_FUNCTION___OC__ZN3Uec17UecDirectTransfer21setS1ApUlNasTransportEPNS_8UecEventER25SS1apUFUplinkNASTransport.array[((signed int )0u)])), (&llvm_cbe_agg_2e_tmp));
#line 899 "UecDirectTransfer.cpp"
  _ZNSaIcED1Ev((&llvm_cbe_agg_2e_tmp));
#line 899 "UecDirectTransfer.cpp"
  _ZN3Uec17UecDebugCallstackC1ERKSs((&llvm_cbe__callstackTmp_), (&llvm_cbe__currentFunction_));
#line 902 "UecDirectTransfer.cpp"
  llvm_cbe_call = _ZNK3Uec14UecServiceBase16getUecDataAccessEv((((struct l_class_OC_Uec_KD__KD_UecServiceBase *)llvm_cbe_this1)));
#line 902 "UecDirectTransfer.cpp"
  llvm_cbe_tmp__133 = *(((struct l_class_OC_Uec_KD__KD_UecBaseData * (***) (struct l_class_OC_Uec_KD__KD_UecBaseData *, unsigned int ))llvm_cbe_call));
#line 902 "UecDirectTransfer.cpp"
  llvm_cbe_tmp__134 = *((&llvm_cbe_tmp__133[((signed long long )2ull)]));
#line 902 "UecDirectTransfer.cpp"
  llvm_cbe_call15 = llvm_cbe_tmp__134(llvm_cbe_call, 1u);
#line 902 "UecDirectTransfer.cpp"
  *(&llvm_cbe_l_uecUeContextDataWrite_ptr) = (((struct l_class_OC_Uec_KD__KD_UecUeContextData *)llvm_cbe_call15));
#line 903 "UecDirectTransfer.cpp"
  llvm_cbe_call17 = _ZNK3Uec14UecServiceBase23getUecServiceControllerEv((((struct l_class_OC_Uec_KD__KD_UecServiceBase *)llvm_cbe_this1)));
#line 903 "UecDirectTransfer.cpp"
  llvm_cbe_tmp__135 = *(((struct l_class_OC_Uec_KD__KD_UecCellData * (***) (struct l_class_OC_Uec_KD__KD_UecBaseData *, unsigned int ))llvm_cbe_call17));
#line 903 "UecDirectTransfer.cpp"
  llvm_cbe_tmp__136 = *((&llvm_cbe_tmp__135[((signed long long )12ull)]));
#line 903 "UecDirectTransfer.cpp"
  llvm_cbe_tmp = *(&llvm_cbe_l_uecUeContextDataWrite_ptr);
#line 903 "UecDirectTransfer.cpp"
  llvm_cbe_call20 = _ZNK3Uec16UecUeContextData9getCellIdEv(llvm_cbe_tmp);
#line 903 "UecDirectTransfer.cpp"
  llvm_cbe_call22 = llvm_cbe_tmp__136(llvm_cbe_call17, llvm_cbe_call20);
#line 903 "UecDirectTransfer.cpp"
  *(&llvm_cbe_l_uecCellDataRead_ptr) = llvm_cbe_call22;
#line 906 "UecDirectTransfer.cpp"
  llvm_cbe_tmp23 = *(&llvm_cbe_p_asn1PayLoad_2e_addr);
#line 0 "LLVM INTERNAL"
  llvm_cbe_tmp__137 = memset((((unsigned char *)llvm_cbe_tmp23)), 0u, 72ull);
#line 909 "UecDirectTransfer.cpp"
  llvm_cbe_tmp24 = *(&llvm_cbe_l_uecUeContextDataWrite_ptr);
#line 909 "UecDirectTransfer.cpp"
  llvm_cbe_call26 = _ZNK3Uec16UecUeContextData12getEnbUeS1IdEv(llvm_cbe_tmp24);
#line 909 "UecDirectTransfer.cpp"
  llvm_cbe_tmp27 = *(&llvm_cbe_p_asn1PayLoad_2e_addr);
#line 909 "UecDirectTransfer.cpp"
  *((&llvm_cbe_tmp27->field1)) = llvm_cbe_call26;
#line 910 "UecDirectTransfer.cpp"
  llvm_cbe_tmp29 = *(&llvm_cbe_l_uecUeContextDataWrite_ptr);
#line 910 "UecDirectTransfer.cpp"
  llvm_cbe_call31 = _ZNK3Uec16UecUeContextData12getMmeUeS1IdEv(llvm_cbe_tmp29);
#line 910 "UecDirectTransfer.cpp"
  llvm_cbe_tmp33 = *(&llvm_cbe_p_asn1PayLoad_2e_addr);
#line 910 "UecDirectTransfer.cpp"
  *((&llvm_cbe_tmp33->field0)) = (((unsigned long long )(unsigned int )llvm_cbe_call31));
#line 913 "UecDirectTransfer.cpp"
  llvm_cbe_tmp36 = *(&llvm_cbe_p_uecEvent_ptr_2e_addr);
#line 913 "UecDirectTransfer.cpp"
  llvm_cbe_call38 = _ZNK3Uec8UecEvent14getAsn1PayLoadEv(llvm_cbe_tmp36);
#line 913 "UecDirectTransfer.cpp"
  *(&llvm_cbe_l_asn1PayLoad_ptr) = (((struct l_struct_OC_SErrcULInformationTransfer *)llvm_cbe_call38));
#line 915 "UecDirectTransfer.cpp"
  llvm_cbe_tmp39 = *(&llvm_cbe_l_asn1PayLoad_ptr);
#line 915 "UecDirectTransfer.cpp"
  llvm_cbe_tmp47 = *((&((&((&((&((&((&((&llvm_cbe_tmp39->field0))->field1))->field0))->field1))->field0))->field0))->field0));
#line 0 "LLVM INTERNAL"
  if ((llvm_cbe_tmp47 == 0u)) {    goto llvm_cbe_sw_2e_bb;  } else {    goto llvm_cbe_sw_2e_default;  }


llvm_cbe_sw_2e_bb:
#line 920 "UecDirectTransfer.cpp"
  llvm_cbe_tmp48 = *(&llvm_cbe_l_asn1PayLoad_ptr);
#line 920 "UecDirectTransfer.cpp"
  llvm_cbe_tmp58 = *((&((&((&((&((&((&((&((&((&llvm_cbe_tmp48->field0))->field1))->field0))->field1))->field0))->field0))->field1))->field0))->field0));
#line 920 "UecDirectTransfer.cpp"
  *((&((&llvm_cbe_this1->field2))->field0)) = llvm_cbe_tmp58;
#line 921 "UecDirectTransfer.cpp"
  llvm_cbe_tmp63 = *((&((&llvm_cbe_this1->field2))->field0));
#line 921 "UecDirectTransfer.cpp"
  llvm_cbe_call66 = _Znam((((unsigned long long )(((unsigned long long )(((unsigned long long )(unsigned int )llvm_cbe_tmp63))) * ((unsigned long long )1ull)))));
#line 921 "UecDirectTransfer.cpp"
  *((&((&llvm_cbe_this1->field2))->field1)) = llvm_cbe_call66;
#line 923 "UecDirectTransfer.cpp"
  llvm_cbe_tmp71 = *((&((&llvm_cbe_this1->field2))->field1));
#line 923 "UecDirectTransfer.cpp"
  llvm_cbe_tmp72 = *(&llvm_cbe_l_asn1PayLoad_ptr);
#line 923 "UecDirectTransfer.cpp"
  llvm_cbe_tmp82 = *((&((&((&((&((&((&((&((&((&llvm_cbe_tmp72->field0))->field1))->field0))->field1))->field0))->field0))->field1))->field0))->field1));
#line 923 "UecDirectTransfer.cpp"
  llvm_cbe_tmp85 = *((&((&llvm_cbe_this1->field2))->field0));
#line 0 "LLVM INTERNAL"
  llvm_cbe_tmp__138 = memcpy(llvm_cbe_tmp71, llvm_cbe_tmp82, (((unsigned long long )(unsigned int )llvm_cbe_tmp85)));
#line 925 "UecDirectTransfer.cpp"
  llvm_cbe_tmp87 = *(&llvm_cbe_p_asn1PayLoad_2e_addr);
#line 0 "LLVM INTERNAL"
  llvm_cbe_tmp__139 = memcpy((((unsigned char *)((&llvm_cbe_tmp87->field2)))), (((unsigned char *)((&llvm_cbe_this1->field2)))), 16ull);
#line 941 "UecDirectTransfer.cpp"
  llvm_cbe_tmp117 = *(&llvm_cbe_l_uecCellDataRead_ptr);
#line 941 "UecDirectTransfer.cpp"
  llvm_cbe_call119 = _ZNK3Uec11UecCellData9getCellIdEv(llvm_cbe_tmp117);
#line 941 "UecDirectTransfer.cpp"
  llvm_cbe_tmp120 = *(&llvm_cbe_p_asn1PayLoad_2e_addr);
#line 941 "UecDirectTransfer.cpp"
  *((&((&llvm_cbe_tmp120->field3))->field1)) = llvm_cbe_call119;
#line 942 "UecDirectTransfer.cpp"
  llvm_cbe_tmp123 = *(&llvm_cbe_l_uecCellDataRead_ptr);
#line 942 "UecDirectTransfer.cpp"
  llvm_cbe_call125 = _ZNK3Uec11UecCellData12getPlmnRestLEj(llvm_cbe_tmp123, 0u);
#line 942 "UecDirectTransfer.cpp"
  llvm_cbe_tmp127 = *((&llvm_cbe_call125->field0));
#line 942 "UecDirectTransfer.cpp"
  llvm_cbe_tmp128 = *(&llvm_cbe_p_asn1PayLoad_2e_addr);
#line 942 "UecDirectTransfer.cpp"
  *((&((&((&llvm_cbe_tmp128->field3))->field0))->field2)) = llvm_cbe_tmp127;
#line 943 "UecDirectTransfer.cpp"
  llvm_cbe_tmp132 = *(&llvm_cbe_l_uecCellDataRead_ptr);
#line 943 "UecDirectTransfer.cpp"
  llvm_cbe_call134 = _ZNK3Uec11UecCellData12getPlmnRestLEj(llvm_cbe_tmp132, 0u);
#line 943 "UecDirectTransfer.cpp"
  llvm_cbe_tmp136 = *((&llvm_cbe_call134->field1));
#line 943 "UecDirectTransfer.cpp"
  llvm_cbe_tmp137 = *(&llvm_cbe_p_asn1PayLoad_2e_addr);
#line 943 "UecDirectTransfer.cpp"
  *((&((&((&llvm_cbe_tmp137->field3))->field0))->field1)) = llvm_cbe_tmp136;
#line 944 "UecDirectTransfer.cpp"
  llvm_cbe_tmp141 = *(&llvm_cbe_l_uecCellDataRead_ptr);
#line 944 "UecDirectTransfer.cpp"
  llvm_cbe_call143 = _ZNK3Uec11UecCellData12getPlmnRestLEj(llvm_cbe_tmp141, 0u);
#line 944 "UecDirectTransfer.cpp"
  llvm_cbe_tmp145 = *((&llvm_cbe_call143->field2));
#line 944 "UecDirectTransfer.cpp"
  llvm_cbe_tmp146 = *(&llvm_cbe_p_asn1PayLoad_2e_addr);
#line 944 "UecDirectTransfer.cpp"
  *((&((&((&llvm_cbe_tmp146->field3))->field0))->field0)) = llvm_cbe_tmp145;
#line 946 "UecDirectTransfer.cpp"
  llvm_cbe_tmp152 = *(&llvm_cbe_l_uecCellDataRead_ptr);
#line 946 "UecDirectTransfer.cpp"
  llvm_cbe_call154 = _ZNK3Uec11UecCellData6getTacEv(llvm_cbe_tmp152);
#line 946 "UecDirectTransfer.cpp"
  *(&llvm_cbe_u32Val) = llvm_cbe_call154;
#line 946 "UecDirectTransfer.cpp"
  *(&llvm_cbe_shift) = 0u;
#line 946 "UecDirectTransfer.cpp"
  llvm_cbe_tmp155 = *(&llvm_cbe_u32Val);
#line 946 "UecDirectTransfer.cpp"
  llvm_cbe_tmp156 = *(&llvm_cbe_shift);
#line 946 "UecDirectTransfer.cpp"
  *(&llvm_cbe_u32Val) = (llvm_cbe_tmp155 << llvm_cbe_tmp156);
#line 946 "UecDirectTransfer.cpp"
  llvm_cbe_tmp157 = *(&llvm_cbe_u32Val);
#line 946 "UecDirectTransfer.cpp"
  llvm_cbe_tmp159 = *(&llvm_cbe_p_asn1PayLoad_2e_addr);
#line 946 "UecDirectTransfer.cpp"
  *((&(*((&(*((&((&((&llvm_cbe_tmp159->field4))->field1))->field0))).array[((signed int )0u)]))))) = (((unsigned char )((((unsigned int )(((unsigned int )llvm_cbe_tmp157) >> ((unsigned int )8u)))) & 255u)));
#line 946 "UecDirectTransfer.cpp"
  llvm_cbe_tmp163 = *(&llvm_cbe_u32Val);
#line 946 "UecDirectTransfer.cpp"
  llvm_cbe_tmp166 = *(&llvm_cbe_p_asn1PayLoad_2e_addr);
#line 946 "UecDirectTransfer.cpp"
  *((&((&(*((&((&((&llvm_cbe_tmp166->field4))->field1))->field0))).array[((signed int )0u)]))[((signed long long )1ull)])) = (((unsigned char )(llvm_cbe_tmp163 & 255u)));
#line 948 "UecDirectTransfer.cpp"
  llvm_cbe_tmp173 = *(&llvm_cbe_l_uecUeContextDataWrite_ptr);
#line 948 "UecDirectTransfer.cpp"
  llvm_cbe_call175 = _ZNK3Uec16UecUeContextData11getS1GummeiEv(llvm_cbe_tmp173);
#line 948 "UecDirectTransfer.cpp"
  ((struct __attribute__ ((packed, aligned(1))) {struct l_struct_OC_SS1apUEAggregateMaximumBitrate data; } *)(((struct l_struct_OC_SS1apUEAggregateMaximumBitrate *)(&llvm_cbe_coerce))))->data = llvm_cbe_call175;
#line 0 "LLVM INTERNAL"
  llvm_cbe_tmp__140 = memcpy((((unsigned char *)(&llvm_cbe_l_plmnId))), (((unsigned char *)((&llvm_cbe_coerce.field0)))), 12ull);
#line 950 "UecDirectTransfer.cpp"
  llvm_cbe_tmp180 = *((&llvm_cbe_l_plmnId.field2));
#line 950 "UecDirectTransfer.cpp"
  llvm_cbe_tmp181 = *(&llvm_cbe_p_asn1PayLoad_2e_addr);
#line 950 "UecDirectTransfer.cpp"
  *((&((&((&llvm_cbe_tmp181->field4))->field0))->field2)) = llvm_cbe_tmp180;
#line 951 "UecDirectTransfer.cpp"
  llvm_cbe_tmp186 = *((&llvm_cbe_l_plmnId.field1));
#line 951 "UecDirectTransfer.cpp"
  llvm_cbe_tmp187 = *(&llvm_cbe_p_asn1PayLoad_2e_addr);
#line 951 "UecDirectTransfer.cpp"
  *((&((&((&llvm_cbe_tmp187->field4))->field0))->field1)) = llvm_cbe_tmp186;
#line 952 "UecDirectTransfer.cpp"
  llvm_cbe_tmp192 = *((&llvm_cbe_l_plmnId.field0));
#line 952 "UecDirectTransfer.cpp"
  llvm_cbe_tmp193 = *(&llvm_cbe_p_asn1PayLoad_2e_addr);
#line 952 "UecDirectTransfer.cpp"
  *((&((&((&llvm_cbe_tmp193->field4))->field0))->field0)) = llvm_cbe_tmp192;
#line 954 "UecDirectTransfer.cpp"
  *(&llvm_cbe_retval) = 1u;
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_cleanup_2e_dst) = 1u;
#line 954 "UecDirectTransfer.cpp"
  goto llvm_cbe_cleanup;

llvm_cbe_sw_2e_default:
#line 932 "UecDirectTransfer.cpp"
  llvm_cbe_call93 = _ZN11DummyStreamlsIPKcEERS_T_((&mout), ((&_OC_str64.array[((signed int )0u)])));
#line 932 "UecDirectTransfer.cpp"
  llvm_cbe_call95 = _ZN11DummyStreamlsIPFRSt8ios_baseS2_EEERS_T_(llvm_cbe_call93, _ZSt3decRSt8ios_base);
#line 932 "UecDirectTransfer.cpp"
  llvm_cbe_call97 = _ZNK3Uec14UecServiceBase13getUecManagerEv((((struct l_class_OC_Uec_KD__KD_UecServiceBase *)llvm_cbe_this1)));
#line 932 "UecDirectTransfer.cpp"
  llvm_cbe_call99 = _ZNK3Uec14UecManagerBase13getInstanceIdEv(llvm_cbe_call97);
#line 932 "UecDirectTransfer.cpp"
  llvm_cbe_call101 = _ZN11DummyStreamlsIjEERS_T_(llvm_cbe_call95, llvm_cbe_call99);
#line 932 "UecDirectTransfer.cpp"
  llvm_cbe_call103 = _ZN11DummyStreamlsIPKcEERS_T_(llvm_cbe_call101, ((&_OC_str65.array[((signed int )0u)])));
#line 932 "UecDirectTransfer.cpp"
  llvm_cbe_call105 = _ZN11DummyStreamlsIPFRSt8ios_baseS2_EEERS_T_(llvm_cbe_call103, _ZSt3decRSt8ios_base);
#line 932 "UecDirectTransfer.cpp"
  llvm_cbe_tmp106 = *(&llvm_cbe_l_asn1PayLoad_ptr);
#line 932 "UecDirectTransfer.cpp"
  llvm_cbe_tmp114 = *((&((&((&((&((&((&((&llvm_cbe_tmp106->field0))->field1))->field0))->field1))->field0))->field0))->field0));
#line 932 "UecDirectTransfer.cpp"
  llvm_cbe_call116 = _ZN11DummyStreamlsI34EDiscUErrcDedicatedInformationTypeEERS_T_(llvm_cbe_call105, llvm_cbe_tmp114);
#line 937 "UecDirectTransfer.cpp"
  *(&llvm_cbe_retval) = 4294967295u;
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_cleanup_2e_dst) = 2u;
#line 937 "UecDirectTransfer.cpp"
  goto llvm_cbe_cleanup;

llvm_cbe_cleanup_2e_pad:
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_cleanup_2e_dst199) = 1u;
#line 0 "LLVM INTERNAL"
  goto llvm_cbe_cleanup200;

llvm_cbe_cleanup_2e_pad197:
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_cleanup_2e_dst199) = 1u;
#line 0 "LLVM INTERNAL"
  goto llvm_cbe_cleanup200;

llvm_cbe_cleanup:
#line 955 "UecDirectTransfer.cpp"
  _ZN3Uec17UecDebugCallstackD1Ev((&llvm_cbe__callstackTmp_));
#line 955 "UecDirectTransfer.cpp"
  llvm_cbe_tmp198 = *(&llvm_cbe_cleanup_2e_dst);
#line 955 "UecDirectTransfer.cpp"
  switch (llvm_cbe_tmp198) {
  default:
    goto llvm_cbe_cleanup_2e_end;;
  case 1u:
    goto llvm_cbe_cleanup_2e_pad;  case 2u:
    goto llvm_cbe_cleanup_2e_pad197;  }

llvm_cbe_cleanup_2e_end:
#line 955 "UecDirectTransfer.cpp"
  *(&llvm_cbe_cleanup_2e_dst199) = 0u;
#line 955 "UecDirectTransfer.cpp"
  goto llvm_cbe_cleanup200;

llvm_cbe_cleanup200:
#line 955 "UecDirectTransfer.cpp"
  _ZNSsD1Ev((&llvm_cbe__currentFunction_));
#line 955 "UecDirectTransfer.cpp"
  llvm_cbe_tmp201 = *(&llvm_cbe_cleanup_2e_dst199);
#line 955 "UecDirectTransfer.cpp"
  llvm_cbe_tmp__141 = *(&llvm_cbe_retval);
#line 955 "UecDirectTransfer.cpp"
  return llvm_cbe_tmp__141;
}


#line 0 "LLVM INTERNAL"
bool _ZN3Uec16UecUeContextData14isMmeConnectedEv(struct l_class_OC_Uec_KD__KD_UecUeContextData *llvm_cbe_this) {
  bool llvm_cbe_retval;    /* Address-exposed local */
  struct l_class_OC_Uec_KD__KD_UecUeContextData *llvm_cbe_this_2e_addr;    /* Address-exposed local */
  struct l_class_OC_Uec_KD__KD_UecUeContextData *llvm_cbe_this1;
  unsigned int llvm_cbe_tmp3;
  bool llvm_cbe_tmp__142;

#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_this_2e_addr) = llvm_cbe_this;
#line 0 "LLVM INTERNAL"
  llvm_cbe_this1 = *(&llvm_cbe_this_2e_addr);
#line 869 "UecUeContextData.hpp"
  llvm_cbe_tmp3 = *((&((&llvm_cbe_this1->field1))->field1));
#line 869 "UecUeContextData.hpp"
  *(&llvm_cbe_retval) = ((((llvm_cbe_tmp3 & 2u) != 0u)) & 1);
#line 870 "UecUeContextData.hpp"
  llvm_cbe_tmp__142 = ((*(&llvm_cbe_retval))&1);
#line 870 "UecUeContextData.hpp"
  return llvm_cbe_tmp__142;
}


#line 0 "LLVM INTERNAL"
unsigned int _ZNK3Uec8UecEvent16getAsn1PayLoadIdEv(struct l_class_OC_Uec_KD__KD_UecEvent *llvm_cbe_this) {
  unsigned int llvm_cbe_retval;    /* Address-exposed local */
  struct l_class_OC_Uec_KD__KD_UecEvent *llvm_cbe_this_2e_addr;    /* Address-exposed local */
  struct l_class_OC_Uec_KD__KD_UecEvent *llvm_cbe_this1;
  unsigned int llvm_cbe_tmp2;
  unsigned int llvm_cbe_tmp__143;

#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_this_2e_addr) = llvm_cbe_this;
#line 0 "LLVM INTERNAL"
  llvm_cbe_this1 = *(&llvm_cbe_this_2e_addr);
#line 361 "UecEvent.hpp"
  llvm_cbe_tmp2 = *((&llvm_cbe_this1->field5));
#line 361 "UecEvent.hpp"
  *(&llvm_cbe_retval) = llvm_cbe_tmp2;
#line 361 "UecEvent.hpp"
  llvm_cbe_tmp__143 = *(&llvm_cbe_retval);
#line 361 "UecEvent.hpp"
  return llvm_cbe_tmp__143;
}


#line 0 "LLVM INTERNAL"
unsigned int _ZNK3Uec11UecCellData6getTacEv(struct l_class_OC_Uec_KD__KD_UecCellData *llvm_cbe_this) {
  unsigned int llvm_cbe_retval;    /* Address-exposed local */
  struct l_class_OC_Uec_KD__KD_UecCellData *llvm_cbe_this_2e_addr;    /* Address-exposed local */
  struct l_class_OC_Uec_KD__KD_UecCellData *llvm_cbe_this1;
  unsigned int llvm_cbe_tmp4;
  unsigned int llvm_cbe_tmp__144;

#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_this_2e_addr) = llvm_cbe_this;
#line 0 "LLVM INTERNAL"
  llvm_cbe_this1 = *(&llvm_cbe_this_2e_addr);
#line 222 "UecCellData.hpp"
  llvm_cbe_tmp4 = *((&((&((&llvm_cbe_this1->field1))->field4))->field0));
#line 222 "UecCellData.hpp"
  *(&llvm_cbe_retval) = llvm_cbe_tmp4;
#line 223 "UecCellData.hpp"
  llvm_cbe_tmp__144 = *(&llvm_cbe_retval);
#line 223 "UecCellData.hpp"
  return llvm_cbe_tmp__144;
}


#line 0 "LLVM INTERNAL"
struct l_struct_OC_SS1apUEAggregateMaximumBitrate _ZNK3Uec16UecUeContextData11getS1GummeiEv(struct l_class_OC_Uec_KD__KD_UecUeContextData *llvm_cbe_this) {
  struct l_struct_OC_SS1apGUMMEI llvm_cbe_retval;    /* Address-exposed local */
  struct l_class_OC_Uec_KD__KD_UecUeContextData *llvm_cbe_this_2e_addr;    /* Address-exposed local */
  unsigned int llvm_cbe_shift;    /* Address-exposed local */
  unsigned int llvm_cbe_u32Val;    /* Address-exposed local */
  struct l_class_OC_Uec_KD__KD_UecUeContextData *llvm_cbe_this1;
  unsigned int llvm_cbe_tmp4;
  unsigned char *llvm_cbe_tmp__145;
  unsigned char *llvm_cbe_tmp__146;
  unsigned short llvm_cbe_tmp20;
  unsigned int llvm_cbe_tmp22;
  unsigned int llvm_cbe_tmp23;
  unsigned int llvm_cbe_tmp24;
  unsigned int llvm_cbe_tmp28;
  unsigned char llvm_cbe_tmp39;
  struct l_struct_OC_SS1apUEAggregateMaximumBitrate llvm_cbe_tmp__147;

#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_this_2e_addr) = llvm_cbe_this;
#line 0 "LLVM INTERNAL"
  llvm_cbe_this1 = *(&llvm_cbe_this_2e_addr);
#line 1806 "UecUeContextData.hpp"
  llvm_cbe_tmp4 = *((&((&((&llvm_cbe_this1->field1))->field25))->field1));
#line 1806 "UecUeContextData.hpp"
  if ((llvm_cbe_tmp4 != 0u)) {    goto llvm_cbe_if_2e_else;  } else {    goto llvm_cbe_if_2e_then;  }


llvm_cbe_if_2e_then:
#line 0 "LLVM INTERNAL"
  llvm_cbe_tmp__145 = memset((((unsigned char *)(&llvm_cbe_retval))), 0u, 16ull);
#line 1809 "UecUeContextData.hpp"
  *((&((&llvm_cbe_retval.field0))->field0)) = 2u;
#line 1810 "UecUeContextData.hpp"
  goto llvm_cbe_if_2e_end;

llvm_cbe_if_2e_else:
#line 0 "LLVM INTERNAL"
  llvm_cbe_tmp__146 = memcpy((((unsigned char *)((&llvm_cbe_retval.field0)))), (((unsigned char *)((&((&((&((&llvm_cbe_this1->field1))->field25))->field2))->field0)))), 12ull);
#line 1813 "UecUeContextData.hpp"
  llvm_cbe_tmp20 = *((&((&((&((&llvm_cbe_this1->field1))->field25))->field2))->field1));
#line 1813 "UecUeContextData.hpp"
  *(&llvm_cbe_u32Val) = (((unsigned int )(unsigned short )llvm_cbe_tmp20));
#line 1813 "UecUeContextData.hpp"
  *(&llvm_cbe_shift) = 0u;
#line 1813 "UecUeContextData.hpp"
  llvm_cbe_tmp22 = *(&llvm_cbe_u32Val);
#line 1813 "UecUeContextData.hpp"
  llvm_cbe_tmp23 = *(&llvm_cbe_shift);
#line 1813 "UecUeContextData.hpp"
  *(&llvm_cbe_u32Val) = (llvm_cbe_tmp22 << llvm_cbe_tmp23);
#line 1813 "UecUeContextData.hpp"
  llvm_cbe_tmp24 = *(&llvm_cbe_u32Val);
#line 1813 "UecUeContextData.hpp"
  *((&(*((&(*((&((&llvm_cbe_retval.field1))->field0))).array[((signed int )0u)]))))) = (((unsigned char )((((unsigned int )(((unsigned int )llvm_cbe_tmp24) >> ((unsigned int )8u)))) & 255u)));
#line 1813 "UecUeContextData.hpp"
  llvm_cbe_tmp28 = *(&llvm_cbe_u32Val);
#line 1813 "UecUeContextData.hpp"
  *((&((&(*((&((&llvm_cbe_retval.field1))->field0))).array[((signed int )0u)]))[((signed long long )1ull)])) = (((unsigned char )(llvm_cbe_tmp28 & 255u)));
#line 1814 "UecUeContextData.hpp"
  llvm_cbe_tmp39 = *((&((&((&((&llvm_cbe_this1->field1))->field25))->field2))->field2));
#line 1814 "UecUeContextData.hpp"
  *((&llvm_cbe_retval.field2)) = llvm_cbe_tmp39;
#line 1815 "UecUeContextData.hpp"
  goto llvm_cbe_if_2e_end;

llvm_cbe_if_2e_end:
#line 1817 "UecUeContextData.hpp"
  llvm_cbe_tmp__147 = ((struct __attribute__ ((packed, aligned(1))) {struct l_struct_OC_SS1apUEAggregateMaximumBitrate data; } *)(((struct l_struct_OC_SS1apUEAggregateMaximumBitrate *)(&llvm_cbe_retval))))->data;
#line 1817 "UecUeContextData.hpp"
  return llvm_cbe_tmp__147;
}


#line 0 "LLVM INTERNAL"
unsigned int _ZNK3Uec11UecCellData9getCellIdEv(struct l_class_OC_Uec_KD__KD_UecCellData *llvm_cbe_this) {
  unsigned int llvm_cbe_retval;    /* Address-exposed local */
  struct l_class_OC_Uec_KD__KD_UecCellData *llvm_cbe_this_2e_addr;    /* Address-exposed local */
  struct l_class_OC_Uec_KD__KD_UecCellData *llvm_cbe_this1;
  unsigned int llvm_cbe_tmp3;
  unsigned int llvm_cbe_tmp__148;

#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_this_2e_addr) = llvm_cbe_this;
#line 0 "LLVM INTERNAL"
  llvm_cbe_this1 = *(&llvm_cbe_this_2e_addr);
#line 185 "UecCellData.hpp"
  llvm_cbe_tmp3 = *((&((&llvm_cbe_this1->field1))->field1));
#line 185 "UecCellData.hpp"
  *(&llvm_cbe_retval) = llvm_cbe_tmp3;
#line 186 "UecCellData.hpp"
  llvm_cbe_tmp__148 = *(&llvm_cbe_retval);
#line 186 "UecCellData.hpp"
  return llvm_cbe_tmp__148;
}


#line 0 "LLVM INTERNAL"
struct l_struct_OC_SAmRlcPbTab *_ZNK3Uec11UecCellData12getPlmnRestLEj(struct l_class_OC_Uec_KD__KD_UecCellData *llvm_cbe_this, unsigned int llvm_cbe_i) {
  struct l_struct_OC_SAmRlcPbTab *llvm_cbe_retval;    /* Address-exposed local */
  struct l_class_OC_Uec_KD__KD_UecCellData *llvm_cbe_this_2e_addr;    /* Address-exposed local */
  unsigned int llvm_cbe_i_2e_addr;    /* Address-exposed local */
  struct l_class_OC_Uec_KD__KD_UecCellData *llvm_cbe_this1;
  unsigned int llvm_cbe_tmp;
  struct l_struct_OC_SAmRlcPbTab *llvm_cbe_tmp__149;

#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_this_2e_addr) = llvm_cbe_this;
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_i_2e_addr) = llvm_cbe_i;
#line 0 "LLVM INTERNAL"
  llvm_cbe_this1 = *(&llvm_cbe_this_2e_addr);
#line 248 "UecCellData.hpp"
  llvm_cbe_tmp = *(&llvm_cbe_i_2e_addr);
#line 248 "UecCellData.hpp"
  *(&llvm_cbe_retval) = ((&((&(*((&((&((&llvm_cbe_this1->field1))->field4))->field2))).array[((signed int )0u)]))[((signed long long )(((unsigned long long )(unsigned int )llvm_cbe_tmp)))]));
#line 249 "UecCellData.hpp"
  llvm_cbe_tmp__149 = *(&llvm_cbe_retval);
#line 249 "UecCellData.hpp"
  return llvm_cbe_tmp__149;
}


#line 0 "LLVM INTERNAL"
bool _ZNK3Uec16UecUeContextData24isValidInitialUeIdentityEv(struct l_class_OC_Uec_KD__KD_UecUeContextData *llvm_cbe_this) {
  bool llvm_cbe_retval;    /* Address-exposed local */
  struct l_class_OC_Uec_KD__KD_UecUeContextData *llvm_cbe_this_2e_addr;    /* Address-exposed local */
  struct l_class_OC_Uec_KD__KD_UecUeContextData *llvm_cbe_this1;
  unsigned char llvm_cbe_tmp3;
  bool llvm_cbe_tmp__150;

#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_this_2e_addr) = llvm_cbe_this;
#line 0 "LLVM INTERNAL"
  llvm_cbe_this1 = *(&llvm_cbe_this_2e_addr);
#line 1022 "UecUeContextData.hpp"
  llvm_cbe_tmp3 = *((&((&llvm_cbe_this1->field1))->field8));
#line 1022 "UecUeContextData.hpp"
  *(&llvm_cbe_retval) = (((((((bool )llvm_cbe_tmp3&1u))&1))) & 1);
#line 1023 "UecUeContextData.hpp"
  llvm_cbe_tmp__150 = ((*(&llvm_cbe_retval))&1);
#line 1023 "UecUeContextData.hpp"
  return llvm_cbe_tmp__150;
}


#line 0 "LLVM INTERNAL"
struct l_struct_OC_UErrcInitialUEIdentity *_ZNK3Uec16UecUeContextData20getInitialUeIdentityEv(struct l_class_OC_Uec_KD__KD_UecUeContextData *llvm_cbe_this) {
  struct l_struct_OC_UErrcInitialUEIdentity *llvm_cbe_retval;    /* Address-exposed local */
  struct l_class_OC_Uec_KD__KD_UecUeContextData *llvm_cbe_this_2e_addr;    /* Address-exposed local */
  struct l_class_OC_Uec_KD__KD_UecUeContextData *llvm_cbe_this1;
  struct l_struct_OC_UErrcInitialUEIdentity *llvm_cbe_tmp__151;

#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_this_2e_addr) = llvm_cbe_this;
#line 0 "LLVM INTERNAL"
  llvm_cbe_this1 = *(&llvm_cbe_this_2e_addr);
#line 1011 "UecUeContextData.hpp"
  *(&llvm_cbe_retval) = ((&((&llvm_cbe_this1->field1))->field9));
#line 1012 "UecUeContextData.hpp"
  llvm_cbe_tmp__151 = *(&llvm_cbe_retval);
#line 1012 "UecUeContextData.hpp"
  return llvm_cbe_tmp__151;
}


#line 0 "LLVM INTERNAL"
unsigned int _ZN3Uec12UecParamConv26convErrcEstablishmentCauseE23EErrcEstablishmentCause(unsigned int llvm_cbe_p_enum) {
  unsigned int llvm_cbe_retval;    /* Address-exposed local */
  unsigned int llvm_cbe_p_enum_2e_addr;    /* Address-exposed local */
  unsigned int llvm_cbe_tmp;
  struct l_class_OC_BaseEvent *llvm_cbe_call;
  unsigned int llvm_cbe_tmp5;
  struct l_class_OC_BaseEvent *llvm_cbe_call6;
  unsigned int llvm_cbe_tmp__152;

#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_p_enum_2e_addr) = llvm_cbe_p_enum;
#line 1562 "UecCallProcParams.hpp"
  llvm_cbe_tmp = *(&llvm_cbe_p_enum_2e_addr);
#line 1562 "UecCallProcParams.hpp"
  switch (llvm_cbe_tmp) {
  default:
    goto llvm_cbe_sw_2e_default;;
  case 0u:
    goto llvm_cbe_sw_2e_bb;    break;
  case 1u:
    goto llvm_cbe_sw_2e_bb1;  case 2u:
    goto llvm_cbe_sw_2e_bb2;  case 3u:
    goto llvm_cbe_sw_2e_bb3;  case 4u:
    goto llvm_cbe_sw_2e_bb4;  }

llvm_cbe_sw_2e_bb:
#line 1564 "UecCallProcParams.hpp"
  *(&llvm_cbe_retval) = 0u;
#line 1564 "UecCallProcParams.hpp"
  goto llvm_cbe_return;

llvm_cbe_sw_2e_bb1:
#line 1565 "UecCallProcParams.hpp"
  *(&llvm_cbe_retval) = 1u;
#line 1565 "UecCallProcParams.hpp"
  goto llvm_cbe_return;

llvm_cbe_sw_2e_bb2:
#line 1566 "UecCallProcParams.hpp"
  *(&llvm_cbe_retval) = 2u;
#line 1566 "UecCallProcParams.hpp"
  goto llvm_cbe_return;

llvm_cbe_sw_2e_bb3:
#line 1567 "UecCallProcParams.hpp"
  *(&llvm_cbe_retval) = 3u;
#line 1567 "UecCallProcParams.hpp"
  goto llvm_cbe_return;

llvm_cbe_sw_2e_bb4:
#line 1568 "UecCallProcParams.hpp"
  *(&llvm_cbe_retval) = 4u;
#line 1568 "UecCallProcParams.hpp"
  goto llvm_cbe_return;

llvm_cbe_sw_2e_default:
#line 1569 "UecCallProcParams.hpp"
  llvm_cbe_call = _ZN11DummyStreamlsIPKcEERS_T_((&mout), ((&_OC_str95.array[((signed int )0u)])));
#line 1569 "UecCallProcParams.hpp"
  llvm_cbe_tmp5 = *(&llvm_cbe_p_enum_2e_addr);
#line 1569 "UecCallProcParams.hpp"
  llvm_cbe_call6 = _ZN11DummyStreamlsI23EErrcEstablishmentCauseEERS_T_(llvm_cbe_call, llvm_cbe_tmp5);
#line 1569 "UecCallProcParams.hpp"
  *(&llvm_cbe_retval) = 0u;
#line 1569 "UecCallProcParams.hpp"
  goto llvm_cbe_return;

llvm_cbe_return:
#line 1571 "UecCallProcParams.hpp"
  llvm_cbe_tmp__152 = *(&llvm_cbe_retval);
#line 1571 "UecCallProcParams.hpp"
  return llvm_cbe_tmp__152;
}


#line 0 "LLVM INTERNAL"
unsigned int *_ZNK3Uec16UecUeContextData21getEstablishmentCauseEv(struct l_class_OC_Uec_KD__KD_UecUeContextData *llvm_cbe_this) {
  unsigned int *llvm_cbe_retval;    /* Address-exposed local */
  struct l_class_OC_Uec_KD__KD_UecUeContextData *llvm_cbe_this_2e_addr;    /* Address-exposed local */
  struct l_class_OC_Uec_KD__KD_UecUeContextData *llvm_cbe_this1;
  unsigned int *llvm_cbe_tmp__153;

#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_this_2e_addr) = llvm_cbe_this;
#line 0 "LLVM INTERNAL"
  llvm_cbe_this1 = *(&llvm_cbe_this_2e_addr);
#line 1030 "UecUeContextData.hpp"
  *(&llvm_cbe_retval) = ((&((&llvm_cbe_this1->field1))->field10));
#line 1031 "UecUeContextData.hpp"
  llvm_cbe_tmp__153 = *(&llvm_cbe_retval);
#line 1031 "UecUeContextData.hpp"
  return llvm_cbe_tmp__153;
}


#line 0 "LLVM INTERNAL"
unsigned int _ZNK3Uec16UecUeContextData14getNasDeliveryEv(struct l_class_OC_Uec_KD__KD_UecUeContextData *llvm_cbe_this) {
  unsigned int llvm_cbe_retval;    /* Address-exposed local */
  struct l_class_OC_Uec_KD__KD_UecUeContextData *llvm_cbe_this_2e_addr;    /* Address-exposed local */
  struct l_class_OC_Uec_KD__KD_UecUeContextData *llvm_cbe_this1;
  unsigned int llvm_cbe_tmp3;
  unsigned int llvm_cbe_tmp__154;

#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_this_2e_addr) = llvm_cbe_this;
#line 0 "LLVM INTERNAL"
  llvm_cbe_this1 = *(&llvm_cbe_this_2e_addr);
#line 2334 "UecUeContextData.hpp"
  llvm_cbe_tmp3 = *((&((&llvm_cbe_this1->field1))->field22));
#line 2334 "UecUeContextData.hpp"
  *(&llvm_cbe_retval) = llvm_cbe_tmp3;
#line 2335 "UecUeContextData.hpp"
  llvm_cbe_tmp__154 = *(&llvm_cbe_retval);
#line 2335 "UecUeContextData.hpp"
  return llvm_cbe_tmp__154;
}


#line 0 "LLVM INTERNAL"
unsigned int _ZNK3Uec16UecUeContextData8getCrntiEv(struct l_class_OC_Uec_KD__KD_UecUeContextData *llvm_cbe_this) {
  unsigned int llvm_cbe_retval;    /* Address-exposed local */
  struct l_class_OC_Uec_KD__KD_UecUeContextData *llvm_cbe_this_2e_addr;    /* Address-exposed local */
  struct l_class_OC_Uec_KD__KD_UecUeContextData *llvm_cbe_this1;
  unsigned int llvm_cbe_tmp3;
  unsigned int llvm_cbe_tmp__155;

#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_this_2e_addr) = llvm_cbe_this;
#line 0 "LLVM INTERNAL"
  llvm_cbe_this1 = *(&llvm_cbe_this_2e_addr);
#line 928 "UecUeContextData.hpp"
  llvm_cbe_tmp3 = *((&((&llvm_cbe_this1->field1))->field4));
#line 928 "UecUeContextData.hpp"
  *(&llvm_cbe_retval) = llvm_cbe_tmp3;
#line 929 "UecUeContextData.hpp"
  llvm_cbe_tmp__155 = *(&llvm_cbe_retval);
#line 929 "UecUeContextData.hpp"
  return llvm_cbe_tmp__155;
}


#line 0 "LLVM INTERNAL"
bool _ZN3Uec16UecUeContextData10isSbActiveEj(struct l_class_OC_Uec_KD__KD_UecUeContextData *llvm_cbe_this, unsigned int llvm_cbe_i) {
  bool llvm_cbe_retval;    /* Address-exposed local */
  struct l_class_OC_Uec_KD__KD_UecUeContextData *llvm_cbe_this_2e_addr;    /* Address-exposed local */
  unsigned int llvm_cbe_i_2e_addr;    /* Address-exposed local */
  struct l_class_OC_Uec_KD__KD_UecUeContextData *llvm_cbe_this1;
  unsigned int llvm_cbe_tmp;
  unsigned char llvm_cbe_tmp6;
  bool llvm_cbe_tmp__156;

#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_this_2e_addr) = llvm_cbe_this;
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_i_2e_addr) = llvm_cbe_i;
#line 0 "LLVM INTERNAL"
  llvm_cbe_this1 = *(&llvm_cbe_this_2e_addr);
#line 1897 "UecUeContextData.hpp"
  llvm_cbe_tmp = *(&llvm_cbe_i_2e_addr);
#line 1897 "UecUeContextData.hpp"
  llvm_cbe_tmp6 = *((&((&((&(*((&((&((&llvm_cbe_this1->field1))->field23))->field3))).array[((signed int )0u)]))[((signed long long )(((unsigned long long )(unsigned int )llvm_cbe_tmp)))]))->field0));
#line 1897 "UecUeContextData.hpp"
  *(&llvm_cbe_retval) = (((((((bool )llvm_cbe_tmp6&1u))&1))) & 1);
#line 1898 "UecUeContextData.hpp"
  llvm_cbe_tmp__156 = ((*(&llvm_cbe_retval))&1);
#line 1898 "UecUeContextData.hpp"
  return llvm_cbe_tmp__156;
}


#line 0 "LLVM INTERNAL"
unsigned int _ZN3Uec17UecDirectTransfer11storeNasPduEPNS_8UecEventE(struct l_class_OC_Uec_KD__KD_UecDirectTransfer *llvm_cbe_this, struct l_class_OC_Uec_KD__KD_UecEvent *llvm_cbe_p_uecEvent_ptr) {
  unsigned int llvm_cbe_retval;    /* Address-exposed local */
  struct l_class_OC_Uec_KD__KD_UecDirectTransfer *llvm_cbe_this_2e_addr;    /* Address-exposed local */
  struct l_class_OC_Uec_KD__KD_UecEvent *llvm_cbe_p_uecEvent_ptr_2e_addr;    /* Address-exposed local */
  struct l_class_OC_std_KD__KD_basic_string llvm_cbe__currentFunction_;    /* Address-exposed local */
  struct l_class_OC_BaseEvent llvm_cbe_agg_2e_tmp;    /* Address-exposed local */
  struct l_class_OC_Uec_KD__KD_UecDebugCallstack llvm_cbe__callstackTmp_;    /* Address-exposed local */
  struct l_class_OC_Uec_KD__KD_UecUeContextData *llvm_cbe_l_uecUeContextDataWrite_ptr;    /* Address-exposed local */
  struct l_struct_OC_SS1apUFDownlinkNASTransport *llvm_cbe_l_asn1PayLoad_ptr;    /* Address-exposed local */
  unsigned int llvm_cbe_cleanup_2e_dst;    /* Address-exposed local */
  unsigned int llvm_cbe_cleanup_2e_dst42;    /* Address-exposed local */
  struct l_class_OC_Uec_KD__KD_UecDirectTransfer *llvm_cbe_this1;
  struct l_class_OC_Uec_KD__KD_UecBaseData *llvm_cbe_call;
  struct l_class_OC_Uec_KD__KD_UecBaseData * (**llvm_cbe_tmp__157) (struct l_class_OC_Uec_KD__KD_UecBaseData *, unsigned int );
  struct l_class_OC_Uec_KD__KD_UecBaseData * (*llvm_cbe_tmp__158) (struct l_class_OC_Uec_KD__KD_UecBaseData *, unsigned int );
  struct l_class_OC_Uec_KD__KD_UecBaseData *llvm_cbe_call15;
  struct l_class_OC_Uec_KD__KD_UecEvent *llvm_cbe_tmp;
  unsigned char *llvm_cbe_call17;
  struct l_class_OC_Uec_KD__KD_UecUeContextData *llvm_cbe_tmp18;
  struct l_struct_OC_SS1apUFDownlinkNASTransport *llvm_cbe_tmp19;
  struct l_class_OC_BaseEvent *llvm_cbe_call23;
  struct l_class_OC_BaseEvent *llvm_cbe_call25;
  struct l_class_OC_Uec_KD__KD_UecEvent *llvm_cbe_tmp26;
  unsigned int llvm_cbe_call28;
  struct l_class_OC_BaseEvent *llvm_cbe_call30;
  struct l_class_OC_BaseEvent *llvm_cbe_call32;
  struct l_class_OC_BaseEvent *llvm_cbe_call34;
  struct l_struct_OC_SS1apUFDownlinkNASTransport *llvm_cbe_tmp35;
  unsigned int llvm_cbe_tmp38;
  struct l_class_OC_BaseEvent *llvm_cbe_call40;
  unsigned int llvm_cbe_tmp41;
  unsigned int llvm_cbe_tmp44;
  unsigned int llvm_cbe_tmp__159;

#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_this_2e_addr) = llvm_cbe_this;
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_p_uecEvent_ptr_2e_addr) = llvm_cbe_p_uecEvent_ptr;
#line 0 "LLVM INTERNAL"
  llvm_cbe_this1 = *(&llvm_cbe_this_2e_addr);
#line 1038 "UecDirectTransfer.cpp"
  _ZNSaIcEC1Ev((&llvm_cbe_agg_2e_tmp));
#line 1038 "UecDirectTransfer.cpp"
  _ZNSsC1EPKcRKSaIcE((&llvm_cbe__currentFunction_), ((&__PRETTY_FUNCTION___OC__ZN3Uec17UecDirectTransfer11storeNasPduEPNS_8UecEventE.array[((signed int )0u)])), (&llvm_cbe_agg_2e_tmp));
#line 1038 "UecDirectTransfer.cpp"
  _ZNSaIcED1Ev((&llvm_cbe_agg_2e_tmp));
#line 1038 "UecDirectTransfer.cpp"
  _ZN3Uec17UecDebugCallstackC1ERKSs((&llvm_cbe__callstackTmp_), (&llvm_cbe__currentFunction_));
#line 1041 "UecDirectTransfer.cpp"
  llvm_cbe_call = _ZNK3Uec14UecServiceBase16getUecDataAccessEv((((struct l_class_OC_Uec_KD__KD_UecServiceBase *)llvm_cbe_this1)));
#line 1041 "UecDirectTransfer.cpp"
  llvm_cbe_tmp__157 = *(((struct l_class_OC_Uec_KD__KD_UecBaseData * (***) (struct l_class_OC_Uec_KD__KD_UecBaseData *, unsigned int ))llvm_cbe_call));
#line 1041 "UecDirectTransfer.cpp"
  llvm_cbe_tmp__158 = *((&llvm_cbe_tmp__157[((signed long long )2ull)]));
#line 1041 "UecDirectTransfer.cpp"
  llvm_cbe_call15 = llvm_cbe_tmp__158(llvm_cbe_call, 1u);
#line 1041 "UecDirectTransfer.cpp"
  *(&llvm_cbe_l_uecUeContextDataWrite_ptr) = (((struct l_class_OC_Uec_KD__KD_UecUeContextData *)llvm_cbe_call15));
#line 1044 "UecDirectTransfer.cpp"
  llvm_cbe_tmp = *(&llvm_cbe_p_uecEvent_ptr_2e_addr);
#line 1044 "UecDirectTransfer.cpp"
  llvm_cbe_call17 = _ZNK3Uec8UecEvent14getAsn1PayLoadEv(llvm_cbe_tmp);
#line 1044 "UecDirectTransfer.cpp"
  *(&llvm_cbe_l_asn1PayLoad_ptr) = (((struct l_struct_OC_SS1apUFDownlinkNASTransport *)llvm_cbe_call17));
#line 1046 "UecDirectTransfer.cpp"
  llvm_cbe_tmp18 = *(&llvm_cbe_l_uecUeContextDataWrite_ptr);
#line 1046 "UecDirectTransfer.cpp"
  llvm_cbe_tmp19 = *(&llvm_cbe_l_asn1PayLoad_ptr);
#line 1046 "UecDirectTransfer.cpp"
  _ZN3Uec16UecUeContextData12insertNasPduERK10SAsnDynstr(llvm_cbe_tmp18, ((&llvm_cbe_tmp19->field2)));
#line 1048 "UecDirectTransfer.cpp"
  llvm_cbe_call23 = _ZN11DummyStreamlsIPKcEERS_T_((&mout), ((&_OC_str66.array[((signed int )0u)])));
#line 1048 "UecDirectTransfer.cpp"
  llvm_cbe_call25 = _ZN11DummyStreamlsIPFRSt8ios_baseS2_EEERS_T_(llvm_cbe_call23, _ZSt3decRSt8ios_base);
#line 1048 "UecDirectTransfer.cpp"
  llvm_cbe_tmp26 = *(&llvm_cbe_p_uecEvent_ptr_2e_addr);
#line 1048 "UecDirectTransfer.cpp"
  llvm_cbe_call28 = _ZNK3Uec8UecEvent13getInstanceIdEv(llvm_cbe_tmp26);
#line 1048 "UecDirectTransfer.cpp"
  llvm_cbe_call30 = _ZN11DummyStreamlsIjEERS_T_(llvm_cbe_call25, llvm_cbe_call28);
#line 1048 "UecDirectTransfer.cpp"
  llvm_cbe_call32 = _ZN11DummyStreamlsIPKcEERS_T_(llvm_cbe_call30, ((&_OC_str67.array[((signed int )0u)])));
#line 1048 "UecDirectTransfer.cpp"
  llvm_cbe_call34 = _ZN11DummyStreamlsIPFRSt8ios_baseS2_EEERS_T_(llvm_cbe_call32, _ZSt3decRSt8ios_base);
#line 1048 "UecDirectTransfer.cpp"
  llvm_cbe_tmp35 = *(&llvm_cbe_l_asn1PayLoad_ptr);
#line 1048 "UecDirectTransfer.cpp"
  llvm_cbe_tmp38 = *((&((&llvm_cbe_tmp35->field2))->field0));
#line 1048 "UecDirectTransfer.cpp"
  llvm_cbe_call40 = _ZN11DummyStreamlsIjEERS_T_(llvm_cbe_call34, llvm_cbe_tmp38);
#line 1052 "UecDirectTransfer.cpp"
  *(&llvm_cbe_retval) = 1u;
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_cleanup_2e_dst) = 1u;
#line 1053 "UecDirectTransfer.cpp"
  _ZN3Uec17UecDebugCallstackD1Ev((&llvm_cbe__callstackTmp_));
#line 1053 "UecDirectTransfer.cpp"
  llvm_cbe_tmp41 = *(&llvm_cbe_cleanup_2e_dst);
#line 0 "LLVM INTERNAL"
  if ((llvm_cbe_tmp41 == 1u)) {    goto llvm_cbe_cleanup_2e_pad;  } else {    goto llvm_cbe_cleanup_2e_end;  }


llvm_cbe_cleanup_2e_pad:
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_cleanup_2e_dst42) = 1u;
#line 0 "LLVM INTERNAL"
  goto llvm_cbe_cleanup43;

llvm_cbe_cleanup_2e_end:
#line 1053 "UecDirectTransfer.cpp"
  *(&llvm_cbe_cleanup_2e_dst42) = 0u;
#line 1053 "UecDirectTransfer.cpp"
  goto llvm_cbe_cleanup43;

llvm_cbe_cleanup43:
#line 1053 "UecDirectTransfer.cpp"
  _ZNSsD1Ev((&llvm_cbe__currentFunction_));
#line 1053 "UecDirectTransfer.cpp"
  llvm_cbe_tmp44 = *(&llvm_cbe_cleanup_2e_dst42);
#line 1053 "UecDirectTransfer.cpp"
  llvm_cbe_tmp__159 = *(&llvm_cbe_retval);
#line 1053 "UecDirectTransfer.cpp"
  return llvm_cbe_tmp__159;
}


#line 0 "LLVM INTERNAL"
struct l_class_OC_BaseEvent *_ZN11DummyStreamlsIhEERS_T_(struct l_class_OC_BaseEvent *llvm_cbe_this, unsigned char llvm_cbe_arg) {
  struct l_class_OC_BaseEvent *llvm_cbe_retval;    /* Address-exposed local */
  struct l_class_OC_BaseEvent *llvm_cbe_this_2e_addr;    /* Address-exposed local */
  unsigned char llvm_cbe_arg_2e_addr;    /* Address-exposed local */
  struct l_class_OC_BaseEvent *llvm_cbe_this1;
  struct l_class_OC_BaseEvent *llvm_cbe_tmp__160;

#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_this_2e_addr) = llvm_cbe_this;
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_arg_2e_addr) = llvm_cbe_arg;
#line 0 "LLVM INTERNAL"
  llvm_cbe_this1 = *(&llvm_cbe_this_2e_addr);
#line 30 "BaseTypes.hpp"
  *(&llvm_cbe_retval) = llvm_cbe_this1;
#line 31 "BaseTypes.hpp"
  llvm_cbe_tmp__160 = *(&llvm_cbe_retval);
#line 31 "BaseTypes.hpp"
  return llvm_cbe_tmp__160;
}


#line 0 "LLVM INTERNAL"
void _ZN3Uec16UecUeContextData26setHandoverRestrictionListERK28SS1apHandoverRestrictionList(struct l_class_OC_Uec_KD__KD_UecUeContextData *llvm_cbe_this, struct l_struct_OC_SS1apHandoverRestrictionList *llvm_cbe_p_handoverRestrictionList) {
  struct l_class_OC_Uec_KD__KD_UecUeContextData *llvm_cbe_this_2e_addr;    /* Address-exposed local */
  struct l_struct_OC_SS1apHandoverRestrictionList *llvm_cbe_p_handoverRestrictionList_2e_addr;    /* Address-exposed local */
  unsigned int llvm_cbe_i;    /* Address-exposed local */
  unsigned int llvm_cbe_i168;    /* Address-exposed local */
  struct l_class_OC_Uec_KD__KD_UecUeContextData *llvm_cbe_this1;
  struct l_struct_OC_SS1apHandoverRestrictionList *llvm_cbe_tmp5;
  unsigned char *llvm_cbe_tmp__161;
  struct l_struct_OC_SS1apHandoverRestrictionList *llvm_cbe_tmp9;
  unsigned int llvm_cbe_tmp11;
  struct l_struct_OC_SS1apHandoverRestrictionList *llvm_cbe_tmp20;
  unsigned char *llvm_cbe_tmp__162;
  struct l_struct_OC_SS1apHandoverRestrictionList *llvm_cbe_tmp24;
  unsigned int llvm_cbe_tmp26;
  struct l_struct_OC_SS1apHandoverRestrictionList *llvm_cbe_tmp31;
  unsigned int llvm_cbe_tmp33;
  struct l_struct_OC_SS1apHandoverRestrictionList *llvm_cbe_tmp34;
  unsigned char llvm_cbe_tmp37;
  unsigned int llvm_cbe_tmp44;
  struct l_struct_OC_SS1apHandoverRestrictionList *llvm_cbe_tmp45;
  unsigned char llvm_cbe_tmp48;
  unsigned int llvm_cbe_tmp49;
  unsigned int llvm_cbe_tmp56;
  struct l_struct_OC_SS1apHandoverRestrictionList *llvm_cbe_tmp57;
  unsigned char *llvm_cbe_tmp__163;
  unsigned int llvm_cbe_tmp66;
  struct l_struct_OC_SS1apHandoverRestrictionList *llvm_cbe_tmp67;
  unsigned short llvm_cbe_tmp75;
  unsigned int llvm_cbe_tmp76;
  unsigned int llvm_cbe_tmp87;
  struct l_struct_OC_SS1apHandoverRestrictionList *llvm_cbe_tmp88;
  unsigned short llvm_cbe_tmp96;
  unsigned char *llvm_cbe_call;
  unsigned int llvm_cbe_tmp98;
  unsigned int llvm_cbe_tmp109;
  struct l_struct_OC_SErrcN4TxAntennaTm5 *llvm_cbe_tmp120;
  unsigned int llvm_cbe_tmp122;
  struct l_struct_OC_SS1apHandoverRestrictionList *llvm_cbe_tmp123;
  unsigned int llvm_cbe_tmp133;
  struct l_struct_OC_SS1apHandoverRestrictionList *llvm_cbe_tmp134;
  unsigned short llvm_cbe_tmp142;
  unsigned char *llvm_cbe_tmp__164;
  unsigned int llvm_cbe_tmp145;
  struct l_struct_OC_SS1apHandoverRestrictionList *llvm_cbe_tmp146;
  unsigned int llvm_cbe_tmp148;
  struct l_struct_OC_SS1apHandoverRestrictionList *llvm_cbe_tmp153;
  unsigned int llvm_cbe_tmp155;
  struct l_struct_OC_SS1apHandoverRestrictionList *llvm_cbe_tmp158;
  unsigned char llvm_cbe_tmp161;
  unsigned int llvm_cbe_tmp170;
  struct l_struct_OC_SS1apHandoverRestrictionList *llvm_cbe_tmp171;
  unsigned char llvm_cbe_tmp174;
  unsigned int llvm_cbe_tmp178;
  unsigned int llvm_cbe_tmp188;
  struct l_struct_OC_SS1apHandoverRestrictionList *llvm_cbe_tmp189;
  unsigned char *llvm_cbe_tmp__165;
  unsigned int llvm_cbe_tmp198;
  struct l_struct_OC_SS1apHandoverRestrictionList *llvm_cbe_tmp199;
  unsigned short llvm_cbe_tmp207;
  unsigned int llvm_cbe_tmp208;
  unsigned int llvm_cbe_tmp219;
  struct l_struct_OC_SS1apHandoverRestrictionList *llvm_cbe_tmp220;
  unsigned short llvm_cbe_tmp228;
  unsigned char *llvm_cbe_call230;
  unsigned int llvm_cbe_tmp231;
  unsigned int llvm_cbe_tmp242;
  struct l_struct_OC_SErrcN4TxAntennaTm5 *llvm_cbe_tmp253;
  unsigned int llvm_cbe_tmp255;
  struct l_struct_OC_SS1apHandoverRestrictionList *llvm_cbe_tmp256;
  unsigned int llvm_cbe_tmp266;
  struct l_struct_OC_SS1apHandoverRestrictionList *llvm_cbe_tmp267;
  unsigned short llvm_cbe_tmp275;
  unsigned char *llvm_cbe_tmp__166;
  unsigned int llvm_cbe_tmp280;
  struct l_struct_OC_SS1apHandoverRestrictionList *llvm_cbe_tmp284;
  unsigned int llvm_cbe_tmp286;
  struct l_struct_OC_SS1apHandoverRestrictionList *llvm_cbe_tmp291;
  unsigned int llvm_cbe_tmp293;

#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_this_2e_addr) = llvm_cbe_this;
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_p_handoverRestrictionList_2e_addr) = llvm_cbe_p_handoverRestrictionList;
#line 0 "LLVM INTERNAL"
  llvm_cbe_this1 = *(&llvm_cbe_this_2e_addr);
#line 2101 "UecUeContextData.hpp"
  _ZN3Uec16UecUeContextData28clearHandoverRestrictionListEv(llvm_cbe_this1);
#line 2103 "UecUeContextData.hpp"
  llvm_cbe_tmp5 = *(&llvm_cbe_p_handoverRestrictionList_2e_addr);
#line 0 "LLVM INTERNAL"
  llvm_cbe_tmp__161 = memcpy((((unsigned char *)((&((&((&((&llvm_cbe_this1->field1))->field25))->field13))->field0)))), (((unsigned char *)((&llvm_cbe_tmp5->field0)))), 12ull);
#line 2104 "UecUeContextData.hpp"
  llvm_cbe_tmp9 = *(&llvm_cbe_p_handoverRestrictionList_2e_addr);
#line 2104 "UecUeContextData.hpp"
  llvm_cbe_tmp11 = *((&llvm_cbe_tmp9->field1));
#line 2104 "UecUeContextData.hpp"
  *((&((&((&((&llvm_cbe_this1->field1))->field25))->field13))->field1)) = llvm_cbe_tmp11;
#line 2105 "UecUeContextData.hpp"
  llvm_cbe_tmp20 = *(&llvm_cbe_p_handoverRestrictionList_2e_addr);
#line 0 "LLVM INTERNAL"
  llvm_cbe_tmp__162 = memcpy((((unsigned char *)((&((&((&((&llvm_cbe_this1->field1))->field25))->field13))->field2)))), (((unsigned char *)((&llvm_cbe_tmp20->field2)))), 184ull);
#line 2106 "UecUeContextData.hpp"
  llvm_cbe_tmp24 = *(&llvm_cbe_p_handoverRestrictionList_2e_addr);
#line 2106 "UecUeContextData.hpp"
  llvm_cbe_tmp26 = *((&llvm_cbe_tmp24->field3));
#line 2106 "UecUeContextData.hpp"
  *((&((&((&((&llvm_cbe_this1->field1))->field25))->field13))->field3)) = llvm_cbe_tmp26;
#line 2107 "UecUeContextData.hpp"
  llvm_cbe_tmp31 = *(&llvm_cbe_p_handoverRestrictionList_2e_addr);
#line 2107 "UecUeContextData.hpp"
  llvm_cbe_tmp33 = *((&llvm_cbe_tmp31->field3));
#line 2107 "UecUeContextData.hpp"
  if ((llvm_cbe_tmp33 != 0u)) {    goto llvm_cbe_if_2e_then;  } else {    goto llvm_cbe_if_2e_end;  }


llvm_cbe_if_2e_then:
#line 2109 "UecUeContextData.hpp"
  llvm_cbe_tmp34 = *(&llvm_cbe_p_handoverRestrictionList_2e_addr);
#line 2109 "UecUeContextData.hpp"
  llvm_cbe_tmp37 = *((&((&llvm_cbe_tmp34->field4))->field0));
#line 2109 "UecUeContextData.hpp"
  *((&((&((&((&((&llvm_cbe_this1->field1))->field25))->field13))->field4))->field0)) = llvm_cbe_tmp37;
#line 2110 "UecUeContextData.hpp"
  *(&llvm_cbe_i) = 0u;
#line 2110 "UecUeContextData.hpp"
  goto llvm_cbe_for_2e_cond;
#line 2110 "UecUeContextData.hpp"
  do {     /* Syntactic loop 'for.cond' to make GCC happy */
llvm_cbe_for_2e_cond:
#line 2110 "UecUeContextData.hpp"
  llvm_cbe_tmp44 = *(&llvm_cbe_i);
#line 2110 "UecUeContextData.hpp"
  llvm_cbe_tmp45 = *(&llvm_cbe_p_handoverRestrictionList_2e_addr);
#line 2110 "UecUeContextData.hpp"
  llvm_cbe_tmp48 = *((&((&llvm_cbe_tmp45->field4))->field0));
#line 2110 "UecUeContextData.hpp"
  if ((((unsigned int )llvm_cbe_tmp44) < ((unsigned int )(((unsigned int )(unsigned char )llvm_cbe_tmp48))))) {    goto llvm_cbe_for_2e_body;  } else {    goto llvm_cbe_if_2e_end;  }


llvm_cbe_for_2e_body:
#line 2112 "UecUeContextData.hpp"
  llvm_cbe_tmp49 = *(&llvm_cbe_i);
#line 2112 "UecUeContextData.hpp"
  llvm_cbe_tmp56 = *(&llvm_cbe_i);
#line 2112 "UecUeContextData.hpp"
  llvm_cbe_tmp57 = *(&llvm_cbe_p_handoverRestrictionList_2e_addr);
#line 0 "LLVM INTERNAL"
  llvm_cbe_tmp__163 = memcpy((((unsigned char *)((&((&((&(*((&((&((&((&((&llvm_cbe_this1->field1))->field25))->field13))->field4))->field1))).array[((signed int )0u)]))[((signed long long )(((unsigned long long )(unsigned int )llvm_cbe_tmp49)))]))->field0)))), (((unsigned char *)((&((&((&(*((&((&llvm_cbe_tmp57->field4))->field4))).array[((signed int )0u)]))[((signed long long )(((unsigned long long )(unsigned int )llvm_cbe_tmp56)))]))->field0)))), 12ull);
#line 2113 "UecUeContextData.hpp"
  llvm_cbe_tmp66 = *(&llvm_cbe_i);
#line 2113 "UecUeContextData.hpp"
  llvm_cbe_tmp67 = *(&llvm_cbe_p_handoverRestrictionList_2e_addr);
#line 2113 "UecUeContextData.hpp"
  llvm_cbe_tmp75 = *((&((&((&((&(*((&((&llvm_cbe_tmp67->field4))->field4))).array[((signed int )0u)]))[((signed long long )(((unsigned long long )(unsigned int )llvm_cbe_tmp66)))]))->field1))->field0));
#line 2113 "UecUeContextData.hpp"
  llvm_cbe_tmp76 = *(&llvm_cbe_i);
#line 2113 "UecUeContextData.hpp"
  *((&((&((&((&(*((&((&((&((&((&llvm_cbe_this1->field1))->field25))->field13))->field4))->field1))).array[((signed int )0u)]))[((signed long long )(((unsigned long long )(unsigned int )llvm_cbe_tmp76)))]))->field1))->field0)) = llvm_cbe_tmp75;
#line 2114 "UecUeContextData.hpp"
  llvm_cbe_tmp87 = *(&llvm_cbe_i);
#line 2114 "UecUeContextData.hpp"
  llvm_cbe_tmp88 = *(&llvm_cbe_p_handoverRestrictionList_2e_addr);
#line 2114 "UecUeContextData.hpp"
  llvm_cbe_tmp96 = *((&((&((&((&(*((&((&llvm_cbe_tmp88->field4))->field4))).array[((signed int )0u)]))[((signed long long )(((unsigned long long )(unsigned int )llvm_cbe_tmp87)))]))->field1))->field0));
#line 2114 "UecUeContextData.hpp"
  llvm_cbe_call = _Znam((((unsigned long long )(((unsigned long long )(((unsigned long long )(unsigned short )llvm_cbe_tmp96))) * ((unsigned long long )2ull)))));
#line 2114 "UecUeContextData.hpp"
  llvm_cbe_tmp98 = *(&llvm_cbe_i);
#line 2114 "UecUeContextData.hpp"
  *((&((&((&((&(*((&((&((&((&((&llvm_cbe_this1->field1))->field25))->field13))->field4))->field1))).array[((signed int )0u)]))[((signed long long )(((unsigned long long )(unsigned int )llvm_cbe_tmp98)))]))->field1))->field1)) = (((struct l_struct_OC_SErrcN4TxAntennaTm5 *)llvm_cbe_call));
#line 2116 "UecUeContextData.hpp"
  llvm_cbe_tmp109 = *(&llvm_cbe_i);
#line 2116 "UecUeContextData.hpp"
  llvm_cbe_tmp120 = *((&((&((&((&(*((&((&((&((&((&llvm_cbe_this1->field1))->field25))->field13))->field4))->field1))).array[((signed int )0u)]))[((signed long long )(((unsigned long long )(unsigned int )llvm_cbe_tmp109)))]))->field1))->field1));
#line 2116 "UecUeContextData.hpp"
  llvm_cbe_tmp122 = *(&llvm_cbe_i);
#line 2116 "UecUeContextData.hpp"
  llvm_cbe_tmp123 = *(&llvm_cbe_p_handoverRestrictionList_2e_addr);
#line 2116 "UecUeContextData.hpp"
  llvm_cbe_tmp133 = *(&llvm_cbe_i);
#line 2116 "UecUeContextData.hpp"
  llvm_cbe_tmp134 = *(&llvm_cbe_p_handoverRestrictionList_2e_addr);
#line 2116 "UecUeContextData.hpp"
  llvm_cbe_tmp142 = *((&((&((&((&(*((&((&llvm_cbe_tmp134->field4))->field4))).array[((signed int )0u)]))[((signed long long )(((unsigned long long )(unsigned int )llvm_cbe_tmp133)))]))->field1))->field0));
#line 0 "LLVM INTERNAL"
  llvm_cbe_tmp__164 = memcpy((((unsigned char *)llvm_cbe_tmp120)), (((unsigned char *)((&(*((&((&((&((&(*((&((&llvm_cbe_tmp123->field4))->field4))).array[((signed int )0u)]))[((signed long long )(((unsigned long long )(unsigned int )llvm_cbe_tmp122)))]))->field1))->field1))).array[((signed int )0u)])))), (((unsigned long long )(((unsigned long long )(((signed long long )(signed int )(((unsigned int )(unsigned short )llvm_cbe_tmp142))))) * ((unsigned long long )2ull)))));
#line 2110 "UecUeContextData.hpp"
  llvm_cbe_tmp145 = *(&llvm_cbe_i);
#line 2110 "UecUeContextData.hpp"
  *(&llvm_cbe_i) = (((unsigned int )(((unsigned int )llvm_cbe_tmp145) + ((unsigned int )1u))));
#line 2110 "UecUeContextData.hpp"
  goto llvm_cbe_for_2e_cond;
  } while (1); /* end of syntactic loop 'for.cond' */
llvm_cbe_if_2e_end:
#line 2121 "UecUeContextData.hpp"
  llvm_cbe_tmp146 = *(&llvm_cbe_p_handoverRestrictionList_2e_addr);
#line 2121 "UecUeContextData.hpp"
  llvm_cbe_tmp148 = *((&llvm_cbe_tmp146->field5));
#line 2121 "UecUeContextData.hpp"
  *((&((&((&((&llvm_cbe_this1->field1))->field25))->field13))->field5)) = llvm_cbe_tmp148;
#line 2122 "UecUeContextData.hpp"
  llvm_cbe_tmp153 = *(&llvm_cbe_p_handoverRestrictionList_2e_addr);
#line 2122 "UecUeContextData.hpp"
  llvm_cbe_tmp155 = *((&llvm_cbe_tmp153->field5));
#line 2122 "UecUeContextData.hpp"
  if ((llvm_cbe_tmp155 != 0u)) {    goto llvm_cbe_if_2e_then157;  } else {    goto llvm_cbe_if_2e_end283;  }


llvm_cbe_if_2e_then157:
#line 2124 "UecUeContextData.hpp"
  llvm_cbe_tmp158 = *(&llvm_cbe_p_handoverRestrictionList_2e_addr);
#line 2124 "UecUeContextData.hpp"
  llvm_cbe_tmp161 = *((&((&llvm_cbe_tmp158->field6))->field0));
#line 2124 "UecUeContextData.hpp"
  *((&((&((&((&((&llvm_cbe_this1->field1))->field25))->field13))->field6))->field0)) = llvm_cbe_tmp161;
#line 2125 "UecUeContextData.hpp"
  *(&llvm_cbe_i168) = 0u;
#line 2125 "UecUeContextData.hpp"
  goto llvm_cbe_for_2e_cond169;
#line 2125 "UecUeContextData.hpp"
  do {     /* Syntactic loop 'for.cond169' to make GCC happy */
llvm_cbe_for_2e_cond169:
#line 2125 "UecUeContextData.hpp"
  llvm_cbe_tmp170 = *(&llvm_cbe_i168);
#line 2125 "UecUeContextData.hpp"
  llvm_cbe_tmp171 = *(&llvm_cbe_p_handoverRestrictionList_2e_addr);
#line 2125 "UecUeContextData.hpp"
  llvm_cbe_tmp174 = *((&((&llvm_cbe_tmp171->field6))->field0));
#line 2125 "UecUeContextData.hpp"
  if ((((unsigned int )llvm_cbe_tmp170) < ((unsigned int )(((unsigned int )(unsigned char )llvm_cbe_tmp174))))) {    goto llvm_cbe_for_2e_body177;  } else {    goto llvm_cbe_if_2e_end283;  }


llvm_cbe_for_2e_body177:
#line 2127 "UecUeContextData.hpp"
  llvm_cbe_tmp178 = *(&llvm_cbe_i168);
#line 2127 "UecUeContextData.hpp"
  llvm_cbe_tmp188 = *(&llvm_cbe_i168);
#line 2127 "UecUeContextData.hpp"
  llvm_cbe_tmp189 = *(&llvm_cbe_p_handoverRestrictionList_2e_addr);
#line 0 "LLVM INTERNAL"
  llvm_cbe_tmp__165 = memcpy((((unsigned char *)((&((&((&(*((&((&((&((&((&llvm_cbe_this1->field1))->field25))->field13))->field6))->field1))).array[((signed int )0u)]))[((signed long long )(((unsigned long long )(unsigned int )llvm_cbe_tmp178)))]))->field0)))), (((unsigned char *)((&((&((&(*((&((&llvm_cbe_tmp189->field6))->field4))).array[((signed int )0u)]))[((signed long long )(((unsigned long long )(unsigned int )llvm_cbe_tmp188)))]))->field0)))), 12ull);
#line 2128 "UecUeContextData.hpp"
  llvm_cbe_tmp198 = *(&llvm_cbe_i168);
#line 2128 "UecUeContextData.hpp"
  llvm_cbe_tmp199 = *(&llvm_cbe_p_handoverRestrictionList_2e_addr);
#line 2128 "UecUeContextData.hpp"
  llvm_cbe_tmp207 = *((&((&((&((&(*((&((&llvm_cbe_tmp199->field6))->field4))).array[((signed int )0u)]))[((signed long long )(((unsigned long long )(unsigned int )llvm_cbe_tmp198)))]))->field1))->field0));
#line 2128 "UecUeContextData.hpp"
  llvm_cbe_tmp208 = *(&llvm_cbe_i168);
#line 2128 "UecUeContextData.hpp"
  *((&((&((&((&(*((&((&((&((&((&llvm_cbe_this1->field1))->field25))->field13))->field6))->field1))).array[((signed int )0u)]))[((signed long long )(((unsigned long long )(unsigned int )llvm_cbe_tmp208)))]))->field1))->field0)) = llvm_cbe_tmp207;
#line 2129 "UecUeContextData.hpp"
  llvm_cbe_tmp219 = *(&llvm_cbe_i168);
#line 2129 "UecUeContextData.hpp"
  llvm_cbe_tmp220 = *(&llvm_cbe_p_handoverRestrictionList_2e_addr);
#line 2129 "UecUeContextData.hpp"
  llvm_cbe_tmp228 = *((&((&((&((&(*((&((&llvm_cbe_tmp220->field6))->field4))).array[((signed int )0u)]))[((signed long long )(((unsigned long long )(unsigned int )llvm_cbe_tmp219)))]))->field1))->field0));
#line 2129 "UecUeContextData.hpp"
  llvm_cbe_call230 = _Znam((((unsigned long long )(((unsigned long long )(((unsigned long long )(unsigned short )llvm_cbe_tmp228))) * ((unsigned long long )2ull)))));
#line 2129 "UecUeContextData.hpp"
  llvm_cbe_tmp231 = *(&llvm_cbe_i168);
#line 2129 "UecUeContextData.hpp"
  *((&((&((&((&(*((&((&((&((&((&llvm_cbe_this1->field1))->field25))->field13))->field6))->field1))).array[((signed int )0u)]))[((signed long long )(((unsigned long long )(unsigned int )llvm_cbe_tmp231)))]))->field1))->field1)) = (((struct l_struct_OC_SErrcN4TxAntennaTm5 *)llvm_cbe_call230));
#line 2131 "UecUeContextData.hpp"
  llvm_cbe_tmp242 = *(&llvm_cbe_i168);
#line 2131 "UecUeContextData.hpp"
  llvm_cbe_tmp253 = *((&((&((&((&(*((&((&((&((&((&llvm_cbe_this1->field1))->field25))->field13))->field6))->field1))).array[((signed int )0u)]))[((signed long long )(((unsigned long long )(unsigned int )llvm_cbe_tmp242)))]))->field1))->field1));
#line 2131 "UecUeContextData.hpp"
  llvm_cbe_tmp255 = *(&llvm_cbe_i168);
#line 2131 "UecUeContextData.hpp"
  llvm_cbe_tmp256 = *(&llvm_cbe_p_handoverRestrictionList_2e_addr);
#line 2131 "UecUeContextData.hpp"
  llvm_cbe_tmp266 = *(&llvm_cbe_i168);
#line 2131 "UecUeContextData.hpp"
  llvm_cbe_tmp267 = *(&llvm_cbe_p_handoverRestrictionList_2e_addr);
#line 2131 "UecUeContextData.hpp"
  llvm_cbe_tmp275 = *((&((&((&((&(*((&((&llvm_cbe_tmp267->field6))->field4))).array[((signed int )0u)]))[((signed long long )(((unsigned long long )(unsigned int )llvm_cbe_tmp266)))]))->field1))->field0));
#line 0 "LLVM INTERNAL"
  llvm_cbe_tmp__166 = memcpy((((unsigned char *)llvm_cbe_tmp253)), (((unsigned char *)((&(*((&((&((&((&(*((&((&llvm_cbe_tmp256->field6))->field4))).array[((signed int )0u)]))[((signed long long )(((unsigned long long )(unsigned int )llvm_cbe_tmp255)))]))->field1))->field1))).array[((signed int )0u)])))), (((unsigned long long )(((unsigned long long )(((signed long long )(signed int )(((unsigned int )(unsigned short )llvm_cbe_tmp275))))) * ((unsigned long long )2ull)))));
#line 2125 "UecUeContextData.hpp"
  llvm_cbe_tmp280 = *(&llvm_cbe_i168);
#line 2125 "UecUeContextData.hpp"
  *(&llvm_cbe_i168) = (((unsigned int )(((unsigned int )llvm_cbe_tmp280) + ((unsigned int )1u))));
#line 2125 "UecUeContextData.hpp"
  goto llvm_cbe_for_2e_cond169;
  } while (1); /* end of syntactic loop 'for.cond169' */
llvm_cbe_if_2e_end283:
#line 2136 "UecUeContextData.hpp"
  llvm_cbe_tmp284 = *(&llvm_cbe_p_handoverRestrictionList_2e_addr);
#line 2136 "UecUeContextData.hpp"
  llvm_cbe_tmp286 = *((&llvm_cbe_tmp284->field7));
#line 2136 "UecUeContextData.hpp"
  *((&((&((&((&llvm_cbe_this1->field1))->field25))->field13))->field7)) = llvm_cbe_tmp286;
#line 2137 "UecUeContextData.hpp"
  llvm_cbe_tmp291 = *(&llvm_cbe_p_handoverRestrictionList_2e_addr);
#line 2137 "UecUeContextData.hpp"
  llvm_cbe_tmp293 = *((&llvm_cbe_tmp291->field8));
#line 2137 "UecUeContextData.hpp"
  *((&((&((&((&llvm_cbe_this1->field1))->field25))->field13))->field8)) = llvm_cbe_tmp293;
#line 2139 "UecUeContextData.hpp"
  *((&((&((&llvm_cbe_this1->field1))->field25))->field12)) = 1u;
#line 2140 "UecUeContextData.hpp"
  return;
}


#line 0 "LLVM INTERNAL"
struct l_class_OC_BaseEvent *_ZN11DummyStreamlsIPvEERS_T_(struct l_class_OC_BaseEvent *llvm_cbe_this, unsigned char *llvm_cbe_arg) {
  struct l_class_OC_BaseEvent *llvm_cbe_retval;    /* Address-exposed local */
  struct l_class_OC_BaseEvent *llvm_cbe_this_2e_addr;    /* Address-exposed local */
  unsigned char *llvm_cbe_arg_2e_addr;    /* Address-exposed local */
  struct l_class_OC_BaseEvent *llvm_cbe_this1;
  struct l_class_OC_BaseEvent *llvm_cbe_tmp__167;

#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_this_2e_addr) = llvm_cbe_this;
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_arg_2e_addr) = llvm_cbe_arg;
#line 0 "LLVM INTERNAL"
  llvm_cbe_this1 = *(&llvm_cbe_this_2e_addr);
#line 30 "BaseTypes.hpp"
  *(&llvm_cbe_retval) = llvm_cbe_this1;
#line 31 "BaseTypes.hpp"
  llvm_cbe_tmp__167 = *(&llvm_cbe_retval);
#line 31 "BaseTypes.hpp"
  return llvm_cbe_tmp__167;
}


#line 0 "LLVM INTERNAL"
unsigned int _ZNK3Uec16UecUeContextData11getLockStepEv(struct l_class_OC_Uec_KD__KD_UecUeContextData *llvm_cbe_this) {
  unsigned int llvm_cbe_retval;    /* Address-exposed local */
  struct l_class_OC_Uec_KD__KD_UecUeContextData *llvm_cbe_this_2e_addr;    /* Address-exposed local */
  struct l_class_OC_Uec_KD__KD_UecUeContextData *llvm_cbe_this1;
  unsigned int llvm_cbe_tmp4;
  unsigned int llvm_cbe_tmp__168;

#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_this_2e_addr) = llvm_cbe_this;
#line 0 "LLVM INTERNAL"
  llvm_cbe_this1 = *(&llvm_cbe_this_2e_addr);
#line 1992 "UecUeContextData.hpp"
  llvm_cbe_tmp4 = *((&((&((&llvm_cbe_this1->field1))->field25))->field9));
#line 1992 "UecUeContextData.hpp"
  *(&llvm_cbe_retval) = llvm_cbe_tmp4;
#line 1993 "UecUeContextData.hpp"
  llvm_cbe_tmp__168 = *(&llvm_cbe_retval);
#line 1993 "UecUeContextData.hpp"
  return llvm_cbe_tmp__168;
}


#line 0 "LLVM INTERNAL"
void _ZN3Uec27UEC_CTY_TupuProceedLockStepC1Ev(struct l_struct_OC_SAntennaInfo *llvm_cbe_this) {
  struct l_struct_OC_SAntennaInfo *llvm_cbe_this_2e_addr;    /* Address-exposed local */
  struct l_struct_OC_SAntennaInfo *llvm_cbe_this1;

#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_this_2e_addr) = llvm_cbe_this;
#line 0 "LLVM INTERNAL"
  llvm_cbe_this1 = *(&llvm_cbe_this_2e_addr);
#line 0 "LLVM INTERNAL"
  _ZN3Uec27UEC_CTY_TupuProceedLockStepC2Ev(llvm_cbe_this1);
#line 628 "UecMessages.hpp"
  return;
}


#line 0 "LLVM INTERNAL"
struct l_class_OC_BaseEvent *_ZN11DummyStreamlsIN3Uec15EUecNasDeliveryEEERS_T_(struct l_class_OC_BaseEvent *llvm_cbe_this, unsigned int llvm_cbe_arg) {
  struct l_class_OC_BaseEvent *llvm_cbe_retval;    /* Address-exposed local */
  struct l_class_OC_BaseEvent *llvm_cbe_this_2e_addr;    /* Address-exposed local */
  unsigned int llvm_cbe_arg_2e_addr;    /* Address-exposed local */
  struct l_class_OC_BaseEvent *llvm_cbe_this1;
  struct l_class_OC_BaseEvent *llvm_cbe_tmp__169;

#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_this_2e_addr) = llvm_cbe_this;
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_arg_2e_addr) = llvm_cbe_arg;
#line 0 "LLVM INTERNAL"
  llvm_cbe_this1 = *(&llvm_cbe_this_2e_addr);
#line 30 "BaseTypes.hpp"
  *(&llvm_cbe_retval) = llvm_cbe_this1;
#line 31 "BaseTypes.hpp"
  llvm_cbe_tmp__169 = *(&llvm_cbe_retval);
#line 31 "BaseTypes.hpp"
  return llvm_cbe_tmp__169;
}


#line 0 "LLVM INTERNAL"
struct l_class_OC_BaseEvent *_ZN11DummyStreamlsIN3Uec13EUecDirectionEEERS_T_(struct l_class_OC_BaseEvent *llvm_cbe_this, unsigned int llvm_cbe_arg) {
  struct l_class_OC_BaseEvent *llvm_cbe_retval;    /* Address-exposed local */
  struct l_class_OC_BaseEvent *llvm_cbe_this_2e_addr;    /* Address-exposed local */
  unsigned int llvm_cbe_arg_2e_addr;    /* Address-exposed local */
  struct l_class_OC_BaseEvent *llvm_cbe_this1;
  struct l_class_OC_BaseEvent *llvm_cbe_tmp__170;

#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_this_2e_addr) = llvm_cbe_this;
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_arg_2e_addr) = llvm_cbe_arg;
#line 0 "LLVM INTERNAL"
  llvm_cbe_this1 = *(&llvm_cbe_this_2e_addr);
#line 30 "BaseTypes.hpp"
  *(&llvm_cbe_retval) = llvm_cbe_this1;
#line 31 "BaseTypes.hpp"
  llvm_cbe_tmp__170 = *(&llvm_cbe_retval);
#line 31 "BaseTypes.hpp"
  return llvm_cbe_tmp__170;
}


#line 0 "LLVM INTERNAL"
unsigned int _ZN3Uec14UecServiceBase12getServiceIdEv(struct l_class_OC_Uec_KD__KD_UecServiceBase *llvm_cbe_this) {
  unsigned int llvm_cbe_retval;    /* Address-exposed local */
  struct l_class_OC_Uec_KD__KD_UecServiceBase *llvm_cbe_this_2e_addr;    /* Address-exposed local */
  struct l_class_OC_Uec_KD__KD_UecServiceBase *llvm_cbe_this1;
  unsigned int llvm_cbe_tmp2;
  unsigned int llvm_cbe_tmp__171;

#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_this_2e_addr) = llvm_cbe_this;
#line 0 "LLVM INTERNAL"
  llvm_cbe_this1 = *(&llvm_cbe_this_2e_addr);
#line 84 "UecServiceBase.hpp"
  llvm_cbe_tmp2 = *((&llvm_cbe_this1->field5));
#line 84 "UecServiceBase.hpp"
  *(&llvm_cbe_retval) = llvm_cbe_tmp2;
#line 84 "UecServiceBase.hpp"
  llvm_cbe_tmp__171 = *(&llvm_cbe_retval);
#line 84 "UecServiceBase.hpp"
  return llvm_cbe_tmp__171;
}


#line 0 "LLVM INTERNAL"
void _ZN3Uec10UecFsmBase7InitFSMEv(struct l_class_OC_Uec_KD__KD_UecFsmBase *llvm_cbe_this) {
  struct l_class_OC_Uec_KD__KD_UecFsmBase *llvm_cbe_this_2e_addr;    /* Address-exposed local */
  struct l_class_OC_Uec_KD__KD_UecFsmBase *llvm_cbe_this1;

#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_this_2e_addr) = llvm_cbe_this;
#line 0 "LLVM INTERNAL"
  llvm_cbe_this1 = *(&llvm_cbe_this_2e_addr);
#line 73 "UecFsmBase.hpp"
  return;
}


#line 0 "LLVM INTERNAL"
void _ZN3Uec10UecFsmBase8StartFSMEv(struct l_class_OC_Uec_KD__KD_UecFsmBase *llvm_cbe_this) {
  struct l_class_OC_Uec_KD__KD_UecFsmBase *llvm_cbe_this_2e_addr;    /* Address-exposed local */
  struct l_class_OC_Uec_KD__KD_UecFsmBase *llvm_cbe_this1;

#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_this_2e_addr) = llvm_cbe_this;
#line 0 "LLVM INTERNAL"
  llvm_cbe_this1 = *(&llvm_cbe_this_2e_addr);
#line 82 "UecFsmBase.hpp"
  return;
}


#line 0 "LLVM INTERNAL"
void _ZN7CStateTIN3Uec17UecDirectTransferEEC2EPS1_MS1_FjP6CEventESs(struct l_class_OC_CStateT *llvm_cbe_this, struct l_class_OC_Uec_KD__KD_UecDirectTransfer *llvm_cbe_itsFsmPtr, struct l_struct_OC_SS1apUEAggregateMaximumBitrate llvm_cbe_tmp__172, struct l_class_OC_std_KD__KD_basic_string *llvm_cbe_name) {
  struct l_class_OC_CStateT *llvm_cbe_this_2e_addr;    /* Address-exposed local */
  struct l_class_OC_Uec_KD__KD_UecDirectTransfer *llvm_cbe_itsFsmPtr_2e_addr;    /* Address-exposed local */
  struct l_struct_OC_SS1apUEAggregateMaximumBitrate llvm_cbe_stateFuncPtr;    /* Address-exposed local */
  struct l_class_OC_std_KD__KD_basic_string llvm_cbe_agg_2e_tmp;    /* Address-exposed local */
  struct l_class_OC_CStateT *llvm_cbe_this1;
  struct l_class_OC_Uec_KD__KD_UecDirectTransfer *llvm_cbe_tmp;
  unsigned char *llvm_cbe_tmp__173;
  struct l_struct_OC_SS1apUEAggregateMaximumBitrate *llvm_cbe_tmp11;
  struct l_struct_OC_SS1apUEAggregateMaximumBitrate *llvm_cbe_tmp12;

#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_this_2e_addr) = llvm_cbe_this;
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_itsFsmPtr_2e_addr) = llvm_cbe_itsFsmPtr;
#line 0 "LLVM INTERNAL"
  ((struct __attribute__ ((packed, aligned(1))) {struct l_struct_OC_SS1apUEAggregateMaximumBitrate data; } *)(&llvm_cbe_stateFuncPtr))->data = llvm_cbe_tmp__172;
#line 0 "LLVM INTERNAL"
  llvm_cbe_this1 = *(&llvm_cbe_this_2e_addr);
#line 0 "LLVM INTERNAL"
  _ZNSsC1ERKSs((&llvm_cbe_agg_2e_tmp), llvm_cbe_name);
#line 0 "LLVM INTERNAL"
  _ZN10CStateBaseC2ESs((((struct l_class_OC_CStateBase *)llvm_cbe_this1)), (&llvm_cbe_agg_2e_tmp));
#line 0 "LLVM INTERNAL"
  _ZNSsD1Ev((&llvm_cbe_agg_2e_tmp));
#line 0 "LLVM INTERNAL"
  *(((unsigned char ***)llvm_cbe_this1)) = ((&_ZTV7CStateTIN3Uec17UecDirectTransferEE.array[((signed long long )2ull)]));
#line 68 "CStateT.hpp"
  llvm_cbe_tmp = *(&llvm_cbe_itsFsmPtr_2e_addr);
#line 68 "CStateT.hpp"
  *((&llvm_cbe_this1->field1)) = llvm_cbe_tmp;
#line 0 "LLVM INTERNAL"
  llvm_cbe_tmp__173 = memcpy((((unsigned char *)((&llvm_cbe_this1->field2)))), (((unsigned char *)(&llvm_cbe_stateFuncPtr))), 16ull);
#line 70 "CStateT.hpp"
  llvm_cbe_tmp11 = (&llvm_cbe_this1->field4);
#line 70 "CStateT.hpp"
  *((&llvm_cbe_tmp11->field0)) = 0ull;
#line 70 "CStateT.hpp"
  *((&llvm_cbe_tmp11->field1)) = 0ull;
#line 71 "CStateT.hpp"
  llvm_cbe_tmp12 = (&llvm_cbe_this1->field5);
#line 71 "CStateT.hpp"
  *((&llvm_cbe_tmp12->field0)) = 0ull;
#line 71 "CStateT.hpp"
  *((&llvm_cbe_tmp12->field1)) = 0ull;
#line 72 "CStateT.hpp"
  *((&llvm_cbe_this1->field3)) = ((struct l_class_OC_CStateBase *)/*NULL*/0);
#line 73 "CStateT.hpp"
  return;
}


#line 0 "LLVM INTERNAL"
void _ZN10CStateBaseC2ESs(struct l_class_OC_CStateBase *llvm_cbe_this, struct l_class_OC_std_KD__KD_basic_string *llvm_cbe_name) {
  struct l_class_OC_CStateBase *llvm_cbe_this_2e_addr;    /* Address-exposed local */
  struct l_class_OC_CStateBase *llvm_cbe_this1;
  struct l_class_OC_std_KD__KD_basic_string *llvm_cbe_call;

#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_this_2e_addr) = llvm_cbe_this;
#line 0 "LLVM INTERNAL"
  llvm_cbe_this1 = *(&llvm_cbe_this_2e_addr);
#line 0 "LLVM INTERNAL"
  *(((unsigned char ***)llvm_cbe_this1)) = ((&_ZTV10CStateBase.array[((signed long long )2ull)]));
#line 0 "LLVM INTERNAL"
  _ZNSsC1Ev(((&llvm_cbe_this1->field1)));
#line 63 "CStateBase.hpp"
  llvm_cbe_call = _ZNSsaSERKSs(((&llvm_cbe_this1->field1)), llvm_cbe_name);
#line 64 "CStateBase.hpp"
  *((&llvm_cbe_this1->field2)) = ((struct l_class_OC_CStateBase *)/*NULL*/0);
#line 65 "CStateBase.hpp"
  *((&llvm_cbe_this1->field3)) = ((struct l_class_OC_CStateBase *)/*NULL*/0);
#line 66 "CStateBase.hpp"
  *((&llvm_cbe_this1->field4)) = ((struct l_class_OC_CStateBase *)/*NULL*/0);
#line 67 "CStateBase.hpp"
  return;
}


#line 0 "LLVM INTERNAL"
void _ZN10CStateBaseD2Ev(struct l_class_OC_CStateBase *llvm_cbe_this) {
  struct l_class_OC_CStateBase *llvm_cbe_this_2e_addr;    /* Address-exposed local */
  struct l_class_OC_CStateBase *llvm_cbe_this1;

#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_this_2e_addr) = llvm_cbe_this;
#line 0 "LLVM INTERNAL"
  llvm_cbe_this1 = *(&llvm_cbe_this_2e_addr);
#line 0 "LLVM INTERNAL"
  *(((unsigned char ***)llvm_cbe_this1)) = ((&_ZTV10CStateBase.array[((signed long long )2ull)]));
#line 74 "CStateBase.hpp"
  _ZNSsD1Ev(((&llvm_cbe_this1->field1)));
#line 74 "CStateBase.hpp"
  return;
}


#line 0 "LLVM INTERNAL"
void _ZN7CStateTIN3Uec17UecDirectTransferEED1Ev(struct l_class_OC_CStateT *llvm_cbe_this) {
  struct l_class_OC_CStateT *llvm_cbe_this_2e_addr;    /* Address-exposed local */
  struct l_class_OC_CStateT *llvm_cbe_this1;

#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_this_2e_addr) = llvm_cbe_this;
#line 0 "LLVM INTERNAL"
  llvm_cbe_this1 = *(&llvm_cbe_this_2e_addr);
#line 0 "LLVM INTERNAL"
  _ZN7CStateTIN3Uec17UecDirectTransferEED2Ev(llvm_cbe_this1);
#line 80 "CStateT.hpp"
  return;
}


#line 0 "LLVM INTERNAL"
void _ZN7CStateTIN3Uec17UecDirectTransferEED0Ev(struct l_class_OC_CStateT *llvm_cbe_this) {
  struct l_class_OC_CStateT *llvm_cbe_this_2e_addr;    /* Address-exposed local */
  struct l_class_OC_CStateT *llvm_cbe_this1;

#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_this_2e_addr) = llvm_cbe_this;
#line 0 "LLVM INTERNAL"
  llvm_cbe_this1 = *(&llvm_cbe_this_2e_addr);
#line 0 "LLVM INTERNAL"
  _ZN7CStateTIN3Uec17UecDirectTransferEED1Ev(llvm_cbe_this1);
#line 0 "LLVM INTERNAL"
  _ZdlPv((((unsigned char *)llvm_cbe_this1)));
#line 80 "CStateT.hpp"
  return;
}


#line 0 "LLVM INTERNAL"
unsigned int _ZN7CStateTIN3Uec17UecDirectTransferEE11HandleEventEP6CEvent(struct l_class_OC_CStateT *llvm_cbe_this, struct l_class_OC_CEvent *llvm_cbe_pEvent) {
  unsigned int llvm_cbe_retval;    /* Address-exposed local */
  struct l_class_OC_CStateT *llvm_cbe_this_2e_addr;    /* Address-exposed local */
  struct l_class_OC_CEvent *llvm_cbe_pEvent_2e_addr;    /* Address-exposed local */
  struct l_struct_OC_SS1apUEAggregateMaximumBitrate llvm_cbe_mem_2e_fn;    /* Address-exposed local */
  struct l_class_OC_CStateT *llvm_cbe_this1;
  unsigned char *llvm_cbe_tmp__174;
  struct l_class_OC_Uec_KD__KD_UecDirectTransfer *llvm_cbe_tmp5;
  unsigned long long llvm_cbe_mem_2e_fn_2e_adj;
  struct l_class_OC_Uec_KD__KD_UecDirectTransfer *llvm_cbe_this6;
  unsigned long long *llvm_cbe_mem_2e_fn_2e_ptr;
  unsigned long long llvm_cbe_fn;
  unsigned int  (**llvm_cbe_tmp__175) (struct l_class_OC_Uec_KD__KD_UecDirectTransfer *, struct l_class_OC_CEvent *);
  unsigned int  (*llvm_cbe_virtualfn) (struct l_class_OC_Uec_KD__KD_UecDirectTransfer *, struct l_class_OC_CEvent *);
  unsigned long long llvm_cbe_fn8;
  unsigned int  (*llvm_cbe_tmp__176) (struct l_class_OC_Uec_KD__KD_UecDirectTransfer *, struct l_class_OC_CEvent *);
  unsigned int  (*llvm_cbe_tmp__177) (struct l_class_OC_Uec_KD__KD_UecDirectTransfer *, struct l_class_OC_CEvent *);
  unsigned int  (*llvm_cbe_tmp__177__PHI_TEMPORARY) (struct l_class_OC_Uec_KD__KD_UecDirectTransfer *, struct l_class_OC_CEvent *);
  struct l_class_OC_CEvent *llvm_cbe_tmp9;
  unsigned int llvm_cbe_call;
  unsigned int llvm_cbe_tmp__178;

#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_this_2e_addr) = llvm_cbe_this;
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_pEvent_2e_addr) = llvm_cbe_pEvent;
#line 0 "LLVM INTERNAL"
  llvm_cbe_this1 = *(&llvm_cbe_this_2e_addr);
#line 0 "LLVM INTERNAL"
  llvm_cbe_tmp__174 = memcpy((((unsigned char *)(&llvm_cbe_mem_2e_fn))), (((unsigned char *)((&llvm_cbe_this1->field2)))), 16ull);
#line 96 "CStateT.hpp"
  llvm_cbe_tmp5 = *((&llvm_cbe_this1->field1));
#line 96 "CStateT.hpp"
  llvm_cbe_mem_2e_fn_2e_adj = *((&llvm_cbe_mem_2e_fn.field1));
#line 96 "CStateT.hpp"
  llvm_cbe_this6 = ((struct l_class_OC_Uec_KD__KD_UecDirectTransfer *)((&(((unsigned char *)llvm_cbe_tmp5))[((signed long long )llvm_cbe_mem_2e_fn_2e_adj)])));
#line 96 "CStateT.hpp"
  llvm_cbe_mem_2e_fn_2e_ptr = (&llvm_cbe_mem_2e_fn.field0);
#line 96 "CStateT.hpp"
  llvm_cbe_fn = *llvm_cbe_mem_2e_fn_2e_ptr;
#line 96 "CStateT.hpp"
  if ((((((bool )(llvm_cbe_fn & 1ull)&1u))&1))) {    goto llvm_cbe_fn_2e_virtual;  } else {    goto llvm_cbe_fn_2e_nonvirtual;  }


llvm_cbe_fn_2e_virtual:
#line 96 "CStateT.hpp"
  llvm_cbe_tmp__175 = *(((unsigned int  (***) (struct l_class_OC_Uec_KD__KD_UecDirectTransfer *, struct l_class_OC_CEvent *))llvm_cbe_this6));
#line 96 "CStateT.hpp"
  llvm_cbe_virtualfn = *(((unsigned int  (**) (struct l_class_OC_Uec_KD__KD_UecDirectTransfer *, struct l_class_OC_CEvent *))((&(((unsigned char *)llvm_cbe_tmp__175))[((signed long long )(((unsigned long long )(((unsigned long long )llvm_cbe_fn) - ((unsigned long long )1ull)))))]))));
#line 96 "CStateT.hpp"
  llvm_cbe_tmp__177__PHI_TEMPORARY = llvm_cbe_virtualfn;   /* for PHI node */
  goto llvm_cbe_fn_2e_end;

llvm_cbe_fn_2e_nonvirtual:
#line 96 "CStateT.hpp"
  llvm_cbe_fn8 = *llvm_cbe_mem_2e_fn_2e_ptr;
#line 96 "CStateT.hpp"
  llvm_cbe_tmp__176 = ((unsigned int  (*) (struct l_class_OC_Uec_KD__KD_UecDirectTransfer *, struct l_class_OC_CEvent *))(unsigned long)llvm_cbe_fn8);
#line 96 "CStateT.hpp"
  llvm_cbe_tmp__177__PHI_TEMPORARY = llvm_cbe_tmp__176;   /* for PHI node */
  goto llvm_cbe_fn_2e_end;

llvm_cbe_fn_2e_end:
#line 96 "CStateT.hpp"
  llvm_cbe_tmp__177 = llvm_cbe_tmp__177__PHI_TEMPORARY;
#line 96 "CStateT.hpp"
  llvm_cbe_tmp9 = *(&llvm_cbe_pEvent_2e_addr);
#line 96 "CStateT.hpp"
  llvm_cbe_call = llvm_cbe_tmp__177(llvm_cbe_this6, llvm_cbe_tmp9);
#line 96 "CStateT.hpp"
  *(&llvm_cbe_retval) = llvm_cbe_call;
#line 97 "CStateT.hpp"
  llvm_cbe_tmp__178 = *(&llvm_cbe_retval);
#line 97 "CStateT.hpp"
  return llvm_cbe_tmp__178;
}


#line 0 "LLVM INTERNAL"
void _ZN7CStateTIN3Uec17UecDirectTransferEE14InitTransitionEv(struct l_class_OC_CStateT *llvm_cbe_this) {
  struct l_class_OC_CStateT *llvm_cbe_this_2e_addr;    /* Address-exposed local */
  struct l_class_OC_CStateT *llvm_cbe_this1;
  struct l_class_OC_CStateBase *llvm_cbe_tmp2;
  struct l_class_OC_Uec_KD__KD_UecDirectTransfer *llvm_cbe_tmp4;
  struct l_class_OC_CStateBase *llvm_cbe_tmp6;

#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_this_2e_addr) = llvm_cbe_this;
#line 0 "LLVM INTERNAL"
  llvm_cbe_this1 = *(&llvm_cbe_this_2e_addr);
#line 168 "CStateT.hpp"
  llvm_cbe_tmp2 = *((&llvm_cbe_this1->field3));
#line 168 "CStateT.hpp"
  if ((llvm_cbe_tmp2 != ((struct l_class_OC_CStateBase *)/*NULL*/0))) {    goto llvm_cbe_if_2e_then;  } else {    goto llvm_cbe_if_2e_end;  }


llvm_cbe_if_2e_then:
#line 170 "CStateT.hpp"
  llvm_cbe_tmp4 = *((&llvm_cbe_this1->field1));
#line 170 "CStateT.hpp"
  llvm_cbe_tmp6 = *((&llvm_cbe_this1->field3));
#line 170 "CStateT.hpp"
  _ZN6Common8CFsmBase15StateTransitionEP10CStateBase((((struct l_class_OC_Common_KD__KD_CFsmBase *)((&(((unsigned char *)llvm_cbe_tmp4))[((signed long long )8ull)])))), llvm_cbe_tmp6);
#line 172 "CStateT.hpp"
  return;

llvm_cbe_if_2e_end:
#line 172 "CStateT.hpp"
  return;
}


#line 0 "LLVM INTERNAL"
void _ZN7CStateTIN3Uec17UecDirectTransferEE11EntryActionEv(struct l_class_OC_CStateT *llvm_cbe_this) {
  struct l_class_OC_CStateT *llvm_cbe_this_2e_addr;    /* Address-exposed local */
  struct l_struct_OC_SS1apUEAggregateMaximumBitrate llvm_cbe_tmp;    /* Address-exposed local */
  struct l_struct_OC_SS1apUEAggregateMaximumBitrate llvm_cbe_mem_2e_fn;    /* Address-exposed local */
  struct l_class_OC_CStateT *llvm_cbe_this1;
  unsigned char *llvm_cbe_tmp__179;
  unsigned long long llvm_cbe_tmp__180;
  unsigned char *llvm_cbe_tmp__181;
  struct l_class_OC_Uec_KD__KD_UecDirectTransfer *llvm_cbe_tmp9;
  unsigned long long llvm_cbe_mem_2e_fn_2e_adj;
  struct l_class_OC_Uec_KD__KD_UecDirectTransfer *llvm_cbe_this10;
  unsigned long long *llvm_cbe_mem_2e_fn_2e_ptr;
  unsigned long long llvm_cbe_fn;
  void  (**llvm_cbe_tmp__182) (struct l_class_OC_Uec_KD__KD_UecDirectTransfer *);
  void  (*llvm_cbe_virtualfn) (struct l_class_OC_Uec_KD__KD_UecDirectTransfer *);
  unsigned long long llvm_cbe_fn12;
  void  (*llvm_cbe_tmp__183) (struct l_class_OC_Uec_KD__KD_UecDirectTransfer *);
  void  (*llvm_cbe_tmp__184) (struct l_class_OC_Uec_KD__KD_UecDirectTransfer *);
  void  (*llvm_cbe_tmp__184__PHI_TEMPORARY) (struct l_class_OC_Uec_KD__KD_UecDirectTransfer *);

#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_this_2e_addr) = llvm_cbe_this;
#line 0 "LLVM INTERNAL"
  llvm_cbe_this1 = *(&llvm_cbe_this_2e_addr);
#line 0 "LLVM INTERNAL"
  llvm_cbe_tmp__179 = memcpy((((unsigned char *)(&llvm_cbe_tmp))), (((unsigned char *)((&llvm_cbe_this1->field4)))), 16ull);
#line 188 "CStateT.hpp"
  llvm_cbe_tmp__180 = *((&llvm_cbe_tmp.field0));
#line 188 "CStateT.hpp"
  if ((llvm_cbe_tmp__180 != 0ull)) {    goto llvm_cbe_if_2e_then;  } else {    goto llvm_cbe_if_2e_end;  }


llvm_cbe_if_2e_then:
#line 0 "LLVM INTERNAL"
  llvm_cbe_tmp__181 = memcpy((((unsigned char *)(&llvm_cbe_mem_2e_fn))), (((unsigned char *)((&llvm_cbe_this1->field4)))), 16ull);
#line 190 "CStateT.hpp"
  llvm_cbe_tmp9 = *((&llvm_cbe_this1->field1));
#line 190 "CStateT.hpp"
  llvm_cbe_mem_2e_fn_2e_adj = *((&llvm_cbe_mem_2e_fn.field1));
#line 190 "CStateT.hpp"
  llvm_cbe_this10 = ((struct l_class_OC_Uec_KD__KD_UecDirectTransfer *)((&(((unsigned char *)llvm_cbe_tmp9))[((signed long long )llvm_cbe_mem_2e_fn_2e_adj)])));
#line 190 "CStateT.hpp"
  llvm_cbe_mem_2e_fn_2e_ptr = (&llvm_cbe_mem_2e_fn.field0);
#line 190 "CStateT.hpp"
  llvm_cbe_fn = *llvm_cbe_mem_2e_fn_2e_ptr;
#line 190 "CStateT.hpp"
  if ((((((bool )(llvm_cbe_fn & 1ull)&1u))&1))) {    goto llvm_cbe_fn_2e_virtual;  } else {    goto llvm_cbe_fn_2e_nonvirtual;  }


llvm_cbe_fn_2e_virtual:
#line 190 "CStateT.hpp"
  llvm_cbe_tmp__182 = *(((void  (***) (struct l_class_OC_Uec_KD__KD_UecDirectTransfer *))llvm_cbe_this10));
#line 190 "CStateT.hpp"
  llvm_cbe_virtualfn = *(((void  (**) (struct l_class_OC_Uec_KD__KD_UecDirectTransfer *))((&(((unsigned char *)llvm_cbe_tmp__182))[((signed long long )(((unsigned long long )(((unsigned long long )llvm_cbe_fn) - ((unsigned long long )1ull)))))]))));
#line 190 "CStateT.hpp"
  llvm_cbe_tmp__184__PHI_TEMPORARY = llvm_cbe_virtualfn;   /* for PHI node */
  goto llvm_cbe_fn_2e_end;

llvm_cbe_fn_2e_nonvirtual:
#line 190 "CStateT.hpp"
  llvm_cbe_fn12 = *llvm_cbe_mem_2e_fn_2e_ptr;
#line 190 "CStateT.hpp"
  llvm_cbe_tmp__183 = ((void  (*) (struct l_class_OC_Uec_KD__KD_UecDirectTransfer *))(unsigned long)llvm_cbe_fn12);
#line 190 "CStateT.hpp"
  llvm_cbe_tmp__184__PHI_TEMPORARY = llvm_cbe_tmp__183;   /* for PHI node */
  goto llvm_cbe_fn_2e_end;

llvm_cbe_fn_2e_end:
#line 190 "CStateT.hpp"
  llvm_cbe_tmp__184 = llvm_cbe_tmp__184__PHI_TEMPORARY;
#line 190 "CStateT.hpp"
  llvm_cbe_tmp__184(llvm_cbe_this10);
#line 192 "CStateT.hpp"
  return;

llvm_cbe_if_2e_end:
#line 192 "CStateT.hpp"
  return;
}


#line 0 "LLVM INTERNAL"
void _ZN7CStateTIN3Uec17UecDirectTransferEE10ExitActionEv(struct l_class_OC_CStateT *llvm_cbe_this) {
  struct l_class_OC_CStateT *llvm_cbe_this_2e_addr;    /* Address-exposed local */
  struct l_struct_OC_SS1apUEAggregateMaximumBitrate llvm_cbe_tmp;    /* Address-exposed local */
  struct l_struct_OC_SS1apUEAggregateMaximumBitrate llvm_cbe_mem_2e_fn;    /* Address-exposed local */
  struct l_class_OC_CStateT *llvm_cbe_this1;
  unsigned char *llvm_cbe_tmp__185;
  unsigned long long llvm_cbe_tmp__186;
  unsigned char *llvm_cbe_tmp__187;
  struct l_class_OC_Uec_KD__KD_UecDirectTransfer *llvm_cbe_tmp9;
  unsigned long long llvm_cbe_mem_2e_fn_2e_adj;
  struct l_class_OC_Uec_KD__KD_UecDirectTransfer *llvm_cbe_this10;
  unsigned long long *llvm_cbe_mem_2e_fn_2e_ptr;
  unsigned long long llvm_cbe_fn;
  void  (**llvm_cbe_tmp__188) (struct l_class_OC_Uec_KD__KD_UecDirectTransfer *);
  void  (*llvm_cbe_virtualfn) (struct l_class_OC_Uec_KD__KD_UecDirectTransfer *);
  unsigned long long llvm_cbe_fn12;
  void  (*llvm_cbe_tmp__189) (struct l_class_OC_Uec_KD__KD_UecDirectTransfer *);
  void  (*llvm_cbe_tmp__190) (struct l_class_OC_Uec_KD__KD_UecDirectTransfer *);
  void  (*llvm_cbe_tmp__190__PHI_TEMPORARY) (struct l_class_OC_Uec_KD__KD_UecDirectTransfer *);

#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_this_2e_addr) = llvm_cbe_this;
#line 0 "LLVM INTERNAL"
  llvm_cbe_this1 = *(&llvm_cbe_this_2e_addr);
#line 0 "LLVM INTERNAL"
  llvm_cbe_tmp__185 = memcpy((((unsigned char *)(&llvm_cbe_tmp))), (((unsigned char *)((&llvm_cbe_this1->field5)))), 16ull);
#line 208 "CStateT.hpp"
  llvm_cbe_tmp__186 = *((&llvm_cbe_tmp.field0));
#line 208 "CStateT.hpp"
  if ((llvm_cbe_tmp__186 != 0ull)) {    goto llvm_cbe_if_2e_then;  } else {    goto llvm_cbe_if_2e_end;  }


llvm_cbe_if_2e_then:
#line 0 "LLVM INTERNAL"
  llvm_cbe_tmp__187 = memcpy((((unsigned char *)(&llvm_cbe_mem_2e_fn))), (((unsigned char *)((&llvm_cbe_this1->field5)))), 16ull);
#line 210 "CStateT.hpp"
  llvm_cbe_tmp9 = *((&llvm_cbe_this1->field1));
#line 210 "CStateT.hpp"
  llvm_cbe_mem_2e_fn_2e_adj = *((&llvm_cbe_mem_2e_fn.field1));
#line 210 "CStateT.hpp"
  llvm_cbe_this10 = ((struct l_class_OC_Uec_KD__KD_UecDirectTransfer *)((&(((unsigned char *)llvm_cbe_tmp9))[((signed long long )llvm_cbe_mem_2e_fn_2e_adj)])));
#line 210 "CStateT.hpp"
  llvm_cbe_mem_2e_fn_2e_ptr = (&llvm_cbe_mem_2e_fn.field0);
#line 210 "CStateT.hpp"
  llvm_cbe_fn = *llvm_cbe_mem_2e_fn_2e_ptr;
#line 210 "CStateT.hpp"
  if ((((((bool )(llvm_cbe_fn & 1ull)&1u))&1))) {    goto llvm_cbe_fn_2e_virtual;  } else {    goto llvm_cbe_fn_2e_nonvirtual;  }


llvm_cbe_fn_2e_virtual:
#line 210 "CStateT.hpp"
  llvm_cbe_tmp__188 = *(((void  (***) (struct l_class_OC_Uec_KD__KD_UecDirectTransfer *))llvm_cbe_this10));
#line 210 "CStateT.hpp"
  llvm_cbe_virtualfn = *(((void  (**) (struct l_class_OC_Uec_KD__KD_UecDirectTransfer *))((&(((unsigned char *)llvm_cbe_tmp__188))[((signed long long )(((unsigned long long )(((unsigned long long )llvm_cbe_fn) - ((unsigned long long )1ull)))))]))));
#line 210 "CStateT.hpp"
  llvm_cbe_tmp__190__PHI_TEMPORARY = llvm_cbe_virtualfn;   /* for PHI node */
  goto llvm_cbe_fn_2e_end;

llvm_cbe_fn_2e_nonvirtual:
#line 210 "CStateT.hpp"
  llvm_cbe_fn12 = *llvm_cbe_mem_2e_fn_2e_ptr;
#line 210 "CStateT.hpp"
  llvm_cbe_tmp__189 = ((void  (*) (struct l_class_OC_Uec_KD__KD_UecDirectTransfer *))(unsigned long)llvm_cbe_fn12);
#line 210 "CStateT.hpp"
  llvm_cbe_tmp__190__PHI_TEMPORARY = llvm_cbe_tmp__189;   /* for PHI node */
  goto llvm_cbe_fn_2e_end;

llvm_cbe_fn_2e_end:
#line 210 "CStateT.hpp"
  llvm_cbe_tmp__190 = llvm_cbe_tmp__190__PHI_TEMPORARY;
#line 210 "CStateT.hpp"
  llvm_cbe_tmp__190(llvm_cbe_this10);
#line 212 "CStateT.hpp"
  return;

llvm_cbe_if_2e_end:
#line 212 "CStateT.hpp"
  return;
}


#line 0 "LLVM INTERNAL"
void _ZN7CStateTIN3Uec17UecDirectTransferEED2Ev(struct l_class_OC_CStateT *llvm_cbe_this) {
  struct l_class_OC_CStateT *llvm_cbe_this_2e_addr;    /* Address-exposed local */
  struct l_class_OC_CStateT *llvm_cbe_this1;

#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_this_2e_addr) = llvm_cbe_this;
#line 0 "LLVM INTERNAL"
  llvm_cbe_this1 = *(&llvm_cbe_this_2e_addr);
#line 0 "LLVM INTERNAL"
  *(((unsigned char ***)llvm_cbe_this1)) = ((&_ZTV7CStateTIN3Uec17UecDirectTransferEE.array[((signed long long )2ull)]));
#line 80 "CStateT.hpp"
  _ZN10CStateBaseD2Ev((((struct l_class_OC_CStateBase *)llvm_cbe_this1)));
#line 80 "CStateT.hpp"
  return;
}


#line 0 "LLVM INTERNAL"
void _ZN10CStateBaseD1Ev(struct l_class_OC_CStateBase *llvm_cbe_this) {
  struct l_class_OC_CStateBase *llvm_cbe_this_2e_addr;    /* Address-exposed local */
  struct l_class_OC_CStateBase *llvm_cbe_this1;

#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_this_2e_addr) = llvm_cbe_this;
#line 0 "LLVM INTERNAL"
  llvm_cbe_this1 = *(&llvm_cbe_this_2e_addr);
#line 0 "LLVM INTERNAL"
  _ZN10CStateBaseD2Ev(llvm_cbe_this1);
#line 74 "CStateBase.hpp"
  return;
}


#line 0 "LLVM INTERNAL"
void _ZN10CStateBaseD0Ev(struct l_class_OC_CStateBase *llvm_cbe_this) {
  struct l_class_OC_CStateBase *llvm_cbe_this_2e_addr;    /* Address-exposed local */
  struct l_class_OC_CStateBase *llvm_cbe_this1;

#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_this_2e_addr) = llvm_cbe_this;
#line 0 "LLVM INTERNAL"
  llvm_cbe_this1 = *(&llvm_cbe_this_2e_addr);
#line 0 "LLVM INTERNAL"
  _ZN10CStateBaseD1Ev(llvm_cbe_this1);
#line 0 "LLVM INTERNAL"
  _ZdlPv((((unsigned char *)llvm_cbe_this1)));
#line 74 "CStateBase.hpp"
  return;
}


#line 0 "LLVM INTERNAL"
void _ZNSs12_Alloc_hiderD1Ev(struct l_struct_OC_std_KD__KD_basic_string_MD_char_MC__AC_std_KD__KD_char_traits_MD_char_OD__MC__AC_std_KD__KD_allocator_MD_char_OD__AC__OD__KD__KD__Alloc_hider *llvm_cbe_this) {
  struct l_struct_OC_std_KD__KD_basic_string_MD_char_MC__AC_std_KD__KD_char_traits_MD_char_OD__MC__AC_std_KD__KD_allocator_MD_char_OD__AC__OD__KD__KD__Alloc_hider *llvm_cbe_this_2e_addr;    /* Address-exposed local */
  struct l_struct_OC_std_KD__KD_basic_string_MD_char_MC__AC_std_KD__KD_char_traits_MD_char_OD__MC__AC_std_KD__KD_allocator_MD_char_OD__AC__OD__KD__KD__Alloc_hider *llvm_cbe_this1;

#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_this_2e_addr) = llvm_cbe_this;
#line 0 "LLVM INTERNAL"
  llvm_cbe_this1 = *(&llvm_cbe_this_2e_addr);
#line 0 "LLVM INTERNAL"
  _ZNSs12_Alloc_hiderD2Ev(llvm_cbe_this1);
#line 251 "basic_string.h"
  return;
}


#line 0 "LLVM INTERNAL"
void _ZNSs12_Alloc_hiderD2Ev(struct l_struct_OC_std_KD__KD_basic_string_MD_char_MC__AC_std_KD__KD_char_traits_MD_char_OD__MC__AC_std_KD__KD_allocator_MD_char_OD__AC__OD__KD__KD__Alloc_hider *llvm_cbe_this) {
  struct l_struct_OC_std_KD__KD_basic_string_MD_char_MC__AC_std_KD__KD_char_traits_MD_char_OD__MC__AC_std_KD__KD_allocator_MD_char_OD__AC__OD__KD__KD__Alloc_hider *llvm_cbe_this_2e_addr;    /* Address-exposed local */
  struct l_struct_OC_std_KD__KD_basic_string_MD_char_MC__AC_std_KD__KD_char_traits_MD_char_OD__MC__AC_std_KD__KD_allocator_MD_char_OD__AC__OD__KD__KD__Alloc_hider *llvm_cbe_this1;

#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_this_2e_addr) = llvm_cbe_this;
#line 0 "LLVM INTERNAL"
  llvm_cbe_this1 = *(&llvm_cbe_this_2e_addr);
#line 0 "LLVM INTERNAL"
  _ZNSaIcED2Ev((((struct l_class_OC_BaseEvent *)llvm_cbe_this1)));
#line 251 "basic_string.h"
  return;
}


#line 0 "LLVM INTERNAL"
void _ZN9__gnu_cxx13new_allocatorIcED2Ev(struct l_class_OC_BaseEvent *llvm_cbe_this) {
  unsigned char *llvm_cbe__rethrow;    /* Address-exposed local */
  struct l_class_OC_BaseEvent *llvm_cbe_this_2e_addr;    /* Address-exposed local */
  struct l_class_OC_BaseEvent *llvm_cbe_this1;

#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_this_2e_addr) = llvm_cbe_this;
#line 0 "LLVM INTERNAL"
  llvm_cbe_this1 = *(&llvm_cbe_this_2e_addr);
#line 73 "new_allocator.h"
  return;
}


#line 0 "LLVM INTERNAL"
void _ZN9__gnu_cxx13new_allocatorIcEC2ERKS1_(struct l_class_OC_BaseEvent *llvm_cbe_this, struct l_class_OC_BaseEvent *llvm_cbe_tmp__191) {
  unsigned char *llvm_cbe__rethrow;    /* Address-exposed local */
  struct l_class_OC_BaseEvent *llvm_cbe_this_2e_addr;    /* Address-exposed local */
  struct l_class_OC_BaseEvent *llvm_cbe__2e_addr;    /* Address-exposed local */
  struct l_class_OC_BaseEvent *llvm_cbe_this1;

#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_this_2e_addr) = llvm_cbe_this;
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe__2e_addr) = llvm_cbe_tmp__191;
#line 0 "LLVM INTERNAL"
  llvm_cbe_this1 = *(&llvm_cbe_this_2e_addr);
#line 68 "new_allocator.h"
  return;
}


#line 0 "LLVM INTERNAL"
void _ZN9__gnu_cxx13new_allocatorIcEC2Ev(struct l_class_OC_BaseEvent *llvm_cbe_this) {
  unsigned char *llvm_cbe__rethrow;    /* Address-exposed local */
  struct l_class_OC_BaseEvent *llvm_cbe_this_2e_addr;    /* Address-exposed local */
  struct l_class_OC_BaseEvent *llvm_cbe_this1;

#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_this_2e_addr) = llvm_cbe_this;
#line 0 "LLVM INTERNAL"
  llvm_cbe_this1 = *(&llvm_cbe_this_2e_addr);
#line 66 "new_allocator.h"
  return;
}


static unsigned int _Z18__gthread_active_pv(void);

#line 0 "LLVM INTERNAL"
static unsigned int _ZN9__gnu_cxx27__exchange_and_add_dispatchEPii(unsigned int *llvm_cbe___mem, unsigned int llvm_cbe___val) {
  unsigned int llvm_cbe_retval;    /* Address-exposed local */
  unsigned int *llvm_cbe___mem_2e_addr;    /* Address-exposed local */
  unsigned int llvm_cbe___val_2e_addr;    /* Address-exposed local */
  unsigned int llvm_cbe_call;
  unsigned int *llvm_cbe_tmp;
  unsigned int llvm_cbe_tmp1;
  unsigned int llvm_cbe_call2;
  unsigned int llvm_cbe_call5;
  unsigned int llvm_cbe_tmp__192;

#line 0 "LLVM INTERNAL"
  *(&llvm_cbe___mem_2e_addr) = llvm_cbe___mem;
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe___val_2e_addr) = llvm_cbe___val;
#line 78 "atomicity.h"
  llvm_cbe_call = _Z18__gthread_active_pv();
#line 79 "atomicity.h"
  llvm_cbe_tmp = *(&llvm_cbe___mem_2e_addr);
#line 79 "atomicity.h"
  llvm_cbe_tmp1 = *(&llvm_cbe___val_2e_addr);
#line 78 "atomicity.h"
  if ((llvm_cbe_call != 0u)) {    goto llvm_cbe_if_2e_then;  } else {    goto llvm_cbe_if_2e_else;  }


llvm_cbe_if_2e_then:
#line 79 "atomicity.h"
  llvm_cbe_call2 = _ZN9__gnu_cxx18__exchange_and_addEPVii(llvm_cbe_tmp, llvm_cbe_tmp1);
#line 79 "atomicity.h"
  *(&llvm_cbe_retval) = llvm_cbe_call2;
#line 79 "atomicity.h"
  goto llvm_cbe_return;

llvm_cbe_if_2e_else:
#line 81 "atomicity.h"
  llvm_cbe_call5 = _ZN9__gnu_cxx25__exchange_and_add_singleEPii(llvm_cbe_tmp, llvm_cbe_tmp1);
#line 81 "atomicity.h"
  *(&llvm_cbe_retval) = llvm_cbe_call5;
#line 81 "atomicity.h"
  goto llvm_cbe_return;

llvm_cbe_return:
#line 85 "atomicity.h"
  llvm_cbe_tmp__192 = *(&llvm_cbe_retval);
#line 85 "atomicity.h"
  return llvm_cbe_tmp__192;
}


#line 0 "LLVM INTERNAL"
static unsigned int _Z18__gthread_active_pv(void) {
  unsigned int llvm_cbe_retval;    /* Address-exposed local */
  unsigned char *llvm_cbe_tmp;
  unsigned int llvm_cbe_tmp__193;

#line 242 "gthr-default.h"
  llvm_cbe_tmp = *(&_ZZ18__gthread_active_pvE20__gthread_active_ptr);
#line 242 "gthr-default.h"
  *(&llvm_cbe_retval) = (((unsigned int )(bool )(llvm_cbe_tmp != ((unsigned char *)/*NULL*/0))));
#line 243 "gthr-default.h"
  llvm_cbe_tmp__193 = *(&llvm_cbe_retval);
#line 243 "gthr-default.h"
  return llvm_cbe_tmp__193;
}


#line 0 "LLVM INTERNAL"
static unsigned int _ZN9__gnu_cxx18__exchange_and_addEPVii(unsigned int *llvm_cbe___mem, unsigned int llvm_cbe___val) {
  unsigned int llvm_cbe_retval;    /* Address-exposed local */
  unsigned int *llvm_cbe___mem_2e_addr;    /* Address-exposed local */
  unsigned int llvm_cbe___val_2e_addr;    /* Address-exposed local */
  unsigned int *llvm_cbe_tmp;
  unsigned int llvm_cbe_tmp1;
  unsigned int llvm_cbe_tmp__194;
  unsigned int llvm_cbe_tmp__195;

#line 0 "LLVM INTERNAL"
  *(&llvm_cbe___mem_2e_addr) = llvm_cbe___mem;
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe___val_2e_addr) = llvm_cbe___val;
#line 46 "atomicity.h"
  llvm_cbe_tmp = *(&llvm_cbe___mem_2e_addr);
#line 46 "atomicity.h"
  llvm_cbe_tmp1 = *(&llvm_cbe___val_2e_addr);
#line 46 "atomicity.h"
  __sync_synchronize();
#line 46 "atomicity.h"
  llvm_cbe_tmp__194 = __sync_fetch_and_add(llvm_cbe_tmp, llvm_cbe_tmp1);
#line 46 "atomicity.h"
  __sync_synchronize();
#line 46 "atomicity.h"
  *(&llvm_cbe_retval) = llvm_cbe_tmp__194;
#line 46 "atomicity.h"
  llvm_cbe_tmp__195 = *(&llvm_cbe_retval);
#line 46 "atomicity.h"
  return llvm_cbe_tmp__195;
}


#line 0 "LLVM INTERNAL"
static unsigned int _ZN9__gnu_cxx25__exchange_and_add_singleEPii(unsigned int *llvm_cbe___mem, unsigned int llvm_cbe___val) {
  unsigned int llvm_cbe_retval;    /* Address-exposed local */
  unsigned int *llvm_cbe___mem_2e_addr;    /* Address-exposed local */
  unsigned int llvm_cbe___val_2e_addr;    /* Address-exposed local */
  unsigned int llvm_cbe___result;    /* Address-exposed local */
  unsigned int *llvm_cbe_tmp;
  unsigned int llvm_cbe_tmp1;
  unsigned int llvm_cbe_tmp2;
  unsigned int *llvm_cbe_tmp3;
  unsigned int llvm_cbe_tmp4;
  unsigned int llvm_cbe_tmp5;
  unsigned int llvm_cbe_tmp__196;

#line 0 "LLVM INTERNAL"
  *(&llvm_cbe___mem_2e_addr) = llvm_cbe___mem;
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe___val_2e_addr) = llvm_cbe___val;
#line 64 "atomicity.h"
  llvm_cbe_tmp = *(&llvm_cbe___mem_2e_addr);
#line 64 "atomicity.h"
  llvm_cbe_tmp1 = *llvm_cbe_tmp;
#line 64 "atomicity.h"
  *(&llvm_cbe___result) = llvm_cbe_tmp1;
#line 65 "atomicity.h"
  llvm_cbe_tmp2 = *(&llvm_cbe___val_2e_addr);
#line 65 "atomicity.h"
  llvm_cbe_tmp3 = *(&llvm_cbe___mem_2e_addr);
#line 65 "atomicity.h"
  llvm_cbe_tmp4 = *llvm_cbe_tmp3;
#line 65 "atomicity.h"
  *llvm_cbe_tmp3 = (((unsigned int )(((unsigned int )llvm_cbe_tmp4) + ((unsigned int )llvm_cbe_tmp2))));
#line 66 "atomicity.h"
  llvm_cbe_tmp5 = *(&llvm_cbe___result);
#line 66 "atomicity.h"
  *(&llvm_cbe_retval) = llvm_cbe_tmp5;
#line 67 "atomicity.h"
  llvm_cbe_tmp__196 = *(&llvm_cbe_retval);
#line 67 "atomicity.h"
  return llvm_cbe_tmp__196;
}


#line 0 "LLVM INTERNAL"
void _ZN3Uec27UEC_CTY_TupuProceedLockStepC2Ev(struct l_struct_OC_SAntennaInfo *llvm_cbe_this) {
  struct l_struct_OC_SAntennaInfo *llvm_cbe_this_2e_addr;    /* Address-exposed local */
  struct l_struct_OC_SAntennaInfo *llvm_cbe_this1;

#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_this_2e_addr) = llvm_cbe_this;
#line 0 "LLVM INTERNAL"
  llvm_cbe_this1 = *(&llvm_cbe_this_2e_addr);
#line 0 "LLVM INTERNAL"
  *((&llvm_cbe_this1->field0)) = 0u;
#line 628 "UecMessages.hpp"
  return;
}


#line 0 "LLVM INTERNAL"
void _ZN3Uec16UecUeContextData28clearHandoverRestrictionListEv(struct l_class_OC_Uec_KD__KD_UecUeContextData *llvm_cbe_this) {
  struct l_class_OC_Uec_KD__KD_UecUeContextData *llvm_cbe_this_2e_addr;    /* Address-exposed local */
  unsigned int llvm_cbe_i;    /* Address-exposed local */
  struct l_class_OC_Uec_KD__KD_UecUeContextData *llvm_cbe_this1;
  unsigned int llvm_cbe_tmp;
  unsigned int llvm_cbe_tmp2;
  struct l_struct_OC_SErrcN4TxAntennaTm5 *llvm_cbe_tmp10;
  unsigned int llvm_cbe_tmp12;
  struct l_struct_OC_SErrcN4TxAntennaTm5 *llvm_cbe_tmp23;
  unsigned int llvm_cbe_tmp24;
  unsigned int llvm_cbe_tmp35;
  unsigned int llvm_cbe_tmp46;
  struct l_struct_OC_SErrcN4TxAntennaTm5 *llvm_cbe_tmp57;
  unsigned int llvm_cbe_tmp60;
  struct l_struct_OC_SErrcN4TxAntennaTm5 *llvm_cbe_tmp71;
  unsigned int llvm_cbe_tmp75;
  unsigned int llvm_cbe_tmp87;
  unsigned int llvm_cbe_tmp98;
  unsigned char *llvm_cbe_tmp__197;
  unsigned int llvm_cbe_tmp108;

#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_this_2e_addr) = llvm_cbe_this;
#line 0 "LLVM INTERNAL"
  llvm_cbe_this1 = *(&llvm_cbe_this_2e_addr);
#line 2070 "UecUeContextData.hpp"
  *(&llvm_cbe_i) = 0u;
#line 2070 "UecUeContextData.hpp"
  goto llvm_cbe_for_2e_cond;
#line 2070 "UecUeContextData.hpp"
  do {     /* Syntactic loop 'for.cond' to make GCC happy */
llvm_cbe_for_2e_cond:
#line 2070 "UecUeContextData.hpp"
  llvm_cbe_tmp = *(&llvm_cbe_i);
#line 2070 "UecUeContextData.hpp"
  if ((((signed int )llvm_cbe_tmp) < ((signed int )16u))) {    goto llvm_cbe_for_2e_body;  } else {    goto llvm_cbe_for_2e_end;  }


llvm_cbe_if_2e_end86:
#line 2084 "UecUeContextData.hpp"
  llvm_cbe_tmp87 = *(&llvm_cbe_i);
#line 2084 "UecUeContextData.hpp"
  *((&((&((&((&(*((&((&((&((&((&llvm_cbe_this1->field1))->field25))->field13))->field6))->field1))).array[((signed int )0u)]))[((signed long long )(((signed long long )(signed int )llvm_cbe_tmp87)))]))->field1))->field0)) = ((unsigned short )0);
#line 2085 "UecUeContextData.hpp"
  llvm_cbe_tmp98 = *(&llvm_cbe_i);
#line 0 "LLVM INTERNAL"
  llvm_cbe_tmp__197 = memset((((unsigned char *)((&((&((&(*((&((&((&((&((&llvm_cbe_this1->field1))->field25))->field13))->field6))->field1))).array[((signed int )0u)]))[((signed long long )(((signed long long )(signed int )llvm_cbe_tmp98)))]))->field0)))), 0u, 12ull);
#line 2070 "UecUeContextData.hpp"
  llvm_cbe_tmp108 = *(&llvm_cbe_i);
#line 2070 "UecUeContextData.hpp"
  *(&llvm_cbe_i) = (((unsigned int )(((unsigned int )llvm_cbe_tmp108) + ((unsigned int )1u))));
#line 2070 "UecUeContextData.hpp"
  goto llvm_cbe_for_2e_cond;

llvm_cbe_if_2e_end:
#line 2077 "UecUeContextData.hpp"
  llvm_cbe_tmp35 = *(&llvm_cbe_i);
#line 2077 "UecUeContextData.hpp"
  *((&((&((&((&(*((&((&((&((&((&llvm_cbe_this1->field1))->field25))->field13))->field4))->field1))).array[((signed int )0u)]))[((signed long long )(((signed long long )(signed int )llvm_cbe_tmp35)))]))->field1))->field0)) = ((unsigned short )0);
#line 2079 "UecUeContextData.hpp"
  llvm_cbe_tmp46 = *(&llvm_cbe_i);
#line 2079 "UecUeContextData.hpp"
  llvm_cbe_tmp57 = *((&((&((&((&(*((&((&((&((&((&llvm_cbe_this1->field1))->field25))->field13))->field6))->field1))).array[((signed int )0u)]))[((signed long long )(((signed long long )(signed int )llvm_cbe_tmp46)))]))->field1))->field1));
#line 2079 "UecUeContextData.hpp"
  if ((llvm_cbe_tmp57 != ((struct l_struct_OC_SErrcN4TxAntennaTm5 *)/*NULL*/0))) {    goto llvm_cbe_if_2e_then59;  } else {    goto llvm_cbe_if_2e_end86;  }


llvm_cbe_for_2e_body:
#line 2072 "UecUeContextData.hpp"
  llvm_cbe_tmp2 = *(&llvm_cbe_i);
#line 2072 "UecUeContextData.hpp"
  llvm_cbe_tmp10 = *((&((&((&((&(*((&((&((&((&((&llvm_cbe_this1->field1))->field25))->field13))->field4))->field1))).array[((signed int )0u)]))[((signed long long )(((signed long long )(signed int )llvm_cbe_tmp2)))]))->field1))->field1));
#line 2072 "UecUeContextData.hpp"
  if ((llvm_cbe_tmp10 != ((struct l_struct_OC_SErrcN4TxAntennaTm5 *)/*NULL*/0))) {    goto llvm_cbe_if_2e_then;  } else {    goto llvm_cbe_if_2e_end;  }


llvm_cbe_delete_2e_end:
#line 2075 "UecUeContextData.hpp"
  llvm_cbe_tmp24 = *(&llvm_cbe_i);
#line 2075 "UecUeContextData.hpp"
  *((&((&((&((&(*((&((&((&((&((&llvm_cbe_this1->field1))->field25))->field13))->field4))->field1))).array[((signed int )0u)]))[((signed long long )(((signed long long )(signed int )llvm_cbe_tmp24)))]))->field1))->field1)) = ((struct l_struct_OC_SErrcN4TxAntennaTm5 *)/*NULL*/0);
#line 2076 "UecUeContextData.hpp"
  goto llvm_cbe_if_2e_end;

llvm_cbe_if_2e_then:
#line 2074 "UecUeContextData.hpp"
  llvm_cbe_tmp12 = *(&llvm_cbe_i);
#line 2074 "UecUeContextData.hpp"
  llvm_cbe_tmp23 = *((&((&((&((&(*((&((&((&((&((&llvm_cbe_this1->field1))->field25))->field13))->field4))->field1))).array[((signed int )0u)]))[((signed long long )(((signed long long )(signed int )llvm_cbe_tmp12)))]))->field1))->field1));
#line 2074 "UecUeContextData.hpp"
  if ((llvm_cbe_tmp23 == ((struct l_struct_OC_SErrcN4TxAntennaTm5 *)/*NULL*/0))) {    goto llvm_cbe_delete_2e_end;  } else {    goto llvm_cbe_delete_2e_notnull;  }


llvm_cbe_delete_2e_notnull:
#line 2074 "UecUeContextData.hpp"
  _ZdaPv((((unsigned char *)llvm_cbe_tmp23)));
#line 2074 "UecUeContextData.hpp"
  goto llvm_cbe_delete_2e_end;

llvm_cbe_delete_2e_end74:
#line 2082 "UecUeContextData.hpp"
  llvm_cbe_tmp75 = *(&llvm_cbe_i);
#line 2082 "UecUeContextData.hpp"
  *((&((&((&((&(*((&((&((&((&((&llvm_cbe_this1->field1))->field25))->field13))->field6))->field1))).array[((signed int )0u)]))[((signed long long )(((signed long long )(signed int )llvm_cbe_tmp75)))]))->field1))->field1)) = ((struct l_struct_OC_SErrcN4TxAntennaTm5 *)/*NULL*/0);
#line 2083 "UecUeContextData.hpp"
  goto llvm_cbe_if_2e_end86;

llvm_cbe_if_2e_then59:
#line 2081 "UecUeContextData.hpp"
  llvm_cbe_tmp60 = *(&llvm_cbe_i);
#line 2081 "UecUeContextData.hpp"
  llvm_cbe_tmp71 = *((&((&((&((&(*((&((&((&((&((&llvm_cbe_this1->field1))->field25))->field13))->field6))->field1))).array[((signed int )0u)]))[((signed long long )(((signed long long )(signed int )llvm_cbe_tmp60)))]))->field1))->field1));
#line 2081 "UecUeContextData.hpp"
  if ((llvm_cbe_tmp71 == ((struct l_struct_OC_SErrcN4TxAntennaTm5 *)/*NULL*/0))) {    goto llvm_cbe_delete_2e_end74;  } else {    goto llvm_cbe_delete_2e_notnull73;  }


llvm_cbe_delete_2e_notnull73:
#line 2081 "UecUeContextData.hpp"
  _ZdaPv((((unsigned char *)llvm_cbe_tmp71)));
#line 2081 "UecUeContextData.hpp"
  goto llvm_cbe_delete_2e_end74;
  } while (1); /* end of syntactic loop 'for.cond' */
llvm_cbe_for_2e_end:
#line 2087 "UecUeContextData.hpp"
  return;
}


#line 0 "LLVM INTERNAL"
struct l_class_OC_BaseEvent *_ZN11DummyStreamlsI23EErrcEstablishmentCauseEERS_T_(struct l_class_OC_BaseEvent *llvm_cbe_this, unsigned int llvm_cbe_arg) {
  struct l_class_OC_BaseEvent *llvm_cbe_retval;    /* Address-exposed local */
  struct l_class_OC_BaseEvent *llvm_cbe_this_2e_addr;    /* Address-exposed local */
  unsigned int llvm_cbe_arg_2e_addr;    /* Address-exposed local */
  struct l_class_OC_BaseEvent *llvm_cbe_this1;
  struct l_class_OC_BaseEvent *llvm_cbe_tmp__198;

#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_this_2e_addr) = llvm_cbe_this;
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_arg_2e_addr) = llvm_cbe_arg;
#line 0 "LLVM INTERNAL"
  llvm_cbe_this1 = *(&llvm_cbe_this_2e_addr);
#line 30 "BaseTypes.hpp"
  *(&llvm_cbe_retval) = llvm_cbe_this1;
#line 31 "BaseTypes.hpp"
  llvm_cbe_tmp__198 = *(&llvm_cbe_retval);
#line 31 "BaseTypes.hpp"
  return llvm_cbe_tmp__198;
}


#line 0 "LLVM INTERNAL"
void _ZNSt4pairISt17_Rb_tree_iteratorIS_IKj10SAsnDynstrEEbEC1Ev(struct l_struct_OC_std_KD__KD_pair *llvm_cbe_this) {
  struct l_struct_OC_std_KD__KD_pair *llvm_cbe_this_2e_addr;    /* Address-exposed local */
  struct l_struct_OC_std_KD__KD_pair *llvm_cbe_this1;

#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_this_2e_addr) = llvm_cbe_this;
#line 0 "LLVM INTERNAL"
  llvm_cbe_this1 = *(&llvm_cbe_this_2e_addr);
#line 0 "LLVM INTERNAL"
  _ZNSt4pairISt17_Rb_tree_iteratorIS_IKj10SAsnDynstrEEbEC2Ev(llvm_cbe_this1);
#line 80 "stl_pair.h"
  return;
}


#line 0 "LLVM INTERNAL"
struct l_struct_OC_SS1apUEAggregateMaximumBitrate _ZNSt3mapIj10SAsnDynstrSt4lessIjESaISt4pairIKjS0_EEE6insertERKS5_(struct l_class_OC_std_KD__KD_map *llvm_cbe_this, struct l_unnamed78 *llvm_cbe___x) {
  struct l_struct_OC_std_KD__KD_pair llvm_cbe_retval;    /* Address-exposed local */
  struct l_class_OC_std_KD__KD_map *llvm_cbe_this_2e_addr;    /* Address-exposed local */
  struct l_unnamed78 *llvm_cbe___x_2e_addr;    /* Address-exposed local */
  struct l_class_OC_std_KD__KD_map *llvm_cbe_this1;
  struct l_unnamed78 *llvm_cbe_tmp2;
  struct l_struct_OC_SS1apUEAggregateMaximumBitrate llvm_cbe_call;
  struct l_struct_OC_SS1apUEAggregateMaximumBitrate llvm_cbe_tmp__199;

#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_this_2e_addr) = llvm_cbe_this;
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe___x_2e_addr) = llvm_cbe___x;
#line 0 "LLVM INTERNAL"
  llvm_cbe_this1 = *(&llvm_cbe_this_2e_addr);
#line 500 "stl_map.h"
  llvm_cbe_tmp2 = *(&llvm_cbe___x_2e_addr);
#line 500 "stl_map.h"
  llvm_cbe_call = _ZNSt8_Rb_treeIjSt4pairIKj10SAsnDynstrESt10_Select1stIS3_ESt4lessIjESaIS3_EE16_M_insert_uniqueERKS3_(((&llvm_cbe_this1->field0)), llvm_cbe_tmp2);
#line 500 "stl_map.h"
  ((struct __attribute__ ((packed, aligned(1))) {struct l_struct_OC_SS1apUEAggregateMaximumBitrate data; } *)(((struct l_struct_OC_SS1apUEAggregateMaximumBitrate *)(&llvm_cbe_retval))))->data = llvm_cbe_call;
#line 500 "stl_map.h"
  llvm_cbe_tmp__199 = ((struct __attribute__ ((packed, aligned(1))) {struct l_struct_OC_SS1apUEAggregateMaximumBitrate data; } *)(((struct l_struct_OC_SS1apUEAggregateMaximumBitrate *)(&llvm_cbe_retval))))->data;
#line 500 "stl_map.h"
  return llvm_cbe_tmp__199;
}


#line 0 "LLVM INTERNAL"
void _ZNSt4pairIKj10SAsnDynstrEC1IjS1_EERKS_IT_T0_E(struct l_unnamed78 *llvm_cbe_this, struct l_unnamed78 *llvm_cbe___p) {
  struct l_unnamed78 *llvm_cbe_this_2e_addr;    /* Address-exposed local */
  struct l_unnamed78 *llvm_cbe___p_2e_addr;    /* Address-exposed local */
  struct l_unnamed78 *llvm_cbe_this1;
  struct l_unnamed78 *llvm_cbe_tmp__200;

#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_this_2e_addr) = llvm_cbe_this;
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe___p_2e_addr) = llvm_cbe___p;
#line 0 "LLVM INTERNAL"
  llvm_cbe_this1 = *(&llvm_cbe_this_2e_addr);
#line 0 "LLVM INTERNAL"
  llvm_cbe_tmp__200 = *(&llvm_cbe___p_2e_addr);
#line 0 "LLVM INTERNAL"
  _ZNSt4pairIKj10SAsnDynstrEC2IjS1_EERKS_IT_T0_E(llvm_cbe_this1, llvm_cbe_tmp__200);
#line 101 "stl_pair.h"
  return;
}


#line 0 "LLVM INTERNAL"
void _ZNSt4pairIj10SAsnDynstrEC1ERKjRKS0_(struct l_unnamed78 *llvm_cbe_this, unsigned int *llvm_cbe___a, struct l_struct_OC_SAsnDynstr *llvm_cbe___b) {
  struct l_unnamed78 *llvm_cbe_this_2e_addr;    /* Address-exposed local */
  unsigned int *llvm_cbe___a_2e_addr;    /* Address-exposed local */
  struct l_struct_OC_SAsnDynstr *llvm_cbe___b_2e_addr;    /* Address-exposed local */
  struct l_unnamed78 *llvm_cbe_this1;
  unsigned int *llvm_cbe_tmp__201;
  struct l_struct_OC_SAsnDynstr *llvm_cbe_tmp__202;

#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_this_2e_addr) = llvm_cbe_this;
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe___a_2e_addr) = llvm_cbe___a;
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe___b_2e_addr) = llvm_cbe___b;
#line 0 "LLVM INTERNAL"
  llvm_cbe_this1 = *(&llvm_cbe_this_2e_addr);
#line 0 "LLVM INTERNAL"
  llvm_cbe_tmp__201 = *(&llvm_cbe___a_2e_addr);
#line 0 "LLVM INTERNAL"
  llvm_cbe_tmp__202 = *(&llvm_cbe___b_2e_addr);
#line 0 "LLVM INTERNAL"
  _ZNSt4pairIj10SAsnDynstrEC2ERKjRKS0_(llvm_cbe_this1, llvm_cbe_tmp__201, llvm_cbe_tmp__202);
#line 84 "stl_pair.h"
  return;
}


#line 0 "LLVM INTERNAL"
struct l_unnamed78 *_ZNKSt17_Rb_tree_iteratorISt4pairIKj10SAsnDynstrEEptEv(struct l_struct_OC_std_KD__KD__Rb_tree_iterator *llvm_cbe_this) {
  struct l_unnamed78 *llvm_cbe_retval;    /* Address-exposed local */
  struct l_struct_OC_std_KD__KD__Rb_tree_iterator *llvm_cbe_this_2e_addr;    /* Address-exposed local */
  struct l_struct_OC_std_KD__KD__Rb_tree_iterator *llvm_cbe_this1;
  struct l_struct_OC_std_KD__KD__Rb_tree_node_base *llvm_cbe_tmp2;
  struct l_unnamed78 *llvm_cbe_tmp__203;

#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_this_2e_addr) = llvm_cbe_this;
#line 0 "LLVM INTERNAL"
  llvm_cbe_this1 = *(&llvm_cbe_this_2e_addr);
#line 179 "stl_tree.h"
  llvm_cbe_tmp2 = *((&llvm_cbe_this1->field0));
#line 179 "stl_tree.h"
  *(&llvm_cbe_retval) = ((&(((struct l_struct_OC_std_KD__KD__Rb_tree_node *)llvm_cbe_tmp2))->field1));
#line 179 "stl_tree.h"
  llvm_cbe_tmp__203 = *(&llvm_cbe_retval);
#line 179 "stl_tree.h"
  return llvm_cbe_tmp__203;
}


#line 0 "LLVM INTERNAL"
void _ZNSt4pairIj10SAsnDynstrEC2ERKjRKS0_(struct l_unnamed78 *llvm_cbe_this, unsigned int *llvm_cbe___a, struct l_struct_OC_SAsnDynstr *llvm_cbe___b) {
  struct l_unnamed78 *llvm_cbe_this_2e_addr;    /* Address-exposed local */
  unsigned int *llvm_cbe___a_2e_addr;    /* Address-exposed local */
  struct l_struct_OC_SAsnDynstr *llvm_cbe___b_2e_addr;    /* Address-exposed local */
  struct l_unnamed78 *llvm_cbe_this1;
  unsigned int *llvm_cbe_tmp2;
  unsigned int llvm_cbe_tmp3;
  struct l_struct_OC_SAsnDynstr *llvm_cbe_tmp5;
  unsigned char *llvm_cbe_tmp__204;

#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_this_2e_addr) = llvm_cbe_this;
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe___a_2e_addr) = llvm_cbe___a;
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe___b_2e_addr) = llvm_cbe___b;
#line 0 "LLVM INTERNAL"
  llvm_cbe_this1 = *(&llvm_cbe_this_2e_addr);
#line 0 "LLVM INTERNAL"
  llvm_cbe_tmp2 = *(&llvm_cbe___a_2e_addr);
#line 0 "LLVM INTERNAL"
  llvm_cbe_tmp3 = *llvm_cbe_tmp2;
#line 0 "LLVM INTERNAL"
  *((&llvm_cbe_this1->field0)) = llvm_cbe_tmp3;
#line 0 "LLVM INTERNAL"
  llvm_cbe_tmp5 = *(&llvm_cbe___b_2e_addr);
#line 0 "LLVM INTERNAL"
  llvm_cbe_tmp__204 = memcpy((((unsigned char *)((&llvm_cbe_this1->field1)))), (((unsigned char *)llvm_cbe_tmp5)), 16ull);
#line 84 "stl_pair.h"
  return;
}


#line 0 "LLVM INTERNAL"
void _ZNSt4pairIKj10SAsnDynstrEC2IjS1_EERKS_IT_T0_E(struct l_unnamed78 *llvm_cbe_this, struct l_unnamed78 *llvm_cbe___p) {
  struct l_unnamed78 *llvm_cbe_this_2e_addr;    /* Address-exposed local */
  struct l_unnamed78 *llvm_cbe___p_2e_addr;    /* Address-exposed local */
  struct l_unnamed78 *llvm_cbe_this1;
  struct l_unnamed78 *llvm_cbe_tmp2;
  unsigned int llvm_cbe_tmp4;
  struct l_unnamed78 *llvm_cbe_tmp6;
  unsigned char *llvm_cbe_tmp__205;

#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_this_2e_addr) = llvm_cbe_this;
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe___p_2e_addr) = llvm_cbe___p;
#line 0 "LLVM INTERNAL"
  llvm_cbe_this1 = *(&llvm_cbe_this_2e_addr);
#line 0 "LLVM INTERNAL"
  llvm_cbe_tmp2 = *(&llvm_cbe___p_2e_addr);
#line 0 "LLVM INTERNAL"
  llvm_cbe_tmp4 = *((&llvm_cbe_tmp2->field0));
#line 0 "LLVM INTERNAL"
  *((&llvm_cbe_this1->field0)) = llvm_cbe_tmp4;
#line 0 "LLVM INTERNAL"
  llvm_cbe_tmp6 = *(&llvm_cbe___p_2e_addr);
#line 0 "LLVM INTERNAL"
  llvm_cbe_tmp__205 = memcpy((((unsigned char *)((&llvm_cbe_this1->field1)))), (((unsigned char *)((&llvm_cbe_tmp6->field1)))), 16ull);
#line 101 "stl_pair.h"
  return;
}


struct l_struct_OC_std_KD__KD__Rb_tree_node *_ZNSt8_Rb_treeIjSt4pairIKj10SAsnDynstrESt10_Select1stIS3_ESt4lessIjESaIS3_EE8_M_beginEv(struct l_class_OC_std_KD__KD__Rb_tree *llvm_cbe_this);

struct l_struct_OC_std_KD__KD__Rb_tree_node *_ZNSt8_Rb_treeIjSt4pairIKj10SAsnDynstrESt10_Select1stIS3_ESt4lessIjESaIS3_EE6_M_endEv(struct l_class_OC_std_KD__KD__Rb_tree *llvm_cbe_this);

#line 0 "LLVM INTERNAL"
struct l_struct_OC_SS1apUEAggregateMaximumBitrate _ZNSt8_Rb_treeIjSt4pairIKj10SAsnDynstrESt10_Select1stIS3_ESt4lessIjESaIS3_EE16_M_insert_uniqueERKS3_(struct l_class_OC_std_KD__KD__Rb_tree *llvm_cbe_this, struct l_unnamed78 *llvm_cbe___x) {
  struct l_struct_OC_std_KD__KD_pair llvm_cbe_retval;    /* Address-exposed local */
  struct l_class_OC_std_KD__KD__Rb_tree *llvm_cbe_this_2e_addr;    /* Address-exposed local */
  struct l_unnamed78 *llvm_cbe___x_2e_addr;    /* Address-exposed local */
  struct l_struct_OC_std_KD__KD__Rb_tree_node *llvm_cbe___x2;    /* Address-exposed local */
  struct l_struct_OC_std_KD__KD__Rb_tree_node *llvm_cbe___y;    /* Address-exposed local */
  unsigned char llvm_cbe___comp;    /* Address-exposed local */
  struct l_class_OC_BaseEvent llvm_cbe_tmp7;    /* Address-exposed local */
  struct l_struct_OC_std_KD__KD__Rb_tree_iterator llvm_cbe___j;    /* Address-exposed local */
  struct l_struct_OC_std_KD__KD__Rb_tree_iterator llvm_cbe_agg_2e_tmp;    /* Address-exposed local */
  struct l_struct_OC_std_KD__KD__Rb_tree_iterator llvm_cbe_agg_2e_tmp25;    /* Address-exposed local */
  unsigned char llvm_cbe_reftmp;    /* Address-exposed local */
  struct l_class_OC_BaseEvent llvm_cbe_tmp37;    /* Address-exposed local */
  struct l_struct_OC_std_KD__KD__Rb_tree_iterator llvm_cbe_agg_2e_tmp42;    /* Address-exposed local */
  unsigned char llvm_cbe_reftmp47;    /* Address-exposed local */
  unsigned char llvm_cbe_reftmp49;    /* Address-exposed local */
  struct l_class_OC_std_KD__KD__Rb_tree *llvm_cbe_this1;
  struct l_struct_OC_std_KD__KD__Rb_tree_node *llvm_cbe_call;
  struct l_struct_OC_std_KD__KD__Rb_tree_node *llvm_cbe_call3;
  struct l_struct_OC_std_KD__KD__Rb_tree_node *llvm_cbe_tmp;
  struct l_struct_OC_std_KD__KD__Rb_tree_node *llvm_cbe_tmp4;
  struct l_unnamed78 *llvm_cbe_tmp8;
  unsigned int *llvm_cbe_call9;
  struct l_struct_OC_std_KD__KD__Rb_tree_node *llvm_cbe_tmp10;
  unsigned int *llvm_cbe_call11;
  bool llvm_cbe_call12;
  unsigned char llvm_cbe_tmp13;
  struct l_struct_OC_std_KD__KD__Rb_tree_node *llvm_cbe_tmp14;
  struct l_struct_OC_std_KD__KD__Rb_tree_node_base *llvm_cbe_tmp__206;
  struct l_struct_OC_std_KD__KD__Rb_tree_node *llvm_cbe_call15;
  struct l_struct_OC_std_KD__KD__Rb_tree_node *llvm_cbe_call17;
  struct l_struct_OC_std_KD__KD__Rb_tree_node *llvm_cbe_cond;
  struct l_struct_OC_std_KD__KD__Rb_tree_node *llvm_cbe_cond__PHI_TEMPORARY;
  struct l_struct_OC_std_KD__KD__Rb_tree_node *llvm_cbe_tmp19;
  unsigned char llvm_cbe_tmp20;
  unsigned long long llvm_cbe_call22;
  bool llvm_cbe_call23;
  struct l_struct_OC_std_KD__KD__Rb_tree_node *llvm_cbe_tmp26;
  struct l_struct_OC_std_KD__KD__Rb_tree_node *llvm_cbe_tmp27;
  struct l_unnamed78 *llvm_cbe_tmp28;
  unsigned long long llvm_cbe_call29;
  struct l_struct_OC_std_KD__KD__Rb_tree_iterator *llvm_cbe_call30;
  struct l_struct_OC_std_KD__KD__Rb_tree_node_base *llvm_cbe_tmp35;
  unsigned int *llvm_cbe_call36;
  struct l_unnamed78 *llvm_cbe_tmp38;
  unsigned int *llvm_cbe_call39;
  bool llvm_cbe_call40;
  struct l_struct_OC_std_KD__KD__Rb_tree_node *llvm_cbe_tmp43;
  struct l_struct_OC_std_KD__KD__Rb_tree_node *llvm_cbe_tmp44;
  struct l_unnamed78 *llvm_cbe_tmp45;
  unsigned long long llvm_cbe_call46;
  struct l_struct_OC_SS1apUEAggregateMaximumBitrate llvm_cbe_tmp__207;

#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_this_2e_addr) = llvm_cbe_this;
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe___x_2e_addr) = llvm_cbe___x;
#line 0 "LLVM INTERNAL"
  llvm_cbe_this1 = *(&llvm_cbe_this_2e_addr);
#line 1164 "stl_tree.h"
  llvm_cbe_call = _ZNSt8_Rb_treeIjSt4pairIKj10SAsnDynstrESt10_Select1stIS3_ESt4lessIjESaIS3_EE8_M_beginEv(llvm_cbe_this1);
#line 1164 "stl_tree.h"
  *(&llvm_cbe___x2) = llvm_cbe_call;
#line 1165 "stl_tree.h"
  llvm_cbe_call3 = _ZNSt8_Rb_treeIjSt4pairIKj10SAsnDynstrESt10_Select1stIS3_ESt4lessIjESaIS3_EE6_M_endEv(llvm_cbe_this1);
#line 1165 "stl_tree.h"
  *(&llvm_cbe___y) = llvm_cbe_call3;
#line 1166 "stl_tree.h"
  *(&llvm_cbe___comp) = ((unsigned char )1);
#line 1167 "stl_tree.h"
  goto llvm_cbe_while_2e_cond;
#line 1167 "stl_tree.h"
  do {     /* Syntactic loop 'while.cond' to make GCC happy */
llvm_cbe_while_2e_cond:
#line 1167 "stl_tree.h"
  llvm_cbe_tmp = *(&llvm_cbe___x2);
#line 1167 "stl_tree.h"
  if ((llvm_cbe_tmp != ((struct l_struct_OC_std_KD__KD__Rb_tree_node *)/*NULL*/0))) {    goto llvm_cbe_while_2e_body;  } else {    goto llvm_cbe_while_2e_end;  }


llvm_cbe_cond_2e_end:
#line 1171 "stl_tree.h"
  llvm_cbe_cond = llvm_cbe_cond__PHI_TEMPORARY;
#line 1171 "stl_tree.h"
  *(&llvm_cbe___x2) = llvm_cbe_cond;
#line 1172 "stl_tree.h"
  goto llvm_cbe_while_2e_cond;

llvm_cbe_cond_2e_true:
#line 1171 "stl_tree.h"
  llvm_cbe_call15 = _ZNSt8_Rb_treeIjSt4pairIKj10SAsnDynstrESt10_Select1stIS3_ESt4lessIjESaIS3_EE7_S_leftEPSt18_Rb_tree_node_base(llvm_cbe_tmp__206);
#line 1171 "stl_tree.h"
  llvm_cbe_cond__PHI_TEMPORARY = llvm_cbe_call15;   /* for PHI node */
  goto llvm_cbe_cond_2e_end;

llvm_cbe_while_2e_body:
#line 1169 "stl_tree.h"
  llvm_cbe_tmp4 = *(&llvm_cbe___x2);
#line 1169 "stl_tree.h"
  *(&llvm_cbe___y) = llvm_cbe_tmp4;
#line 1170 "stl_tree.h"
  llvm_cbe_tmp8 = *(&llvm_cbe___x_2e_addr);
#line 1170 "stl_tree.h"
  llvm_cbe_call9 = _ZNKSt10_Select1stISt4pairIKj10SAsnDynstrEEclERKS3_((&llvm_cbe_tmp7), llvm_cbe_tmp8);
#line 1170 "stl_tree.h"
  llvm_cbe_tmp10 = *(&llvm_cbe___x2);
#line 1170 "stl_tree.h"
  llvm_cbe_call11 = _ZNSt8_Rb_treeIjSt4pairIKj10SAsnDynstrESt10_Select1stIS3_ESt4lessIjESaIS3_EE6_S_keyEPKSt13_Rb_tree_nodeIS3_E(llvm_cbe_tmp10);
#line 1170 "stl_tree.h"
  llvm_cbe_call12 = ((_ZNKSt4lessIjEclERKjS2_(((&((&llvm_cbe_this1->field0))->field0)), llvm_cbe_call9, llvm_cbe_call11))&1);
#line 1170 "stl_tree.h"
  *(&llvm_cbe___comp) = (((unsigned char )(bool )llvm_cbe_call12));
#line 1171 "stl_tree.h"
  llvm_cbe_tmp13 = *(&llvm_cbe___comp);
#line 1171 "stl_tree.h"
  llvm_cbe_tmp14 = *(&llvm_cbe___x2);
#line 1171 "stl_tree.h"
  llvm_cbe_tmp__206 = ((struct l_struct_OC_std_KD__KD__Rb_tree_node_base *)llvm_cbe_tmp14);
#line 1171 "stl_tree.h"
  if ((((((bool )llvm_cbe_tmp13&1u))&1))) {    goto llvm_cbe_cond_2e_true;  } else {    goto llvm_cbe_cond_2e_false;  }


llvm_cbe_cond_2e_false:
#line 1171 "stl_tree.h"
  llvm_cbe_call17 = _ZNSt8_Rb_treeIjSt4pairIKj10SAsnDynstrESt10_Select1stIS3_ESt4lessIjESaIS3_EE8_S_rightEPSt18_Rb_tree_node_base(llvm_cbe_tmp__206);
#line 1171 "stl_tree.h"
  llvm_cbe_cond__PHI_TEMPORARY = llvm_cbe_call17;   /* for PHI node */
  goto llvm_cbe_cond_2e_end;
  } while (1); /* end of syntactic loop 'while.cond' */
llvm_cbe_while_2e_end:
#line 1173 "stl_tree.h"
  llvm_cbe_tmp19 = *(&llvm_cbe___y);
#line 1173 "stl_tree.h"
  _ZNSt17_Rb_tree_iteratorISt4pairIKj10SAsnDynstrEEC1EPSt13_Rb_tree_nodeIS3_E((&llvm_cbe___j), llvm_cbe_tmp19);
#line 1174 "stl_tree.h"
  llvm_cbe_tmp20 = *(&llvm_cbe___comp);
#line 1174 "stl_tree.h"
  if ((((((bool )llvm_cbe_tmp20&1u))&1))) {    goto llvm_cbe_if_2e_then;  } else {    goto llvm_cbe_if_2e_end31;  }


llvm_cbe_if_2e_then:
#line 1176 "stl_tree.h"
  llvm_cbe_call22 = _ZNSt8_Rb_treeIjSt4pairIKj10SAsnDynstrESt10_Select1stIS3_ESt4lessIjESaIS3_EE5beginEv(llvm_cbe_this1);
#line 1176 "stl_tree.h"
  ((struct __attribute__ ((packed, aligned(1))) {unsigned long long data; } *)(((unsigned long long *)(&llvm_cbe_agg_2e_tmp))))->data = llvm_cbe_call22;
#line 1176 "stl_tree.h"
  llvm_cbe_call23 = ((_ZNKSt17_Rb_tree_iteratorISt4pairIKj10SAsnDynstrEEeqERKS4_((&llvm_cbe___j), (&llvm_cbe_agg_2e_tmp)))&1);
#line 1176 "stl_tree.h"
  if (llvm_cbe_call23) {    goto llvm_cbe_if_2e_then24;  } else {    goto llvm_cbe_if_2e_else;  }


llvm_cbe_if_2e_then24:
#line 1177 "stl_tree.h"
  llvm_cbe_tmp26 = *(&llvm_cbe___x2);
#line 1177 "stl_tree.h"
  llvm_cbe_tmp27 = *(&llvm_cbe___y);
#line 1177 "stl_tree.h"
  llvm_cbe_tmp28 = *(&llvm_cbe___x_2e_addr);
#line 1177 "stl_tree.h"
  llvm_cbe_call29 = _ZNSt8_Rb_treeIjSt4pairIKj10SAsnDynstrESt10_Select1stIS3_ESt4lessIjESaIS3_EE10_M_insert_EPKSt18_Rb_tree_node_baseSC_RKS3_(llvm_cbe_this1, (((struct l_struct_OC_std_KD__KD__Rb_tree_node_base *)llvm_cbe_tmp26)), (((struct l_struct_OC_std_KD__KD__Rb_tree_node_base *)llvm_cbe_tmp27)), llvm_cbe_tmp28);
#line 1177 "stl_tree.h"
  ((struct __attribute__ ((packed, aligned(1))) {unsigned long long data; } *)(((unsigned long long *)(&llvm_cbe_agg_2e_tmp25))))->data = llvm_cbe_call29;
#line 1177 "stl_tree.h"
  *(&llvm_cbe_reftmp) = ((unsigned char )1);
#line 1177 "stl_tree.h"
  _ZNSt4pairISt17_Rb_tree_iteratorIS_IKj10SAsnDynstrEEbEC1ERKS4_RKb((&llvm_cbe_retval), (&llvm_cbe_agg_2e_tmp25), (&llvm_cbe_reftmp));
#line 1177 "stl_tree.h"
  goto llvm_cbe_return;

llvm_cbe_if_2e_else:
#line 1179 "stl_tree.h"
  llvm_cbe_call30 = _ZNSt17_Rb_tree_iteratorISt4pairIKj10SAsnDynstrEEmmEv((&llvm_cbe___j));
#line 1179 "stl_tree.h"
  goto llvm_cbe_if_2e_end31;

llvm_cbe_if_2e_end31:
#line 1181 "stl_tree.h"
  llvm_cbe_tmp35 = *((&llvm_cbe___j.field0));
#line 1181 "stl_tree.h"
  llvm_cbe_call36 = _ZNSt8_Rb_treeIjSt4pairIKj10SAsnDynstrESt10_Select1stIS3_ESt4lessIjESaIS3_EE6_S_keyEPKSt18_Rb_tree_node_base(llvm_cbe_tmp35);
#line 1181 "stl_tree.h"
  llvm_cbe_tmp38 = *(&llvm_cbe___x_2e_addr);
#line 1181 "stl_tree.h"
  llvm_cbe_call39 = _ZNKSt10_Select1stISt4pairIKj10SAsnDynstrEEclERKS3_((&llvm_cbe_tmp37), llvm_cbe_tmp38);
#line 1181 "stl_tree.h"
  llvm_cbe_call40 = ((_ZNKSt4lessIjEclERKjS2_(((&((&llvm_cbe_this1->field0))->field0)), llvm_cbe_call36, llvm_cbe_call39))&1);
#line 1181 "stl_tree.h"
  if (llvm_cbe_call40) {    goto llvm_cbe_if_2e_then41;  } else {    goto llvm_cbe_if_2e_end48;  }


llvm_cbe_if_2e_then41:
#line 1182 "stl_tree.h"
  llvm_cbe_tmp43 = *(&llvm_cbe___x2);
#line 1182 "stl_tree.h"
  llvm_cbe_tmp44 = *(&llvm_cbe___y);
#line 1182 "stl_tree.h"
  llvm_cbe_tmp45 = *(&llvm_cbe___x_2e_addr);
#line 1182 "stl_tree.h"
  llvm_cbe_call46 = _ZNSt8_Rb_treeIjSt4pairIKj10SAsnDynstrESt10_Select1stIS3_ESt4lessIjESaIS3_EE10_M_insert_EPKSt18_Rb_tree_node_baseSC_RKS3_(llvm_cbe_this1, (((struct l_struct_OC_std_KD__KD__Rb_tree_node_base *)llvm_cbe_tmp43)), (((struct l_struct_OC_std_KD__KD__Rb_tree_node_base *)llvm_cbe_tmp44)), llvm_cbe_tmp45);
#line 1182 "stl_tree.h"
  ((struct __attribute__ ((packed, aligned(1))) {unsigned long long data; } *)(((unsigned long long *)(&llvm_cbe_agg_2e_tmp42))))->data = llvm_cbe_call46;
#line 1182 "stl_tree.h"
  *(&llvm_cbe_reftmp47) = ((unsigned char )1);
#line 1182 "stl_tree.h"
  _ZNSt4pairISt17_Rb_tree_iteratorIS_IKj10SAsnDynstrEEbEC1ERKS4_RKb((&llvm_cbe_retval), (&llvm_cbe_agg_2e_tmp42), (&llvm_cbe_reftmp47));
#line 1182 "stl_tree.h"
  goto llvm_cbe_return;

llvm_cbe_if_2e_end48:
#line 1183 "stl_tree.h"
  *(&llvm_cbe_reftmp49) = ((unsigned char )0);
#line 1183 "stl_tree.h"
  _ZNSt4pairISt17_Rb_tree_iteratorIS_IKj10SAsnDynstrEEbEC1ERKS4_RKb((&llvm_cbe_retval), (&llvm_cbe___j), (&llvm_cbe_reftmp49));
#line 1183 "stl_tree.h"
  goto llvm_cbe_return;

llvm_cbe_return:
#line 1184 "stl_tree.h"
  llvm_cbe_tmp__207 = ((struct __attribute__ ((packed, aligned(1))) {struct l_struct_OC_SS1apUEAggregateMaximumBitrate data; } *)(((struct l_struct_OC_SS1apUEAggregateMaximumBitrate *)(&llvm_cbe_retval))))->data;
#line 1184 "stl_tree.h"
  return llvm_cbe_tmp__207;
}


#line 0 "LLVM INTERNAL"
struct l_struct_OC_std_KD__KD__Rb_tree_node *_ZNSt8_Rb_treeIjSt4pairIKj10SAsnDynstrESt10_Select1stIS3_ESt4lessIjESaIS3_EE8_M_beginEv(struct l_class_OC_std_KD__KD__Rb_tree *llvm_cbe_this) {
  struct l_struct_OC_std_KD__KD__Rb_tree_node *llvm_cbe_retval;    /* Address-exposed local */
  struct l_class_OC_std_KD__KD__Rb_tree *llvm_cbe_this_2e_addr;    /* Address-exposed local */
  struct l_class_OC_std_KD__KD__Rb_tree *llvm_cbe_this1;
  struct l_struct_OC_std_KD__KD__Rb_tree_node_base *llvm_cbe_tmp4;
  struct l_struct_OC_std_KD__KD__Rb_tree_node *llvm_cbe_tmp__208;

#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_this_2e_addr) = llvm_cbe_this;
#line 0 "LLVM INTERNAL"
  llvm_cbe_this1 = *(&llvm_cbe_this_2e_addr);
#line 482 "stl_tree.h"
  llvm_cbe_tmp4 = *((&((&((&llvm_cbe_this1->field0))->field1))->field1));
#line 482 "stl_tree.h"
  *(&llvm_cbe_retval) = (((struct l_struct_OC_std_KD__KD__Rb_tree_node *)llvm_cbe_tmp4));
#line 482 "stl_tree.h"
  llvm_cbe_tmp__208 = *(&llvm_cbe_retval);
#line 482 "stl_tree.h"
  return llvm_cbe_tmp__208;
}


#line 0 "LLVM INTERNAL"
struct l_struct_OC_std_KD__KD__Rb_tree_node *_ZNSt8_Rb_treeIjSt4pairIKj10SAsnDynstrESt10_Select1stIS3_ESt4lessIjESaIS3_EE6_M_endEv(struct l_class_OC_std_KD__KD__Rb_tree *llvm_cbe_this) {
  struct l_struct_OC_std_KD__KD__Rb_tree_node *llvm_cbe_retval;    /* Address-exposed local */
  struct l_class_OC_std_KD__KD__Rb_tree *llvm_cbe_this_2e_addr;    /* Address-exposed local */
  struct l_class_OC_std_KD__KD__Rb_tree *llvm_cbe_this1;
  struct l_struct_OC_std_KD__KD__Rb_tree_node *llvm_cbe_tmp__209;

#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_this_2e_addr) = llvm_cbe_this;
#line 0 "LLVM INTERNAL"
  llvm_cbe_this1 = *(&llvm_cbe_this_2e_addr);
#line 493 "stl_tree.h"
  *(&llvm_cbe_retval) = (((struct l_struct_OC_std_KD__KD__Rb_tree_node *)((&((&llvm_cbe_this1->field0))->field1))));
#line 493 "stl_tree.h"
  llvm_cbe_tmp__209 = *(&llvm_cbe_retval);
#line 493 "stl_tree.h"
  return llvm_cbe_tmp__209;
}


#line 0 "LLVM INTERNAL"
bool _ZNKSt4lessIjEclERKjS2_(struct l_class_OC_BaseEvent *llvm_cbe_this, unsigned int *llvm_cbe___x, unsigned int *llvm_cbe___y) {
  bool llvm_cbe_retval;    /* Address-exposed local */
  struct l_class_OC_BaseEvent *llvm_cbe_this_2e_addr;    /* Address-exposed local */
  unsigned int *llvm_cbe___x_2e_addr;    /* Address-exposed local */
  unsigned int *llvm_cbe___y_2e_addr;    /* Address-exposed local */
  struct l_class_OC_BaseEvent *llvm_cbe_this1;
  unsigned int *llvm_cbe_tmp;
  unsigned int llvm_cbe_tmp2;
  unsigned int *llvm_cbe_tmp3;
  unsigned int llvm_cbe_tmp4;
  bool llvm_cbe_tmp__210;

#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_this_2e_addr) = llvm_cbe_this;
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe___x_2e_addr) = llvm_cbe___x;
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe___y_2e_addr) = llvm_cbe___y;
#line 0 "LLVM INTERNAL"
  llvm_cbe_this1 = *(&llvm_cbe_this_2e_addr);
#line 230 "stl_function.h"
  llvm_cbe_tmp = *(&llvm_cbe___x_2e_addr);
#line 230 "stl_function.h"
  llvm_cbe_tmp2 = *llvm_cbe_tmp;
#line 230 "stl_function.h"
  llvm_cbe_tmp3 = *(&llvm_cbe___y_2e_addr);
#line 230 "stl_function.h"
  llvm_cbe_tmp4 = *llvm_cbe_tmp3;
#line 230 "stl_function.h"
  *(&llvm_cbe_retval) = (((((unsigned int )llvm_cbe_tmp2) < ((unsigned int )llvm_cbe_tmp4))) & 1);
#line 230 "stl_function.h"
  llvm_cbe_tmp__210 = ((*(&llvm_cbe_retval))&1);
#line 230 "stl_function.h"
  return llvm_cbe_tmp__210;
}


#line 0 "LLVM INTERNAL"
unsigned int *_ZNKSt10_Select1stISt4pairIKj10SAsnDynstrEEclERKS3_(struct l_class_OC_BaseEvent *llvm_cbe_this, struct l_unnamed78 *llvm_cbe___x) {
  unsigned int *llvm_cbe_retval;    /* Address-exposed local */
  struct l_class_OC_BaseEvent *llvm_cbe_this_2e_addr;    /* Address-exposed local */
  struct l_unnamed78 *llvm_cbe___x_2e_addr;    /* Address-exposed local */
  struct l_class_OC_BaseEvent *llvm_cbe_this1;
  struct l_unnamed78 *llvm_cbe_tmp;
  unsigned int *llvm_cbe_tmp__211;

#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_this_2e_addr) = llvm_cbe_this;
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe___x_2e_addr) = llvm_cbe___x;
#line 0 "LLVM INTERNAL"
  llvm_cbe_this1 = *(&llvm_cbe_this_2e_addr);
#line 489 "stl_function.h"
  llvm_cbe_tmp = *(&llvm_cbe___x_2e_addr);
#line 489 "stl_function.h"
  *(&llvm_cbe_retval) = ((&llvm_cbe_tmp->field0));
#line 489 "stl_function.h"
  llvm_cbe_tmp__211 = *(&llvm_cbe_retval);
#line 489 "stl_function.h"
  return llvm_cbe_tmp__211;
}


#line 0 "LLVM INTERNAL"
unsigned int *_ZNSt8_Rb_treeIjSt4pairIKj10SAsnDynstrESt10_Select1stIS3_ESt4lessIjESaIS3_EE6_S_keyEPKSt13_Rb_tree_nodeIS3_E(struct l_struct_OC_std_KD__KD__Rb_tree_node *llvm_cbe___x) {
  unsigned int *llvm_cbe_retval;    /* Address-exposed local */
  struct l_struct_OC_std_KD__KD__Rb_tree_node *llvm_cbe___x_2e_addr;    /* Address-exposed local */
  struct l_class_OC_BaseEvent llvm_cbe_tmp;    /* Address-exposed local */
  struct l_struct_OC_std_KD__KD__Rb_tree_node *llvm_cbe_tmp1;
  struct l_unnamed78 *llvm_cbe_call;
  unsigned int *llvm_cbe_call2;
  unsigned int *llvm_cbe_tmp__212;

#line 0 "LLVM INTERNAL"
  *(&llvm_cbe___x_2e_addr) = llvm_cbe___x;
#line 505 "stl_tree.h"
  llvm_cbe_tmp1 = *(&llvm_cbe___x_2e_addr);
#line 505 "stl_tree.h"
  llvm_cbe_call = _ZNSt8_Rb_treeIjSt4pairIKj10SAsnDynstrESt10_Select1stIS3_ESt4lessIjESaIS3_EE8_S_valueEPKSt13_Rb_tree_nodeIS3_E(llvm_cbe_tmp1);
#line 505 "stl_tree.h"
  llvm_cbe_call2 = _ZNKSt10_Select1stISt4pairIKj10SAsnDynstrEEclERKS3_((&llvm_cbe_tmp), llvm_cbe_call);
#line 505 "stl_tree.h"
  *(&llvm_cbe_retval) = llvm_cbe_call2;
#line 505 "stl_tree.h"
  llvm_cbe_tmp__212 = *(&llvm_cbe_retval);
#line 505 "stl_tree.h"
  return llvm_cbe_tmp__212;
}


#line 0 "LLVM INTERNAL"
struct l_struct_OC_std_KD__KD__Rb_tree_node *_ZNSt8_Rb_treeIjSt4pairIKj10SAsnDynstrESt10_Select1stIS3_ESt4lessIjESaIS3_EE7_S_leftEPSt18_Rb_tree_node_base(struct l_struct_OC_std_KD__KD__Rb_tree_node_base *llvm_cbe___x) {
  struct l_struct_OC_std_KD__KD__Rb_tree_node *llvm_cbe_retval;    /* Address-exposed local */
  struct l_struct_OC_std_KD__KD__Rb_tree_node_base *llvm_cbe___x_2e_addr;    /* Address-exposed local */
  struct l_struct_OC_std_KD__KD__Rb_tree_node_base *llvm_cbe_tmp;
  struct l_struct_OC_std_KD__KD__Rb_tree_node_base *llvm_cbe_tmp2;
  struct l_struct_OC_std_KD__KD__Rb_tree_node *llvm_cbe_tmp__213;

#line 0 "LLVM INTERNAL"
  *(&llvm_cbe___x_2e_addr) = llvm_cbe___x;
#line 509 "stl_tree.h"
  llvm_cbe_tmp = *(&llvm_cbe___x_2e_addr);
#line 509 "stl_tree.h"
  llvm_cbe_tmp2 = *((&llvm_cbe_tmp->field2));
#line 509 "stl_tree.h"
  *(&llvm_cbe_retval) = (((struct l_struct_OC_std_KD__KD__Rb_tree_node *)llvm_cbe_tmp2));
#line 509 "stl_tree.h"
  llvm_cbe_tmp__213 = *(&llvm_cbe_retval);
#line 509 "stl_tree.h"
  return llvm_cbe_tmp__213;
}


#line 0 "LLVM INTERNAL"
struct l_struct_OC_std_KD__KD__Rb_tree_node *_ZNSt8_Rb_treeIjSt4pairIKj10SAsnDynstrESt10_Select1stIS3_ESt4lessIjESaIS3_EE8_S_rightEPSt18_Rb_tree_node_base(struct l_struct_OC_std_KD__KD__Rb_tree_node_base *llvm_cbe___x) {
  struct l_struct_OC_std_KD__KD__Rb_tree_node *llvm_cbe_retval;    /* Address-exposed local */
  struct l_struct_OC_std_KD__KD__Rb_tree_node_base *llvm_cbe___x_2e_addr;    /* Address-exposed local */
  struct l_struct_OC_std_KD__KD__Rb_tree_node_base *llvm_cbe_tmp;
  struct l_struct_OC_std_KD__KD__Rb_tree_node_base *llvm_cbe_tmp2;
  struct l_struct_OC_std_KD__KD__Rb_tree_node *llvm_cbe_tmp__214;

#line 0 "LLVM INTERNAL"
  *(&llvm_cbe___x_2e_addr) = llvm_cbe___x;
#line 517 "stl_tree.h"
  llvm_cbe_tmp = *(&llvm_cbe___x_2e_addr);
#line 517 "stl_tree.h"
  llvm_cbe_tmp2 = *((&llvm_cbe_tmp->field3));
#line 517 "stl_tree.h"
  *(&llvm_cbe_retval) = (((struct l_struct_OC_std_KD__KD__Rb_tree_node *)llvm_cbe_tmp2));
#line 517 "stl_tree.h"
  llvm_cbe_tmp__214 = *(&llvm_cbe_retval);
#line 517 "stl_tree.h"
  return llvm_cbe_tmp__214;
}


#line 0 "LLVM INTERNAL"
void _ZNSt17_Rb_tree_iteratorISt4pairIKj10SAsnDynstrEEC1EPSt13_Rb_tree_nodeIS3_E(struct l_struct_OC_std_KD__KD__Rb_tree_iterator *llvm_cbe_this, struct l_struct_OC_std_KD__KD__Rb_tree_node *llvm_cbe___x) {
  struct l_struct_OC_std_KD__KD__Rb_tree_iterator *llvm_cbe_this_2e_addr;    /* Address-exposed local */
  struct l_struct_OC_std_KD__KD__Rb_tree_node *llvm_cbe___x_2e_addr;    /* Address-exposed local */
  struct l_struct_OC_std_KD__KD__Rb_tree_iterator *llvm_cbe_this1;
  struct l_struct_OC_std_KD__KD__Rb_tree_node *llvm_cbe_tmp;

#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_this_2e_addr) = llvm_cbe_this;
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe___x_2e_addr) = llvm_cbe___x;
#line 0 "LLVM INTERNAL"
  llvm_cbe_this1 = *(&llvm_cbe_this_2e_addr);
#line 0 "LLVM INTERNAL"
  llvm_cbe_tmp = *(&llvm_cbe___x_2e_addr);
#line 0 "LLVM INTERNAL"
  _ZNSt17_Rb_tree_iteratorISt4pairIKj10SAsnDynstrEEC2EPSt13_Rb_tree_nodeIS3_E(llvm_cbe_this1, llvm_cbe_tmp);
#line 171 "stl_tree.h"
  return;
}


#line 0 "LLVM INTERNAL"
bool _ZNKSt17_Rb_tree_iteratorISt4pairIKj10SAsnDynstrEEeqERKS4_(struct l_struct_OC_std_KD__KD__Rb_tree_iterator *llvm_cbe_this, struct l_struct_OC_std_KD__KD__Rb_tree_iterator *llvm_cbe___x) {
  bool llvm_cbe_retval;    /* Address-exposed local */
  struct l_struct_OC_std_KD__KD__Rb_tree_iterator *llvm_cbe_this_2e_addr;    /* Address-exposed local */
  struct l_struct_OC_std_KD__KD__Rb_tree_iterator *llvm_cbe___x_2e_addr;    /* Address-exposed local */
  struct l_struct_OC_std_KD__KD__Rb_tree_iterator *llvm_cbe_this1;
  struct l_struct_OC_std_KD__KD__Rb_tree_node_base *llvm_cbe_tmp2;
  struct l_struct_OC_std_KD__KD__Rb_tree_iterator *llvm_cbe_tmp3;
  struct l_struct_OC_std_KD__KD__Rb_tree_node_base *llvm_cbe_tmp5;
  bool llvm_cbe_tmp__215;

#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_this_2e_addr) = llvm_cbe_this;
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe___x_2e_addr) = llvm_cbe___x;
#line 0 "LLVM INTERNAL"
  llvm_cbe_this1 = *(&llvm_cbe_this_2e_addr);
#line 213 "stl_tree.h"
  llvm_cbe_tmp2 = *((&llvm_cbe_this1->field0));
#line 213 "stl_tree.h"
  llvm_cbe_tmp3 = *(&llvm_cbe___x_2e_addr);
#line 213 "stl_tree.h"
  llvm_cbe_tmp5 = *((&llvm_cbe_tmp3->field0));
#line 213 "stl_tree.h"
  *(&llvm_cbe_retval) = (((llvm_cbe_tmp2 == llvm_cbe_tmp5)) & 1);
#line 213 "stl_tree.h"
  llvm_cbe_tmp__215 = ((*(&llvm_cbe_retval))&1);
#line 213 "stl_tree.h"
  return llvm_cbe_tmp__215;
}


#line 0 "LLVM INTERNAL"
unsigned long long _ZNSt8_Rb_treeIjSt4pairIKj10SAsnDynstrESt10_Select1stIS3_ESt4lessIjESaIS3_EE5beginEv(struct l_class_OC_std_KD__KD__Rb_tree *llvm_cbe_this) {
  struct l_struct_OC_std_KD__KD__Rb_tree_iterator llvm_cbe_retval;    /* Address-exposed local */
  struct l_class_OC_std_KD__KD__Rb_tree *llvm_cbe_this_2e_addr;    /* Address-exposed local */
  struct l_class_OC_std_KD__KD__Rb_tree *llvm_cbe_this1;
  struct l_struct_OC_std_KD__KD__Rb_tree_node_base *llvm_cbe_tmp4;
  unsigned long long llvm_cbe_tmp__216;

#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_this_2e_addr) = llvm_cbe_this;
#line 0 "LLVM INTERNAL"
  llvm_cbe_this1 = *(&llvm_cbe_this_2e_addr);
#line 627 "stl_tree.h"
  llvm_cbe_tmp4 = *((&((&((&llvm_cbe_this1->field0))->field1))->field2));
#line 627 "stl_tree.h"
  _ZNSt17_Rb_tree_iteratorISt4pairIKj10SAsnDynstrEEC1EPSt13_Rb_tree_nodeIS3_E((&llvm_cbe_retval), (((struct l_struct_OC_std_KD__KD__Rb_tree_node *)llvm_cbe_tmp4)));
#line 629 "stl_tree.h"
  llvm_cbe_tmp__216 = ((struct __attribute__ ((packed, aligned(1))) {unsigned long long data; } *)(((unsigned long long *)(&llvm_cbe_retval))))->data;
#line 629 "stl_tree.h"
  return llvm_cbe_tmp__216;
}


#line 0 "LLVM INTERNAL"
void _ZNSt4pairISt17_Rb_tree_iteratorIS_IKj10SAsnDynstrEEbEC1ERKS4_RKb(struct l_struct_OC_std_KD__KD_pair *llvm_cbe_this, struct l_struct_OC_std_KD__KD__Rb_tree_iterator *llvm_cbe___a, unsigned char *llvm_cbe___b) {
  struct l_struct_OC_std_KD__KD_pair *llvm_cbe_this_2e_addr;    /* Address-exposed local */
  struct l_struct_OC_std_KD__KD__Rb_tree_iterator *llvm_cbe___a_2e_addr;    /* Address-exposed local */
  unsigned char *llvm_cbe___b_2e_addr;    /* Address-exposed local */
  struct l_struct_OC_std_KD__KD_pair *llvm_cbe_this1;
  unsigned char *llvm_cbe_tmp__217;
  struct l_struct_OC_std_KD__KD__Rb_tree_iterator *llvm_cbe_tmp__218;

#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_this_2e_addr) = llvm_cbe_this;
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe___a_2e_addr) = llvm_cbe___a;
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe___b_2e_addr) = llvm_cbe___b;
#line 0 "LLVM INTERNAL"
  llvm_cbe_this1 = *(&llvm_cbe_this_2e_addr);
#line 0 "LLVM INTERNAL"
  llvm_cbe_tmp__217 = *(&llvm_cbe___b_2e_addr);
#line 0 "LLVM INTERNAL"
  llvm_cbe_tmp__218 = *(&llvm_cbe___a_2e_addr);
#line 0 "LLVM INTERNAL"
  _ZNSt4pairISt17_Rb_tree_iteratorIS_IKj10SAsnDynstrEEbEC2ERKS4_RKb(llvm_cbe_this1, llvm_cbe_tmp__218, llvm_cbe_tmp__217);
#line 84 "stl_pair.h"
  return;
}


#line 0 "LLVM INTERNAL"
unsigned long long _ZNSt8_Rb_treeIjSt4pairIKj10SAsnDynstrESt10_Select1stIS3_ESt4lessIjESaIS3_EE10_M_insert_EPKSt18_Rb_tree_node_baseSC_RKS3_(struct l_class_OC_std_KD__KD__Rb_tree *llvm_cbe_this, struct l_struct_OC_std_KD__KD__Rb_tree_node_base *llvm_cbe___x, struct l_struct_OC_std_KD__KD__Rb_tree_node_base *llvm_cbe___y, struct l_unnamed78 *llvm_cbe___v) {
  struct l_struct_OC_std_KD__KD__Rb_tree_iterator llvm_cbe_retval;    /* Address-exposed local */
  struct l_class_OC_std_KD__KD__Rb_tree *llvm_cbe_this_2e_addr;    /* Address-exposed local */
  struct l_struct_OC_std_KD__KD__Rb_tree_node_base *llvm_cbe___x_2e_addr;    /* Address-exposed local */
  struct l_struct_OC_std_KD__KD__Rb_tree_node_base *llvm_cbe___y_2e_addr;    /* Address-exposed local */
  struct l_unnamed78 *llvm_cbe___v_2e_addr;    /* Address-exposed local */
  unsigned char llvm_cbe___insert_left;    /* Address-exposed local */
  struct l_class_OC_BaseEvent llvm_cbe_tmp6;    /* Address-exposed local */
  struct l_struct_OC_std_KD__KD__Rb_tree_node *llvm_cbe___z;    /* Address-exposed local */
  struct l_class_OC_std_KD__KD__Rb_tree *llvm_cbe_this1;
  struct l_struct_OC_std_KD__KD__Rb_tree_node_base *llvm_cbe_tmp;
  struct l_struct_OC_std_KD__KD__Rb_tree_node_base *llvm_cbe_tmp2;
  struct l_struct_OC_std_KD__KD__Rb_tree_node *llvm_cbe_call;
  struct l_unnamed78 *llvm_cbe_tmp7;
  unsigned int *llvm_cbe_call8;
  struct l_struct_OC_std_KD__KD__Rb_tree_node_base *llvm_cbe_tmp9;
  unsigned int *llvm_cbe_call10;
  bool llvm_cbe_call11;
  bool llvm_cbe_tmp__219;
  bool llvm_cbe_tmp__219__PHI_TEMPORARY;
  struct l_unnamed78 *llvm_cbe_tmp13;
  struct l_struct_OC_std_KD__KD__Rb_tree_node *llvm_cbe_call14;
  unsigned char llvm_cbe_tmp15;
  struct l_struct_OC_std_KD__KD__Rb_tree_node *llvm_cbe_tmp16;
  struct l_struct_OC_std_KD__KD__Rb_tree_node_base *llvm_cbe_tmp17;
  unsigned long long *llvm_cbe_tmp21;
  unsigned long long llvm_cbe_tmp22;
  struct l_struct_OC_std_KD__KD__Rb_tree_node *llvm_cbe_tmp23;
  unsigned long long llvm_cbe_tmp__220;

#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_this_2e_addr) = llvm_cbe_this;
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe___x_2e_addr) = llvm_cbe___x;
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe___y_2e_addr) = llvm_cbe___y;
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe___v_2e_addr) = llvm_cbe___v;
#line 0 "LLVM INTERNAL"
  llvm_cbe_this1 = *(&llvm_cbe_this_2e_addr);
#line 879 "stl_tree.h"
  llvm_cbe_tmp = *(&llvm_cbe___x_2e_addr);
#line 879 "stl_tree.h"
  if ((llvm_cbe_tmp != ((struct l_struct_OC_std_KD__KD__Rb_tree_node_base *)/*NULL*/0))) {    llvm_cbe_tmp__219__PHI_TEMPORARY = 1;   /* for PHI node */
    goto llvm_cbe_lor_2e_end;  } else {    goto llvm_cbe_lor_2e_lhs_2e_false;  }


llvm_cbe_lor_2e_lhs_2e_false:
#line 879 "stl_tree.h"
  llvm_cbe_tmp2 = *(&llvm_cbe___y_2e_addr);
#line 879 "stl_tree.h"
  llvm_cbe_call = _ZNSt8_Rb_treeIjSt4pairIKj10SAsnDynstrESt10_Select1stIS3_ESt4lessIjESaIS3_EE6_M_endEv(llvm_cbe_this1);
#line 879 "stl_tree.h"
  if ((llvm_cbe_tmp2 == (((struct l_struct_OC_std_KD__KD__Rb_tree_node_base *)llvm_cbe_call)))) {    llvm_cbe_tmp__219__PHI_TEMPORARY = 1;   /* for PHI node */
    goto llvm_cbe_lor_2e_end;  } else {    goto llvm_cbe_lor_2e_rhs;  }


llvm_cbe_lor_2e_rhs:
#line 879 "stl_tree.h"
  llvm_cbe_tmp7 = *(&llvm_cbe___v_2e_addr);
#line 879 "stl_tree.h"
  llvm_cbe_call8 = _ZNKSt10_Select1stISt4pairIKj10SAsnDynstrEEclERKS3_((&llvm_cbe_tmp6), llvm_cbe_tmp7);
#line 879 "stl_tree.h"
  llvm_cbe_tmp9 = *(&llvm_cbe___y_2e_addr);
#line 879 "stl_tree.h"
  llvm_cbe_call10 = _ZNSt8_Rb_treeIjSt4pairIKj10SAsnDynstrESt10_Select1stIS3_ESt4lessIjESaIS3_EE6_S_keyEPKSt18_Rb_tree_node_base(llvm_cbe_tmp9);
#line 879 "stl_tree.h"
  llvm_cbe_call11 = ((_ZNKSt4lessIjEclERKjS2_(((&((&llvm_cbe_this1->field0))->field0)), llvm_cbe_call8, llvm_cbe_call10))&1);
#line 879 "stl_tree.h"
  llvm_cbe_tmp__219__PHI_TEMPORARY = llvm_cbe_call11;   /* for PHI node */
  goto llvm_cbe_lor_2e_end;

llvm_cbe_lor_2e_end:
#line 0 "LLVM INTERNAL"
  llvm_cbe_tmp__219 = ((llvm_cbe_tmp__219__PHI_TEMPORARY)&1);
#line 879 "stl_tree.h"
  *(&llvm_cbe___insert_left) = (((unsigned char )(bool )llvm_cbe_tmp__219));
#line 881 "stl_tree.h"
  llvm_cbe_tmp13 = *(&llvm_cbe___v_2e_addr);
#line 881 "stl_tree.h"
  llvm_cbe_call14 = _ZNSt8_Rb_treeIjSt4pairIKj10SAsnDynstrESt10_Select1stIS3_ESt4lessIjESaIS3_EE14_M_create_nodeERKS3_(llvm_cbe_this1, llvm_cbe_tmp13);
#line 881 "stl_tree.h"
  *(&llvm_cbe___z) = llvm_cbe_call14;
#line 883 "stl_tree.h"
  llvm_cbe_tmp15 = *(&llvm_cbe___insert_left);
#line 883 "stl_tree.h"
  llvm_cbe_tmp16 = *(&llvm_cbe___z);
#line 883 "stl_tree.h"
  llvm_cbe_tmp17 = *(&llvm_cbe___y_2e_addr);
#line 883 "stl_tree.h"
  _ZSt29_Rb_tree_insert_and_rebalancebPSt18_Rb_tree_node_baseS0_RS_((((((bool )llvm_cbe_tmp15&1u))&1)), (((struct l_struct_OC_std_KD__KD__Rb_tree_node_base *)llvm_cbe_tmp16)), llvm_cbe_tmp17, ((&((&llvm_cbe_this1->field0))->field1)));
#line 886 "stl_tree.h"
  llvm_cbe_tmp21 = (&((&llvm_cbe_this1->field0))->field2);
#line 886 "stl_tree.h"
  llvm_cbe_tmp22 = *llvm_cbe_tmp21;
#line 886 "stl_tree.h"
  *llvm_cbe_tmp21 = (((unsigned long long )(((unsigned long long )llvm_cbe_tmp22) + ((unsigned long long )1ull))));
#line 887 "stl_tree.h"
  llvm_cbe_tmp23 = *(&llvm_cbe___z);
#line 887 "stl_tree.h"
  _ZNSt17_Rb_tree_iteratorISt4pairIKj10SAsnDynstrEEC1EPSt13_Rb_tree_nodeIS3_E((&llvm_cbe_retval), llvm_cbe_tmp23);
#line 888 "stl_tree.h"
  llvm_cbe_tmp__220 = ((struct __attribute__ ((packed, aligned(1))) {unsigned long long data; } *)(((unsigned long long *)(&llvm_cbe_retval))))->data;
#line 888 "stl_tree.h"
  return llvm_cbe_tmp__220;
}


#line 0 "LLVM INTERNAL"
struct l_struct_OC_std_KD__KD__Rb_tree_iterator *_ZNSt17_Rb_tree_iteratorISt4pairIKj10SAsnDynstrEEmmEv(struct l_struct_OC_std_KD__KD__Rb_tree_iterator *llvm_cbe_this) {
  struct l_struct_OC_std_KD__KD__Rb_tree_iterator *llvm_cbe_retval;    /* Address-exposed local */
  struct l_struct_OC_std_KD__KD__Rb_tree_iterator *llvm_cbe_this_2e_addr;    /* Address-exposed local */
  struct l_struct_OC_std_KD__KD__Rb_tree_iterator *llvm_cbe_this1;
  struct l_struct_OC_std_KD__KD__Rb_tree_node_base *llvm_cbe_tmp2;
  struct l_struct_OC_std_KD__KD__Rb_tree_node_base *llvm_cbe_call;
  struct l_struct_OC_std_KD__KD__Rb_tree_iterator *llvm_cbe_tmp__221;

#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_this_2e_addr) = llvm_cbe_this;
#line 0 "LLVM INTERNAL"
  llvm_cbe_this1 = *(&llvm_cbe_this_2e_addr);
#line 199 "stl_tree.h"
  llvm_cbe_tmp2 = *((&llvm_cbe_this1->field0));
#line 199 "stl_tree.h"
  llvm_cbe_call = _ZSt18_Rb_tree_decrementPSt18_Rb_tree_node_base(llvm_cbe_tmp2);
#line 199 "stl_tree.h"
  *((&llvm_cbe_this1->field0)) = llvm_cbe_call;
#line 200 "stl_tree.h"
  *(&llvm_cbe_retval) = llvm_cbe_this1;
#line 201 "stl_tree.h"
  llvm_cbe_tmp__221 = *(&llvm_cbe_retval);
#line 201 "stl_tree.h"
  return llvm_cbe_tmp__221;
}


#line 0 "LLVM INTERNAL"
unsigned int *_ZNSt8_Rb_treeIjSt4pairIKj10SAsnDynstrESt10_Select1stIS3_ESt4lessIjESaIS3_EE6_S_keyEPKSt18_Rb_tree_node_base(struct l_struct_OC_std_KD__KD__Rb_tree_node_base *llvm_cbe___x) {
  unsigned int *llvm_cbe_retval;    /* Address-exposed local */
  struct l_struct_OC_std_KD__KD__Rb_tree_node_base *llvm_cbe___x_2e_addr;    /* Address-exposed local */
  struct l_class_OC_BaseEvent llvm_cbe_tmp;    /* Address-exposed local */
  struct l_struct_OC_std_KD__KD__Rb_tree_node_base *llvm_cbe_tmp1;
  struct l_unnamed78 *llvm_cbe_call;
  unsigned int *llvm_cbe_call2;
  unsigned int *llvm_cbe_tmp__222;

#line 0 "LLVM INTERNAL"
  *(&llvm_cbe___x_2e_addr) = llvm_cbe___x;
#line 529 "stl_tree.h"
  llvm_cbe_tmp1 = *(&llvm_cbe___x_2e_addr);
#line 529 "stl_tree.h"
  llvm_cbe_call = _ZNSt8_Rb_treeIjSt4pairIKj10SAsnDynstrESt10_Select1stIS3_ESt4lessIjESaIS3_EE8_S_valueEPKSt18_Rb_tree_node_base(llvm_cbe_tmp1);
#line 529 "stl_tree.h"
  llvm_cbe_call2 = _ZNKSt10_Select1stISt4pairIKj10SAsnDynstrEEclERKS3_((&llvm_cbe_tmp), llvm_cbe_call);
#line 529 "stl_tree.h"
  *(&llvm_cbe_retval) = llvm_cbe_call2;
#line 529 "stl_tree.h"
  llvm_cbe_tmp__222 = *(&llvm_cbe_retval);
#line 529 "stl_tree.h"
  return llvm_cbe_tmp__222;
}


#line 0 "LLVM INTERNAL"
struct l_unnamed78 *_ZNSt8_Rb_treeIjSt4pairIKj10SAsnDynstrESt10_Select1stIS3_ESt4lessIjESaIS3_EE8_S_valueEPKSt18_Rb_tree_node_base(struct l_struct_OC_std_KD__KD__Rb_tree_node_base *llvm_cbe___x) {
  struct l_unnamed78 *llvm_cbe_retval;    /* Address-exposed local */
  struct l_struct_OC_std_KD__KD__Rb_tree_node_base *llvm_cbe___x_2e_addr;    /* Address-exposed local */
  struct l_struct_OC_std_KD__KD__Rb_tree_node_base *llvm_cbe_tmp;
  struct l_unnamed78 *llvm_cbe_tmp__223;

#line 0 "LLVM INTERNAL"
  *(&llvm_cbe___x_2e_addr) = llvm_cbe___x;
#line 525 "stl_tree.h"
  llvm_cbe_tmp = *(&llvm_cbe___x_2e_addr);
#line 525 "stl_tree.h"
  *(&llvm_cbe_retval) = ((&(((struct l_struct_OC_std_KD__KD__Rb_tree_node *)llvm_cbe_tmp))->field1));
#line 525 "stl_tree.h"
  llvm_cbe_tmp__223 = *(&llvm_cbe_retval);
#line 525 "stl_tree.h"
  return llvm_cbe_tmp__223;
}


struct l_struct_OC_std_KD__KD__Rb_tree_node *_ZNSt8_Rb_treeIjSt4pairIKj10SAsnDynstrESt10_Select1stIS3_ESt4lessIjESaIS3_EE11_M_get_nodeEv(struct l_class_OC_std_KD__KD__Rb_tree *llvm_cbe_this);

#line 0 "LLVM INTERNAL"
struct l_struct_OC_std_KD__KD__Rb_tree_node *_ZNSt8_Rb_treeIjSt4pairIKj10SAsnDynstrESt10_Select1stIS3_ESt4lessIjESaIS3_EE14_M_create_nodeERKS3_(struct l_class_OC_std_KD__KD__Rb_tree *llvm_cbe_this, struct l_unnamed78 *llvm_cbe___x) {
  struct l_struct_OC_std_KD__KD__Rb_tree_node *llvm_cbe_retval;    /* Address-exposed local */
  struct l_class_OC_std_KD__KD__Rb_tree *llvm_cbe_this_2e_addr;    /* Address-exposed local */
  struct l_unnamed78 *llvm_cbe___x_2e_addr;    /* Address-exposed local */
  struct l_struct_OC_std_KD__KD__Rb_tree_node *llvm_cbe___tmp;    /* Address-exposed local */
  struct l_class_OC_BaseEvent llvm_cbe_tmp;    /* Address-exposed local */
  unsigned char *llvm_cbe__rethrow;    /* Address-exposed local */
  unsigned int llvm_cbe_cleanup_2e_dst;    /* Address-exposed local */
  unsigned int llvm_cbe_cleanup_2e_dst18;    /* Address-exposed local */
  struct l_class_OC_std_KD__KD__Rb_tree *llvm_cbe_this1;
  struct l_struct_OC_std_KD__KD__Rb_tree_node *llvm_cbe_call;
  struct l_struct_OC_std_KD__KD__Rb_tree_node *llvm_cbe_tmp5;
  struct l_unnamed78 *llvm_cbe_tmp7;
  unsigned int llvm_cbe_tmp19;
  unsigned char *llvm_cbe_tmp__224;
  struct l_struct_OC_std_KD__KD__Rb_tree_node *llvm_cbe_tmp22;
  struct l_struct_OC_std_KD__KD__Rb_tree_node *llvm_cbe_tmp__225;

#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_this_2e_addr) = llvm_cbe_this;
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe___x_2e_addr) = llvm_cbe___x;
#line 0 "LLVM INTERNAL"
  llvm_cbe_this1 = *(&llvm_cbe_this_2e_addr);
#line 369 "stl_tree.h"
  llvm_cbe_call = _ZNSt8_Rb_treeIjSt4pairIKj10SAsnDynstrESt10_Select1stIS3_ESt4lessIjESaIS3_EE11_M_get_nodeEv(llvm_cbe_this1);
#line 369 "stl_tree.h"
  *(&llvm_cbe___tmp) = llvm_cbe_call;
#line 371 "stl_tree.h"
  llvm_cbe_tmp = _ZNKSt8_Rb_treeIjSt4pairIKj10SAsnDynstrESt10_Select1stIS3_ESt4lessIjESaIS3_EE13get_allocatorEv(llvm_cbe_this1);
#line 371 "stl_tree.h"
  llvm_cbe_tmp5 = *(&llvm_cbe___tmp);
#line 371 "stl_tree.h"
  llvm_cbe_tmp7 = *(&llvm_cbe___x_2e_addr);
#line 371 "stl_tree.h"
  _ZN9__gnu_cxx13new_allocatorISt4pairIKj10SAsnDynstrEE9constructEPS4_RKS4_((&llvm_cbe_tmp), ((&llvm_cbe_tmp5->field1)), llvm_cbe_tmp7);
#line 371 "stl_tree.h"
  _ZNSaISt4pairIKj10SAsnDynstrEED1Ev((&llvm_cbe_tmp));
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_cleanup_2e_dst18) = 1u;
#line 376 "stl_tree.h"
  llvm_cbe_tmp19 = *(&llvm_cbe_cleanup_2e_dst18);
#line 0 "LLVM INTERNAL"
  if ((llvm_cbe_tmp19 == 2u)) {    goto llvm_cbe_finally_2e_throw;  } else {    goto llvm_cbe_finally_2e_end;  }


llvm_cbe_finally_2e_throw:
#line 376 "stl_tree.h"
  llvm_cbe_tmp__224 = *(&llvm_cbe__rethrow);
#line 376 "stl_tree.h"
  _Unwind_Resume_or_Rethrow(llvm_cbe_tmp__224);
#line 376 "stl_tree.h"
  /*UNREACHABLE*/;

llvm_cbe_finally_2e_end:
#line 377 "stl_tree.h"
  llvm_cbe_tmp22 = *(&llvm_cbe___tmp);
#line 377 "stl_tree.h"
  *(&llvm_cbe_retval) = llvm_cbe_tmp22;
#line 378 "stl_tree.h"
  llvm_cbe_tmp__225 = *(&llvm_cbe_retval);
#line 378 "stl_tree.h"
  return llvm_cbe_tmp__225;
}


#line 0 "LLVM INTERNAL"
struct l_struct_OC_std_KD__KD__Rb_tree_node *_ZNSt8_Rb_treeIjSt4pairIKj10SAsnDynstrESt10_Select1stIS3_ESt4lessIjESaIS3_EE11_M_get_nodeEv(struct l_class_OC_std_KD__KD__Rb_tree *llvm_cbe_this) {
  struct l_struct_OC_std_KD__KD__Rb_tree_node *llvm_cbe_retval;    /* Address-exposed local */
  struct l_class_OC_std_KD__KD__Rb_tree *llvm_cbe_this_2e_addr;    /* Address-exposed local */
  struct l_class_OC_std_KD__KD__Rb_tree *llvm_cbe_this1;
  struct l_struct_OC_std_KD__KD__Rb_tree_node *llvm_cbe_call;
  struct l_struct_OC_std_KD__KD__Rb_tree_node *llvm_cbe_tmp__226;

#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_this_2e_addr) = llvm_cbe_this;
#line 0 "LLVM INTERNAL"
  llvm_cbe_this1 = *(&llvm_cbe_this_2e_addr);
#line 359 "stl_tree.h"
  llvm_cbe_call = _ZN9__gnu_cxx13new_allocatorISt13_Rb_tree_nodeISt4pairIKj10SAsnDynstrEEE8allocateEmPKv((((struct l_class_OC_BaseEvent *)((&llvm_cbe_this1->field0)))), 1ull, ((unsigned char *)/*NULL*/0));
#line 359 "stl_tree.h"
  *(&llvm_cbe_retval) = llvm_cbe_call;
#line 359 "stl_tree.h"
  llvm_cbe_tmp__226 = *(&llvm_cbe_retval);
#line 359 "stl_tree.h"
  return llvm_cbe_tmp__226;
}


#line 0 "LLVM INTERNAL"
struct l_class_OC_BaseEvent _ZNKSt8_Rb_treeIjSt4pairIKj10SAsnDynstrESt10_Select1stIS3_ESt4lessIjESaIS3_EE13get_allocatorEv(struct l_class_OC_std_KD__KD__Rb_tree *llvm_cbe_this) {
  struct l_class_OC_BaseEvent StructReturn;  /* Struct return temporary */
  struct l_class_OC_BaseEvent *llvm_cbe_agg_2e_result = &StructReturn;
  struct l_class_OC_std_KD__KD__Rb_tree *llvm_cbe_this_2e_addr;    /* Address-exposed local */
  struct l_class_OC_std_KD__KD__Rb_tree *llvm_cbe_this1;
  struct l_class_OC_BaseEvent *llvm_cbe_call;

#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_this_2e_addr) = llvm_cbe_this;
#line 0 "LLVM INTERNAL"
  llvm_cbe_this1 = *(&llvm_cbe_this_2e_addr);
#line 354 "stl_tree.h"
  llvm_cbe_call = _ZNKSt8_Rb_treeIjSt4pairIKj10SAsnDynstrESt10_Select1stIS3_ESt4lessIjESaIS3_EE21_M_get_Node_allocatorEv(llvm_cbe_this1);
#line 354 "stl_tree.h"
  _ZNSaISt4pairIKj10SAsnDynstrEEC1ISt13_Rb_tree_nodeIS2_EEERKSaIT_E(llvm_cbe_agg_2e_result, llvm_cbe_call);
#line 354 "stl_tree.h"
  return StructReturn;
}


#line 0 "LLVM INTERNAL"
void _ZNSaISt4pairIKj10SAsnDynstrEED1Ev(struct l_class_OC_BaseEvent *llvm_cbe_this) {
  unsigned char *llvm_cbe__rethrow;    /* Address-exposed local */
  struct l_class_OC_BaseEvent *llvm_cbe_this_2e_addr;    /* Address-exposed local */
  struct l_class_OC_BaseEvent *llvm_cbe_this1;

#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_this_2e_addr) = llvm_cbe_this;
#line 0 "LLVM INTERNAL"
  llvm_cbe_this1 = *(&llvm_cbe_this_2e_addr);
#line 0 "LLVM INTERNAL"
  _ZNSaISt4pairIKj10SAsnDynstrEED2Ev(llvm_cbe_this1);
#line 109 "allocator.h"
  return;
}


#line 0 "LLVM INTERNAL"
void _ZN9__gnu_cxx13new_allocatorISt4pairIKj10SAsnDynstrEE9constructEPS4_RKS4_(struct l_class_OC_BaseEvent *llvm_cbe_this, struct l_unnamed78 *llvm_cbe___p, struct l_unnamed78 *llvm_cbe___val) {
  struct l_class_OC_BaseEvent *llvm_cbe_this_2e_addr;    /* Address-exposed local */
  struct l_unnamed78 *llvm_cbe___p_2e_addr;    /* Address-exposed local */
  struct l_unnamed78 *llvm_cbe___val_2e_addr;    /* Address-exposed local */
  struct l_class_OC_BaseEvent *llvm_cbe_this1;
  struct l_unnamed78 *llvm_cbe_tmp;
  unsigned char *llvm_cbe_call;
  struct l_unnamed78 *llvm_cbe_tmp2;
  unsigned char *llvm_cbe_tmp__227;

#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_this_2e_addr) = llvm_cbe_this;
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe___p_2e_addr) = llvm_cbe___p;
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe___val_2e_addr) = llvm_cbe___val;
#line 0 "LLVM INTERNAL"
  llvm_cbe_this1 = *(&llvm_cbe_this_2e_addr);
#line 105 "new_allocator.h"
  llvm_cbe_tmp = *(&llvm_cbe___p_2e_addr);
#line 105 "new_allocator.h"
  llvm_cbe_call = _ZnwmPv(24ull, (((unsigned char *)llvm_cbe_tmp)));
#line 105 "new_allocator.h"
  if ((llvm_cbe_call == ((unsigned char *)/*NULL*/0))) {    goto llvm_cbe_new_2e_end;  } else {    goto llvm_cbe_new_2e_notnull;  }


llvm_cbe_new_2e_notnull:
#line 105 "new_allocator.h"
  llvm_cbe_tmp2 = *(&llvm_cbe___val_2e_addr);
#line 0 "LLVM INTERNAL"
  llvm_cbe_tmp__227 = memcpy((((unsigned char *)(((struct l_unnamed78 *)llvm_cbe_call)))), (((unsigned char *)llvm_cbe_tmp2)), 24ull);
#line 105 "new_allocator.h"
  return;

llvm_cbe_new_2e_end:
#line 105 "new_allocator.h"
  return;
}


#line 0 "LLVM INTERNAL"
void _ZNSt8_Rb_treeIjSt4pairIKj10SAsnDynstrESt10_Select1stIS3_ESt4lessIjESaIS3_EE11_M_put_nodeEPSt13_Rb_tree_nodeIS3_E(struct l_class_OC_std_KD__KD__Rb_tree *llvm_cbe_this, struct l_struct_OC_std_KD__KD__Rb_tree_node *llvm_cbe___p) {
  struct l_class_OC_std_KD__KD__Rb_tree *llvm_cbe_this_2e_addr;    /* Address-exposed local */
  struct l_struct_OC_std_KD__KD__Rb_tree_node *llvm_cbe___p_2e_addr;    /* Address-exposed local */
  struct l_class_OC_std_KD__KD__Rb_tree *llvm_cbe_this1;
  struct l_struct_OC_std_KD__KD__Rb_tree_node *llvm_cbe_tmp2;

#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_this_2e_addr) = llvm_cbe_this;
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe___p_2e_addr) = llvm_cbe___p;
#line 0 "LLVM INTERNAL"
  llvm_cbe_this1 = *(&llvm_cbe_this_2e_addr);
#line 363 "stl_tree.h"
  llvm_cbe_tmp2 = *(&llvm_cbe___p_2e_addr);
#line 363 "stl_tree.h"
  _ZN9__gnu_cxx13new_allocatorISt13_Rb_tree_nodeISt4pairIKj10SAsnDynstrEEE10deallocateEPS6_m((((struct l_class_OC_BaseEvent *)((&llvm_cbe_this1->field0)))), llvm_cbe_tmp2, 1ull);
#line 363 "stl_tree.h"
  return;
}


#line 0 "LLVM INTERNAL"
void _ZN9__gnu_cxx13new_allocatorISt13_Rb_tree_nodeISt4pairIKj10SAsnDynstrEEE10deallocateEPS6_m(struct l_class_OC_BaseEvent *llvm_cbe_this, struct l_struct_OC_std_KD__KD__Rb_tree_node *llvm_cbe___p, unsigned long long llvm_cbe_tmp__228) {
  struct l_class_OC_BaseEvent *llvm_cbe_this_2e_addr;    /* Address-exposed local */
  struct l_struct_OC_std_KD__KD__Rb_tree_node *llvm_cbe___p_2e_addr;    /* Address-exposed local */
  unsigned long long llvm_cbe__2e_addr;    /* Address-exposed local */
  struct l_class_OC_BaseEvent *llvm_cbe_this1;
  struct l_struct_OC_std_KD__KD__Rb_tree_node *llvm_cbe_tmp;

#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_this_2e_addr) = llvm_cbe_this;
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe___p_2e_addr) = llvm_cbe___p;
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe__2e_addr) = llvm_cbe_tmp__228;
#line 0 "LLVM INTERNAL"
  llvm_cbe_this1 = *(&llvm_cbe_this_2e_addr);
#line 95 "new_allocator.h"
  llvm_cbe_tmp = *(&llvm_cbe___p_2e_addr);
#line 95 "new_allocator.h"
  _ZdlPv((((unsigned char *)llvm_cbe_tmp)));
#line 95 "new_allocator.h"
  return;
}


#line 0 "LLVM INTERNAL"
unsigned char *_ZnwmPv(unsigned long long llvm_cbe_tmp__229, unsigned char *llvm_cbe___p) {
  unsigned char *llvm_cbe_retval;    /* Address-exposed local */
  unsigned char *llvm_cbe__rethrow;    /* Address-exposed local */
  unsigned long long llvm_cbe__2e_addr;    /* Address-exposed local */
  unsigned char *llvm_cbe___p_2e_addr;    /* Address-exposed local */
  unsigned char *llvm_cbe_tmp;
  unsigned char *llvm_cbe_tmp__230;

#line 0 "LLVM INTERNAL"
  *(&llvm_cbe__2e_addr) = llvm_cbe_tmp__229;
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe___p_2e_addr) = llvm_cbe___p;
#line 101 "new"
  llvm_cbe_tmp = *(&llvm_cbe___p_2e_addr);
#line 101 "new"
  *(&llvm_cbe_retval) = llvm_cbe_tmp;
#line 101 "new"
  llvm_cbe_tmp__230 = *(&llvm_cbe_retval);
#line 101 "new"
  return llvm_cbe_tmp__230;
}


#line 0 "LLVM INTERNAL"
void _ZNSaISt4pairIKj10SAsnDynstrEED2Ev(struct l_class_OC_BaseEvent *llvm_cbe_this) {
  unsigned char *llvm_cbe__rethrow;    /* Address-exposed local */
  struct l_class_OC_BaseEvent *llvm_cbe_this_2e_addr;    /* Address-exposed local */
  struct l_class_OC_BaseEvent *llvm_cbe_this1;

#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_this_2e_addr) = llvm_cbe_this;
#line 0 "LLVM INTERNAL"
  llvm_cbe_this1 = *(&llvm_cbe_this_2e_addr);
#line 109 "allocator.h"
  _ZN9__gnu_cxx13new_allocatorISt4pairIKj10SAsnDynstrEED2Ev(llvm_cbe_this1);
#line 109 "allocator.h"
  return;
}


#line 0 "LLVM INTERNAL"
void _ZN9__gnu_cxx13new_allocatorISt4pairIKj10SAsnDynstrEED2Ev(struct l_class_OC_BaseEvent *llvm_cbe_this) {
  unsigned char *llvm_cbe__rethrow;    /* Address-exposed local */
  struct l_class_OC_BaseEvent *llvm_cbe_this_2e_addr;    /* Address-exposed local */
  struct l_class_OC_BaseEvent *llvm_cbe_this1;

#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_this_2e_addr) = llvm_cbe_this;
#line 0 "LLVM INTERNAL"
  llvm_cbe_this1 = *(&llvm_cbe_this_2e_addr);
#line 73 "new_allocator.h"
  return;
}


#line 0 "LLVM INTERNAL"
void _ZNSaISt4pairIKj10SAsnDynstrEEC1ISt13_Rb_tree_nodeIS2_EEERKSaIT_E(struct l_class_OC_BaseEvent *llvm_cbe_this, struct l_class_OC_BaseEvent *llvm_cbe_tmp__231) {
  unsigned char *llvm_cbe__rethrow;    /* Address-exposed local */
  struct l_class_OC_BaseEvent *llvm_cbe_this_2e_addr;    /* Address-exposed local */
  struct l_class_OC_BaseEvent *llvm_cbe__2e_addr;    /* Address-exposed local */
  struct l_class_OC_BaseEvent *llvm_cbe_this1;
  struct l_class_OC_BaseEvent *llvm_cbe_tmp__232;

#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_this_2e_addr) = llvm_cbe_this;
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe__2e_addr) = llvm_cbe_tmp__231;
#line 0 "LLVM INTERNAL"
  llvm_cbe_this1 = *(&llvm_cbe_this_2e_addr);
#line 0 "LLVM INTERNAL"
  llvm_cbe_tmp__232 = *(&llvm_cbe__2e_addr);
#line 0 "LLVM INTERNAL"
  _ZNSaISt4pairIKj10SAsnDynstrEEC2ISt13_Rb_tree_nodeIS2_EEERKSaIT_E(llvm_cbe_this1, llvm_cbe_tmp__232);
#line 107 "allocator.h"
  return;
}


#line 0 "LLVM INTERNAL"
struct l_class_OC_BaseEvent *_ZNKSt8_Rb_treeIjSt4pairIKj10SAsnDynstrESt10_Select1stIS3_ESt4lessIjESaIS3_EE21_M_get_Node_allocatorEv(struct l_class_OC_std_KD__KD__Rb_tree *llvm_cbe_this) {
  struct l_class_OC_BaseEvent *llvm_cbe_retval;    /* Address-exposed local */
  struct l_class_OC_std_KD__KD__Rb_tree *llvm_cbe_this_2e_addr;    /* Address-exposed local */
  struct l_class_OC_std_KD__KD__Rb_tree *llvm_cbe_this1;
  struct l_class_OC_BaseEvent *llvm_cbe_tmp__233;

#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_this_2e_addr) = llvm_cbe_this;
#line 0 "LLVM INTERNAL"
  llvm_cbe_this1 = *(&llvm_cbe_this_2e_addr);
#line 350 "stl_tree.h"
  *(&llvm_cbe_retval) = (((struct l_class_OC_BaseEvent *)((&llvm_cbe_this1->field0))));
#line 350 "stl_tree.h"
  llvm_cbe_tmp__233 = *(&llvm_cbe_retval);
#line 350 "stl_tree.h"
  return llvm_cbe_tmp__233;
}


#line 0 "LLVM INTERNAL"
void _ZNSaISt4pairIKj10SAsnDynstrEEC2ISt13_Rb_tree_nodeIS2_EEERKSaIT_E(struct l_class_OC_BaseEvent *llvm_cbe_this, struct l_class_OC_BaseEvent *llvm_cbe_tmp__234) {
  unsigned char *llvm_cbe__rethrow;    /* Address-exposed local */
  struct l_class_OC_BaseEvent *llvm_cbe_this_2e_addr;    /* Address-exposed local */
  struct l_class_OC_BaseEvent *llvm_cbe__2e_addr;    /* Address-exposed local */
  struct l_class_OC_BaseEvent *llvm_cbe_this1;

#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_this_2e_addr) = llvm_cbe_this;
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe__2e_addr) = llvm_cbe_tmp__234;
#line 0 "LLVM INTERNAL"
  llvm_cbe_this1 = *(&llvm_cbe_this_2e_addr);
#line 0 "LLVM INTERNAL"
  _ZN9__gnu_cxx13new_allocatorISt4pairIKj10SAsnDynstrEEC2Ev(llvm_cbe_this1);
#line 107 "allocator.h"
  return;
}


#line 0 "LLVM INTERNAL"
void _ZN9__gnu_cxx13new_allocatorISt4pairIKj10SAsnDynstrEEC2Ev(struct l_class_OC_BaseEvent *llvm_cbe_this) {
  unsigned char *llvm_cbe__rethrow;    /* Address-exposed local */
  struct l_class_OC_BaseEvent *llvm_cbe_this_2e_addr;    /* Address-exposed local */
  struct l_class_OC_BaseEvent *llvm_cbe_this1;

#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_this_2e_addr) = llvm_cbe_this;
#line 0 "LLVM INTERNAL"
  llvm_cbe_this1 = *(&llvm_cbe_this_2e_addr);
#line 66 "new_allocator.h"
  return;
}


#line 0 "LLVM INTERNAL"
struct l_struct_OC_std_KD__KD__Rb_tree_node *_ZN9__gnu_cxx13new_allocatorISt13_Rb_tree_nodeISt4pairIKj10SAsnDynstrEEE8allocateEmPKv(struct l_class_OC_BaseEvent *llvm_cbe_this, unsigned long long llvm_cbe___n, unsigned char *llvm_cbe_tmp__235) {
  struct l_struct_OC_std_KD__KD__Rb_tree_node *llvm_cbe_retval;    /* Address-exposed local */
  struct l_class_OC_BaseEvent *llvm_cbe_this_2e_addr;    /* Address-exposed local */
  unsigned long long llvm_cbe___n_2e_addr;    /* Address-exposed local */
  unsigned char *llvm_cbe__2e_addr;    /* Address-exposed local */
  struct l_class_OC_BaseEvent *llvm_cbe_this1;
  unsigned long long llvm_cbe_tmp;
  unsigned long long llvm_cbe_call;
  unsigned long long llvm_cbe_tmp2;
  unsigned char *llvm_cbe_call3;
  struct l_struct_OC_std_KD__KD__Rb_tree_node *llvm_cbe_tmp__236;

#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_this_2e_addr) = llvm_cbe_this;
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe___n_2e_addr) = llvm_cbe___n;
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe__2e_addr) = llvm_cbe_tmp__235;
#line 0 "LLVM INTERNAL"
  llvm_cbe_this1 = *(&llvm_cbe_this_2e_addr);
#line 86 "new_allocator.h"
  llvm_cbe_tmp = *(&llvm_cbe___n_2e_addr);
#line 86 "new_allocator.h"
  llvm_cbe_call = _ZNK9__gnu_cxx13new_allocatorISt13_Rb_tree_nodeISt4pairIKj10SAsnDynstrEEE8max_sizeEv(llvm_cbe_this1);
#line 86 "new_allocator.h"
  if ((((unsigned long long )llvm_cbe_tmp) > ((unsigned long long )llvm_cbe_call))) {    goto llvm_cbe_if_2e_then;  } else {    goto llvm_cbe_if_2e_end;  }


llvm_cbe_if_2e_then:
#line 87 "new_allocator.h"
  _ZSt17__throw_bad_allocv();
#line 87 "new_allocator.h"
  /*UNREACHABLE*/;

llvm_cbe_if_2e_end:
#line 89 "new_allocator.h"
  llvm_cbe_tmp2 = *(&llvm_cbe___n_2e_addr);
#line 89 "new_allocator.h"
  llvm_cbe_call3 = _Znwm((((unsigned long long )(((unsigned long long )llvm_cbe_tmp2) * ((unsigned long long )56ull)))));
#line 89 "new_allocator.h"
  *(&llvm_cbe_retval) = (((struct l_struct_OC_std_KD__KD__Rb_tree_node *)llvm_cbe_call3));
#line 90 "new_allocator.h"
  llvm_cbe_tmp__236 = *(&llvm_cbe_retval);
#line 90 "new_allocator.h"
  return llvm_cbe_tmp__236;
}


#line 0 "LLVM INTERNAL"
unsigned long long _ZNK9__gnu_cxx13new_allocatorISt13_Rb_tree_nodeISt4pairIKj10SAsnDynstrEEE8max_sizeEv(struct l_class_OC_BaseEvent *llvm_cbe_this) {
  unsigned long long llvm_cbe_retval;    /* Address-exposed local */
  unsigned char *llvm_cbe__rethrow;    /* Address-exposed local */
  struct l_class_OC_BaseEvent *llvm_cbe_this_2e_addr;    /* Address-exposed local */
  struct l_class_OC_BaseEvent *llvm_cbe_this1;
  unsigned long long llvm_cbe_tmp__237;

#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_this_2e_addr) = llvm_cbe_this;
#line 0 "LLVM INTERNAL"
  llvm_cbe_this1 = *(&llvm_cbe_this_2e_addr);
#line 99 "new_allocator.h"
  *(&llvm_cbe_retval) = 329406144173384850ull;
#line 99 "new_allocator.h"
  llvm_cbe_tmp__237 = *(&llvm_cbe_retval);
#line 99 "new_allocator.h"
  return llvm_cbe_tmp__237;
}


#line 0 "LLVM INTERNAL"
void _ZNSt4pairISt17_Rb_tree_iteratorIS_IKj10SAsnDynstrEEbEC2ERKS4_RKb(struct l_struct_OC_std_KD__KD_pair *llvm_cbe_this, struct l_struct_OC_std_KD__KD__Rb_tree_iterator *llvm_cbe___a, unsigned char *llvm_cbe___b) {
  struct l_struct_OC_std_KD__KD_pair *llvm_cbe_this_2e_addr;    /* Address-exposed local */
  struct l_struct_OC_std_KD__KD__Rb_tree_iterator *llvm_cbe___a_2e_addr;    /* Address-exposed local */
  unsigned char *llvm_cbe___b_2e_addr;    /* Address-exposed local */
  struct l_struct_OC_std_KD__KD_pair *llvm_cbe_this1;
  struct l_struct_OC_std_KD__KD__Rb_tree_iterator *llvm_cbe_tmp2;
  unsigned char *llvm_cbe_tmp__238;
  unsigned char *llvm_cbe_tmp6;
  unsigned char llvm_cbe_tmp7;

#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_this_2e_addr) = llvm_cbe_this;
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe___a_2e_addr) = llvm_cbe___a;
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe___b_2e_addr) = llvm_cbe___b;
#line 0 "LLVM INTERNAL"
  llvm_cbe_this1 = *(&llvm_cbe_this_2e_addr);
#line 0 "LLVM INTERNAL"
  llvm_cbe_tmp2 = *(&llvm_cbe___a_2e_addr);
#line 0 "LLVM INTERNAL"
  llvm_cbe_tmp__238 = memcpy((((unsigned char *)((&llvm_cbe_this1->field0)))), (((unsigned char *)llvm_cbe_tmp2)), 8ull);
#line 0 "LLVM INTERNAL"
  llvm_cbe_tmp6 = *(&llvm_cbe___b_2e_addr);
#line 0 "LLVM INTERNAL"
  llvm_cbe_tmp7 = *llvm_cbe_tmp6;
#line 0 "LLVM INTERNAL"
  *((&llvm_cbe_this1->field1)) = (((unsigned char )(bool )(((((bool )llvm_cbe_tmp7&1u))&1))));
#line 84 "stl_pair.h"
  return;
}


#line 0 "LLVM INTERNAL"
void _ZNSt17_Rb_tree_iteratorISt4pairIKj10SAsnDynstrEEC2EPSt13_Rb_tree_nodeIS3_E(struct l_struct_OC_std_KD__KD__Rb_tree_iterator *llvm_cbe_this, struct l_struct_OC_std_KD__KD__Rb_tree_node *llvm_cbe___x) {
  struct l_struct_OC_std_KD__KD__Rb_tree_iterator *llvm_cbe_this_2e_addr;    /* Address-exposed local */
  struct l_struct_OC_std_KD__KD__Rb_tree_node *llvm_cbe___x_2e_addr;    /* Address-exposed local */
  struct l_struct_OC_std_KD__KD__Rb_tree_iterator *llvm_cbe_this1;
  struct l_struct_OC_std_KD__KD__Rb_tree_node *llvm_cbe_tmp2;

#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_this_2e_addr) = llvm_cbe_this;
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe___x_2e_addr) = llvm_cbe___x;
#line 0 "LLVM INTERNAL"
  llvm_cbe_this1 = *(&llvm_cbe_this_2e_addr);
#line 0 "LLVM INTERNAL"
  llvm_cbe_tmp2 = *(&llvm_cbe___x_2e_addr);
#line 0 "LLVM INTERNAL"
  *((&llvm_cbe_this1->field0)) = (((struct l_struct_OC_std_KD__KD__Rb_tree_node_base *)llvm_cbe_tmp2));
#line 171 "stl_tree.h"
  return;
}


#line 0 "LLVM INTERNAL"
struct l_unnamed78 *_ZNSt8_Rb_treeIjSt4pairIKj10SAsnDynstrESt10_Select1stIS3_ESt4lessIjESaIS3_EE8_S_valueEPKSt13_Rb_tree_nodeIS3_E(struct l_struct_OC_std_KD__KD__Rb_tree_node *llvm_cbe___x) {
  struct l_unnamed78 *llvm_cbe_retval;    /* Address-exposed local */
  struct l_struct_OC_std_KD__KD__Rb_tree_node *llvm_cbe___x_2e_addr;    /* Address-exposed local */
  struct l_struct_OC_std_KD__KD__Rb_tree_node *llvm_cbe_tmp;
  struct l_unnamed78 *llvm_cbe_tmp__239;

#line 0 "LLVM INTERNAL"
  *(&llvm_cbe___x_2e_addr) = llvm_cbe___x;
#line 501 "stl_tree.h"
  llvm_cbe_tmp = *(&llvm_cbe___x_2e_addr);
#line 501 "stl_tree.h"
  *(&llvm_cbe_retval) = ((&llvm_cbe_tmp->field1));
#line 501 "stl_tree.h"
  llvm_cbe_tmp__239 = *(&llvm_cbe_retval);
#line 501 "stl_tree.h"
  return llvm_cbe_tmp__239;
}


#line 0 "LLVM INTERNAL"
void _ZNSt4pairISt17_Rb_tree_iteratorIS_IKj10SAsnDynstrEEbEC2Ev(struct l_struct_OC_std_KD__KD_pair *llvm_cbe_this) {
  struct l_struct_OC_std_KD__KD_pair *llvm_cbe_this_2e_addr;    /* Address-exposed local */
  struct l_struct_OC_std_KD__KD_pair *llvm_cbe_this1;

#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_this_2e_addr) = llvm_cbe_this;
#line 0 "LLVM INTERNAL"
  llvm_cbe_this1 = *(&llvm_cbe_this_2e_addr);
#line 0 "LLVM INTERNAL"
  _ZNSt17_Rb_tree_iteratorISt4pairIKj10SAsnDynstrEEC1Ev(((&llvm_cbe_this1->field0)));
#line 0 "LLVM INTERNAL"
  *((&llvm_cbe_this1->field1)) = ((unsigned char )0);
#line 80 "stl_pair.h"
  return;
}


#line 0 "LLVM INTERNAL"
void _ZNSt17_Rb_tree_iteratorISt4pairIKj10SAsnDynstrEEC1Ev(struct l_struct_OC_std_KD__KD__Rb_tree_iterator *llvm_cbe_this) {
  struct l_struct_OC_std_KD__KD__Rb_tree_iterator *llvm_cbe_this_2e_addr;    /* Address-exposed local */
  struct l_struct_OC_std_KD__KD__Rb_tree_iterator *llvm_cbe_this1;

#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_this_2e_addr) = llvm_cbe_this;
#line 0 "LLVM INTERNAL"
  llvm_cbe_this1 = *(&llvm_cbe_this_2e_addr);
#line 0 "LLVM INTERNAL"
  _ZNSt17_Rb_tree_iteratorISt4pairIKj10SAsnDynstrEEC2Ev(llvm_cbe_this1);
#line 167 "stl_tree.h"
  return;
}


#line 0 "LLVM INTERNAL"
void _ZNSt17_Rb_tree_iteratorISt4pairIKj10SAsnDynstrEEC2Ev(struct l_struct_OC_std_KD__KD__Rb_tree_iterator *llvm_cbe_this) {
  struct l_struct_OC_std_KD__KD__Rb_tree_iterator *llvm_cbe_this_2e_addr;    /* Address-exposed local */
  struct l_struct_OC_std_KD__KD__Rb_tree_iterator *llvm_cbe_this1;

#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_this_2e_addr) = llvm_cbe_this;
#line 0 "LLVM INTERNAL"
  llvm_cbe_this1 = *(&llvm_cbe_this_2e_addr);
#line 0 "LLVM INTERNAL"
  *((&llvm_cbe_this1->field0)) = ((struct l_struct_OC_std_KD__KD__Rb_tree_node_base *)/*NULL*/0);
#line 167 "stl_tree.h"
  return;
}


#line 0 "LLVM INTERNAL"
void _ZN3Uec8UecEventD2Ev(struct l_class_OC_Uec_KD__KD_UecEvent *llvm_cbe_this) {
  struct l_class_OC_Uec_KD__KD_UecEvent *llvm_cbe_this_2e_addr;    /* Address-exposed local */
  struct l_class_OC_Uec_KD__KD_UecEvent *llvm_cbe_this1;

#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_this_2e_addr) = llvm_cbe_this;
#line 0 "LLVM INTERNAL"
  llvm_cbe_this1 = *(&llvm_cbe_this_2e_addr);
#line 0 "LLVM INTERNAL"
  *(((unsigned char ***)llvm_cbe_this1)) = ((&_ZTVN3Uec8UecEventE.array[((signed long long )2ull)]));
#line 111 "UecEvent.hpp"
  _ZN6CEventD2Ev((((struct l_class_OC_CEvent *)llvm_cbe_this1)));
#line 111 "UecEvent.hpp"
  return;
}


#line 0 "LLVM INTERNAL"
void _ZN6CEventD2Ev(struct l_class_OC_CEvent *llvm_cbe_this) {
  struct l_class_OC_CEvent *llvm_cbe_this_2e_addr;    /* Address-exposed local */
  struct l_class_OC_CEvent *llvm_cbe_this1;

#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_this_2e_addr) = llvm_cbe_this;
#line 0 "LLVM INTERNAL"
  llvm_cbe_this1 = *(&llvm_cbe_this_2e_addr);
#line 0 "LLVM INTERNAL"
  *(((unsigned char ***)llvm_cbe_this1)) = ((&_ZTV6CEvent.array[((signed long long )2ull)]));
#line 80 "CEvent.hpp"
  return;
}


#line 0 "LLVM INTERNAL"
void _ZN3Uec8UecEventD0Ev(struct l_class_OC_Uec_KD__KD_UecEvent *llvm_cbe_this) {
  struct l_class_OC_Uec_KD__KD_UecEvent *llvm_cbe_this_2e_addr;    /* Address-exposed local */
  struct l_class_OC_Uec_KD__KD_UecEvent *llvm_cbe_this1;

#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_this_2e_addr) = llvm_cbe_this;
#line 0 "LLVM INTERNAL"
  llvm_cbe_this1 = *(&llvm_cbe_this_2e_addr);
#line 0 "LLVM INTERNAL"
  _ZN3Uec8UecEventD1Ev(llvm_cbe_this1);
#line 0 "LLVM INTERNAL"
  _ZdlPv((((unsigned char *)llvm_cbe_this1)));
#line 111 "UecEvent.hpp"
  return;
}


#line 0 "LLVM INTERNAL"
struct l_class_OC_CEvent *_ZNK3Uec8UecEvent5CloneEv(struct l_class_OC_Uec_KD__KD_UecEvent *llvm_cbe_this) {
  struct l_class_OC_CEvent *llvm_cbe_retval;    /* Address-exposed local */
  struct l_class_OC_Uec_KD__KD_UecEvent *llvm_cbe_this_2e_addr;    /* Address-exposed local */
  struct l_class_OC_Uec_KD__KD_UecEvent *llvm_cbe_this1;
  unsigned char *llvm_cbe_call;
  struct l_class_OC_Uec_KD__KD_UecEvent *llvm_cbe_tmp__240;
  struct l_class_OC_CEvent *llvm_cbe_tmp__241;

#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_this_2e_addr) = llvm_cbe_this;
#line 0 "LLVM INTERNAL"
  llvm_cbe_this1 = *(&llvm_cbe_this_2e_addr);
#line 124 "UecEvent.hpp"
  llvm_cbe_call = _Znwm(72ull);
#line 124 "UecEvent.hpp"
  llvm_cbe_tmp__240 = ((struct l_class_OC_Uec_KD__KD_UecEvent *)llvm_cbe_call);
#line 124 "UecEvent.hpp"
  _ZN3Uec8UecEventC1ERKS0_(llvm_cbe_tmp__240, llvm_cbe_this1);
#line 124 "UecEvent.hpp"
  *(&llvm_cbe_retval) = (((struct l_class_OC_CEvent *)llvm_cbe_tmp__240));
#line 124 "UecEvent.hpp"
  llvm_cbe_tmp__241 = *(&llvm_cbe_retval);
#line 124 "UecEvent.hpp"
  return llvm_cbe_tmp__241;
}


#line 0 "LLVM INTERNAL"
void _ZN3Uec8UecEventC1ERKS0_(struct l_class_OC_Uec_KD__KD_UecEvent *llvm_cbe_this, struct l_class_OC_Uec_KD__KD_UecEvent *llvm_cbe_rhs) {
  struct l_class_OC_Uec_KD__KD_UecEvent *llvm_cbe_this_2e_addr;    /* Address-exposed local */
  struct l_class_OC_Uec_KD__KD_UecEvent *llvm_cbe_rhs_2e_addr;    /* Address-exposed local */
  struct l_class_OC_Uec_KD__KD_UecEvent *llvm_cbe_this1;
  struct l_class_OC_Uec_KD__KD_UecEvent *llvm_cbe_tmp__242;

#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_this_2e_addr) = llvm_cbe_this;
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_rhs_2e_addr) = llvm_cbe_rhs;
#line 0 "LLVM INTERNAL"
  llvm_cbe_this1 = *(&llvm_cbe_this_2e_addr);
#line 0 "LLVM INTERNAL"
  llvm_cbe_tmp__242 = *(&llvm_cbe_rhs_2e_addr);
#line 0 "LLVM INTERNAL"
  _ZN3Uec8UecEventC2ERKS0_(llvm_cbe_this1, llvm_cbe_tmp__242);
#line 145 "UecEvent.hpp"
  return;
}


#line 0 "LLVM INTERNAL"
void _ZN3Uec8UecEventC2ERKS0_(struct l_class_OC_Uec_KD__KD_UecEvent *llvm_cbe_this, struct l_class_OC_Uec_KD__KD_UecEvent *llvm_cbe_rhs) {
  struct l_class_OC_Uec_KD__KD_UecEvent *llvm_cbe_this_2e_addr;    /* Address-exposed local */
  struct l_class_OC_Uec_KD__KD_UecEvent *llvm_cbe_rhs_2e_addr;    /* Address-exposed local */
  struct l_class_OC_Uec_KD__KD_UecEvent *llvm_cbe_this1;
  struct l_class_OC_Uec_KD__KD_UecEvent *llvm_cbe_tmp;
  struct l_class_OC_BaseEvent *llvm_cbe_tmp__243;
  struct l_class_OC_Uec_KD__KD_UecEvent *llvm_cbe_tmp4;
  unsigned int llvm_cbe_tmp6;
  struct l_class_OC_Uec_KD__KD_UecEvent *llvm_cbe_tmp8;
  unsigned int llvm_cbe_tmp10;
  struct l_class_OC_Uec_KD__KD_UecEvent *llvm_cbe_tmp12;
  unsigned int llvm_cbe_tmp14;
  struct l_class_OC_Uec_KD__KD_UecEvent *llvm_cbe_tmp16;
  unsigned char *llvm_cbe_tmp18;
  struct l_class_OC_Uec_KD__KD_UecEvent *llvm_cbe_tmp20;
  unsigned int llvm_cbe_tmp22;
  struct l_class_OC_Uec_KD__KD_UecEvent *llvm_cbe_tmp24;
  unsigned int llvm_cbe_tmp26;
  struct l_class_OC_Uec_KD__KD_UecEvent *llvm_cbe_tmp28;
  unsigned char *llvm_cbe_tmp30;
  struct l_class_OC_Uec_KD__KD_UecEvent *llvm_cbe_tmp32;
  struct l_class_OC_Uec_KD__KD_UecBaseData *llvm_cbe_tmp34;
  struct l_class_OC_Uec_KD__KD_UecEvent *llvm_cbe_tmp36;
  unsigned int llvm_cbe_tmp38;

#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_this_2e_addr) = llvm_cbe_this;
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_rhs_2e_addr) = llvm_cbe_rhs;
#line 0 "LLVM INTERNAL"
  llvm_cbe_this1 = *(&llvm_cbe_this_2e_addr);
#line 0 "LLVM INTERNAL"
  llvm_cbe_tmp = *(&llvm_cbe_rhs_2e_addr);
#line 0 "LLVM INTERNAL"
  _ZN6CEventC2ERKS_((((struct l_class_OC_CEvent *)llvm_cbe_this1)), (((struct l_class_OC_CEvent *)llvm_cbe_tmp)));
#line 0 "LLVM INTERNAL"
  llvm_cbe_tmp__243 = ((struct l_class_OC_BaseEvent *)llvm_cbe_this1);
#line 0 "LLVM INTERNAL"
  *(((unsigned char ***)llvm_cbe_this1)) = ((&_ZTVN3Uec8UecEventE.array[((signed long long )2ull)]));
#line 0 "LLVM INTERNAL"
  llvm_cbe_tmp4 = *(&llvm_cbe_rhs_2e_addr);
#line 0 "LLVM INTERNAL"
  llvm_cbe_tmp6 = *((&llvm_cbe_tmp4->field1));
#line 0 "LLVM INTERNAL"
  *((&llvm_cbe_this1->field1)) = llvm_cbe_tmp6;
#line 0 "LLVM INTERNAL"
  llvm_cbe_tmp8 = *(&llvm_cbe_rhs_2e_addr);
#line 0 "LLVM INTERNAL"
  llvm_cbe_tmp10 = *((&llvm_cbe_tmp8->field2));
#line 0 "LLVM INTERNAL"
  *((&llvm_cbe_this1->field2)) = llvm_cbe_tmp10;
#line 0 "LLVM INTERNAL"
  llvm_cbe_tmp12 = *(&llvm_cbe_rhs_2e_addr);
#line 0 "LLVM INTERNAL"
  llvm_cbe_tmp14 = *((&llvm_cbe_tmp12->field3));
#line 0 "LLVM INTERNAL"
  *((&llvm_cbe_this1->field3)) = llvm_cbe_tmp14;
#line 0 "LLVM INTERNAL"
  llvm_cbe_tmp16 = *(&llvm_cbe_rhs_2e_addr);
#line 0 "LLVM INTERNAL"
  llvm_cbe_tmp18 = *((&llvm_cbe_tmp16->field4));
#line 0 "LLVM INTERNAL"
  *((&llvm_cbe_this1->field4)) = llvm_cbe_tmp18;
#line 0 "LLVM INTERNAL"
  llvm_cbe_tmp20 = *(&llvm_cbe_rhs_2e_addr);
#line 0 "LLVM INTERNAL"
  llvm_cbe_tmp22 = *((&llvm_cbe_tmp20->field5));
#line 0 "LLVM INTERNAL"
  *((&llvm_cbe_this1->field5)) = llvm_cbe_tmp22;
#line 0 "LLVM INTERNAL"
  llvm_cbe_tmp24 = *(&llvm_cbe_rhs_2e_addr);
#line 0 "LLVM INTERNAL"
  llvm_cbe_tmp26 = *((&llvm_cbe_tmp24->field6));
#line 0 "LLVM INTERNAL"
  *((&llvm_cbe_this1->field6)) = llvm_cbe_tmp26;
#line 0 "LLVM INTERNAL"
  llvm_cbe_tmp28 = *(&llvm_cbe_rhs_2e_addr);
#line 0 "LLVM INTERNAL"
  llvm_cbe_tmp30 = *((&llvm_cbe_tmp28->field7));
#line 0 "LLVM INTERNAL"
  *((&llvm_cbe_this1->field7)) = llvm_cbe_tmp30;
#line 0 "LLVM INTERNAL"
  llvm_cbe_tmp32 = *(&llvm_cbe_rhs_2e_addr);
#line 0 "LLVM INTERNAL"
  llvm_cbe_tmp34 = *((&llvm_cbe_tmp32->field8));
#line 0 "LLVM INTERNAL"
  *((&llvm_cbe_this1->field8)) = llvm_cbe_tmp34;
#line 0 "LLVM INTERNAL"
  llvm_cbe_tmp36 = *(&llvm_cbe_rhs_2e_addr);
#line 0 "LLVM INTERNAL"
  llvm_cbe_tmp38 = *((&llvm_cbe_tmp36->field9));
#line 0 "LLVM INTERNAL"
  *((&llvm_cbe_this1->field9)) = llvm_cbe_tmp38;
#line 145 "UecEvent.hpp"
  return;
}


#line 0 "LLVM INTERNAL"
void _ZN6CEventC2ERKS_(struct l_class_OC_CEvent *llvm_cbe_this, struct l_class_OC_CEvent *llvm_cbe_tmp__244) {
  struct l_class_OC_CEvent *llvm_cbe_this_2e_addr;    /* Address-exposed local */
  struct l_class_OC_CEvent *llvm_cbe__2e_addr;    /* Address-exposed local */
  struct l_class_OC_CEvent *llvm_cbe_this1;
  struct l_class_OC_CEvent *llvm_cbe_tmp2;
  unsigned int llvm_cbe_tmp4;
  struct l_class_OC_CEvent *llvm_cbe_tmp6;
  unsigned int llvm_cbe_tmp8;

#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_this_2e_addr) = llvm_cbe_this;
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe__2e_addr) = llvm_cbe_tmp__244;
#line 0 "LLVM INTERNAL"
  llvm_cbe_this1 = *(&llvm_cbe_this_2e_addr);
#line 0 "LLVM INTERNAL"
  *(((unsigned char ***)llvm_cbe_this1)) = ((&_ZTV6CEvent.array[((signed long long )2ull)]));
#line 0 "LLVM INTERNAL"
  llvm_cbe_tmp2 = *(&llvm_cbe__2e_addr);
#line 0 "LLVM INTERNAL"
  llvm_cbe_tmp4 = *((&llvm_cbe_tmp2->field1));
#line 0 "LLVM INTERNAL"
  *((&llvm_cbe_this1->field1)) = llvm_cbe_tmp4;
#line 0 "LLVM INTERNAL"
  llvm_cbe_tmp6 = *(&llvm_cbe__2e_addr);
#line 0 "LLVM INTERNAL"
  llvm_cbe_tmp8 = *((&llvm_cbe_tmp6->field2));
#line 0 "LLVM INTERNAL"
  *((&llvm_cbe_this1->field2)) = llvm_cbe_tmp8;
#line 42 "CEvent.hpp"
  return;
}


#line 0 "LLVM INTERNAL"
void _ZN6CEventD1Ev(struct l_class_OC_CEvent *llvm_cbe_this) {
  struct l_class_OC_CEvent *llvm_cbe_this_2e_addr;    /* Address-exposed local */
  struct l_class_OC_CEvent *llvm_cbe_this1;

#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_this_2e_addr) = llvm_cbe_this;
#line 0 "LLVM INTERNAL"
  llvm_cbe_this1 = *(&llvm_cbe_this_2e_addr);
#line 0 "LLVM INTERNAL"
  _ZN6CEventD2Ev(llvm_cbe_this1);
#line 80 "CEvent.hpp"
  return;
}


#line 0 "LLVM INTERNAL"
void _ZN6CEventD0Ev(struct l_class_OC_CEvent *llvm_cbe_this) {
  struct l_class_OC_CEvent *llvm_cbe_this_2e_addr;    /* Address-exposed local */
  struct l_class_OC_CEvent *llvm_cbe_this1;

#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_this_2e_addr) = llvm_cbe_this;
#line 0 "LLVM INTERNAL"
  llvm_cbe_this1 = *(&llvm_cbe_this_2e_addr);
#line 0 "LLVM INTERNAL"
  _ZN6CEventD1Ev(llvm_cbe_this1);
#line 0 "LLVM INTERNAL"
  _ZdlPv((((unsigned char *)llvm_cbe_this1)));
#line 80 "CEvent.hpp"
  return;
}


#line 0 "LLVM INTERNAL"
struct l_class_OC_CEvent *_ZNK6CEvent5CloneEv(struct l_class_OC_CEvent *llvm_cbe_this) {
  struct l_class_OC_CEvent *llvm_cbe_retval;    /* Address-exposed local */
  struct l_class_OC_CEvent *llvm_cbe_this_2e_addr;    /* Address-exposed local */
  struct l_class_OC_CEvent *llvm_cbe_this1;
  unsigned char *llvm_cbe_call;
  struct l_class_OC_CEvent *llvm_cbe_tmp__245;
  struct l_class_OC_CEvent *llvm_cbe_tmp__246;

#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_this_2e_addr) = llvm_cbe_this;
#line 0 "LLVM INTERNAL"
  llvm_cbe_this1 = *(&llvm_cbe_this_2e_addr);
#line 117 "CEvent.hpp"
  llvm_cbe_call = _Znwm(16ull);
#line 117 "CEvent.hpp"
  llvm_cbe_tmp__245 = ((struct l_class_OC_CEvent *)llvm_cbe_call);
#line 117 "CEvent.hpp"
  _ZN6CEventC1ERKS_(llvm_cbe_tmp__245, llvm_cbe_this1);
#line 117 "CEvent.hpp"
  *(&llvm_cbe_retval) = llvm_cbe_tmp__245;
#line 117 "CEvent.hpp"
  llvm_cbe_tmp__246 = *(&llvm_cbe_retval);
#line 117 "CEvent.hpp"
  return llvm_cbe_tmp__246;
}


#line 0 "LLVM INTERNAL"
void _ZN6CEventC1ERKS_(struct l_class_OC_CEvent *llvm_cbe_this, struct l_class_OC_CEvent *llvm_cbe_tmp__247) {
  struct l_class_OC_CEvent *llvm_cbe_this_2e_addr;    /* Address-exposed local */
  struct l_class_OC_CEvent *llvm_cbe__2e_addr;    /* Address-exposed local */
  struct l_class_OC_CEvent *llvm_cbe_this1;
  struct l_class_OC_CEvent *llvm_cbe_tmp__248;

#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_this_2e_addr) = llvm_cbe_this;
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe__2e_addr) = llvm_cbe_tmp__247;
#line 0 "LLVM INTERNAL"
  llvm_cbe_this1 = *(&llvm_cbe_this_2e_addr);
#line 0 "LLVM INTERNAL"
  llvm_cbe_tmp__248 = *(&llvm_cbe__2e_addr);
#line 0 "LLVM INTERNAL"
  _ZN6CEventC2ERKS_(llvm_cbe_this1, llvm_cbe_tmp__248);
#line 42 "CEvent.hpp"
  return;
}


#line 0 "LLVM INTERNAL"
void _ZN3Uec8UecEventC2EjNS_15EUecServiceTypeEjPvjiS2_j(struct l_class_OC_Uec_KD__KD_UecEvent *llvm_cbe_this, unsigned int llvm_cbe_messageId, unsigned int llvm_cbe_serviceType, unsigned int llvm_cbe_instanceId, unsigned char *llvm_cbe_payLoadPtr, unsigned int llvm_cbe_payloadSize, unsigned int llvm_cbe_asn1PayLoadId, unsigned char *llvm_cbe_asn1PayLoadPtr, unsigned int llvm_cbe_asn1PayloadSize) {
  struct l_class_OC_Uec_KD__KD_UecEvent *llvm_cbe_this_2e_addr;    /* Address-exposed local */
  unsigned int llvm_cbe_messageId_2e_addr;    /* Address-exposed local */
  unsigned int llvm_cbe_serviceType_2e_addr;    /* Address-exposed local */
  unsigned int llvm_cbe_instanceId_2e_addr;    /* Address-exposed local */
  unsigned char *llvm_cbe_payLoadPtr_2e_addr;    /* Address-exposed local */
  unsigned int llvm_cbe_payloadSize_2e_addr;    /* Address-exposed local */
  unsigned int llvm_cbe_asn1PayLoadId_2e_addr;    /* Address-exposed local */
  unsigned char *llvm_cbe_asn1PayLoadPtr_2e_addr;    /* Address-exposed local */
  unsigned int llvm_cbe_asn1PayloadSize_2e_addr;    /* Address-exposed local */
  struct l_class_OC_Uec_KD__KD_UecEvent *llvm_cbe_this1;
  unsigned int llvm_cbe_tmp;
  struct l_class_OC_BaseEvent *llvm_cbe_tmp__249;
  unsigned int llvm_cbe_tmp4;
  unsigned int llvm_cbe_tmp6;
  unsigned int llvm_cbe_tmp8;
  unsigned char *llvm_cbe_tmp10;
  unsigned int llvm_cbe_tmp12;
  unsigned int llvm_cbe_tmp14;
  unsigned char *llvm_cbe_tmp16;

#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_this_2e_addr) = llvm_cbe_this;
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_messageId_2e_addr) = llvm_cbe_messageId;
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_serviceType_2e_addr) = llvm_cbe_serviceType;
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_instanceId_2e_addr) = llvm_cbe_instanceId;
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_payLoadPtr_2e_addr) = llvm_cbe_payLoadPtr;
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_payloadSize_2e_addr) = llvm_cbe_payloadSize;
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_asn1PayLoadId_2e_addr) = llvm_cbe_asn1PayLoadId;
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_asn1PayLoadPtr_2e_addr) = llvm_cbe_asn1PayLoadPtr;
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_asn1PayloadSize_2e_addr) = llvm_cbe_asn1PayloadSize;
#line 0 "LLVM INTERNAL"
  llvm_cbe_this1 = *(&llvm_cbe_this_2e_addr);
#line 0 "LLVM INTERNAL"
  llvm_cbe_tmp = *(&llvm_cbe_messageId_2e_addr);
#line 0 "LLVM INTERNAL"
  _ZN6CEventC2ENS_10EEventTypeEj((((struct l_class_OC_CEvent *)llvm_cbe_this1)), 0u, llvm_cbe_tmp);
#line 0 "LLVM INTERNAL"
  llvm_cbe_tmp__249 = ((struct l_class_OC_BaseEvent *)llvm_cbe_this1);
#line 0 "LLVM INTERNAL"
  *(((unsigned char ***)llvm_cbe_this1)) = ((&_ZTVN3Uec8UecEventE.array[((signed long long )2ull)]));
#line 0 "LLVM INTERNAL"
  llvm_cbe_tmp4 = *(&llvm_cbe_serviceType_2e_addr);
#line 0 "LLVM INTERNAL"
  *((&llvm_cbe_this1->field1)) = llvm_cbe_tmp4;
#line 0 "LLVM INTERNAL"
  llvm_cbe_tmp6 = *(&llvm_cbe_instanceId_2e_addr);
#line 0 "LLVM INTERNAL"
  *((&llvm_cbe_this1->field2)) = llvm_cbe_tmp6;
#line 0 "LLVM INTERNAL"
  llvm_cbe_tmp8 = *(&llvm_cbe_payloadSize_2e_addr);
#line 0 "LLVM INTERNAL"
  *((&llvm_cbe_this1->field3)) = llvm_cbe_tmp8;
#line 0 "LLVM INTERNAL"
  llvm_cbe_tmp10 = *(&llvm_cbe_payLoadPtr_2e_addr);
#line 0 "LLVM INTERNAL"
  *((&llvm_cbe_this1->field4)) = llvm_cbe_tmp10;
#line 0 "LLVM INTERNAL"
  llvm_cbe_tmp12 = *(&llvm_cbe_asn1PayLoadId_2e_addr);
#line 0 "LLVM INTERNAL"
  *((&llvm_cbe_this1->field5)) = llvm_cbe_tmp12;
#line 0 "LLVM INTERNAL"
  llvm_cbe_tmp14 = *(&llvm_cbe_asn1PayloadSize_2e_addr);
#line 0 "LLVM INTERNAL"
  *((&llvm_cbe_this1->field6)) = llvm_cbe_tmp14;
#line 0 "LLVM INTERNAL"
  llvm_cbe_tmp16 = *(&llvm_cbe_asn1PayLoadPtr_2e_addr);
#line 0 "LLVM INTERNAL"
  *((&llvm_cbe_this1->field7)) = llvm_cbe_tmp16;
#line 0 "LLVM INTERNAL"
  *((&llvm_cbe_this1->field8)) = ((struct l_class_OC_Uec_KD__KD_UecBaseData *)/*NULL*/0);
#line 0 "LLVM INTERNAL"
  *((&llvm_cbe_this1->field9)) = 0u;
#line 101 "UecEvent.hpp"
  return;
}


#line 0 "LLVM INTERNAL"
void _ZN6CEventC2ENS_10EEventTypeEj(struct l_class_OC_CEvent *llvm_cbe_this, unsigned int llvm_cbe_eventType, unsigned int llvm_cbe_eventId) {
  struct l_class_OC_CEvent *llvm_cbe_this_2e_addr;    /* Address-exposed local */
  unsigned int llvm_cbe_eventType_2e_addr;    /* Address-exposed local */
  unsigned int llvm_cbe_eventId_2e_addr;    /* Address-exposed local */
  struct l_class_OC_CEvent *llvm_cbe_this1;
  unsigned int llvm_cbe_tmp;
  unsigned int llvm_cbe_tmp3;

#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_this_2e_addr) = llvm_cbe_this;
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_eventType_2e_addr) = llvm_cbe_eventType;
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_eventId_2e_addr) = llvm_cbe_eventId;
#line 0 "LLVM INTERNAL"
  llvm_cbe_this1 = *(&llvm_cbe_this_2e_addr);
#line 0 "LLVM INTERNAL"
  *(((unsigned char ***)llvm_cbe_this1)) = ((&_ZTV6CEvent.array[((signed long long )2ull)]));
#line 71 "CEvent.hpp"
  llvm_cbe_tmp = *(&llvm_cbe_eventType_2e_addr);
#line 71 "CEvent.hpp"
  *((&llvm_cbe_this1->field1)) = llvm_cbe_tmp;
#line 72 "CEvent.hpp"
  llvm_cbe_tmp3 = *(&llvm_cbe_eventId_2e_addr);
#line 72 "CEvent.hpp"
  *((&llvm_cbe_this1->field2)) = llvm_cbe_tmp3;
#line 73 "CEvent.hpp"
  return;
}


#line 0 "LLVM INTERNAL"
void _ZN3Uec16UecUeContextData10connectMmeEv(struct l_class_OC_Uec_KD__KD_UecUeContextData *llvm_cbe_this) {
  struct l_class_OC_Uec_KD__KD_UecUeContextData *llvm_cbe_this_2e_addr;    /* Address-exposed local */
  struct l_class_OC_Uec_KD__KD_UecUeContextData *llvm_cbe_this1;
  unsigned int llvm_cbe_tmp3;
  struct l_class_OC_BaseEvent *llvm_cbe_call;
  unsigned int llvm_cbe_tmp8;
  struct l_class_OC_BaseEvent *llvm_cbe_call9;
  struct l_class_OC_BaseEvent *llvm_cbe_call10;

#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_this_2e_addr) = llvm_cbe_this;
#line 0 "LLVM INTERNAL"
  llvm_cbe_this1 = *(&llvm_cbe_this_2e_addr);
#line 859 "UecUeContextData.hpp"
  llvm_cbe_tmp3 = *((&((&llvm_cbe_this1->field1))->field1));
#line 859 "UecUeContextData.hpp"
  *((&((&llvm_cbe_this1->field1))->field1)) = (llvm_cbe_tmp3 | 2u);
#line 860 "UecUeContextData.hpp"
  llvm_cbe_call = _ZN11DummyStreamlsIPKcEERS_T_((&mout), ((&_OC_str100.array[((signed int )0u)])));
#line 860 "UecUeContextData.hpp"
  llvm_cbe_tmp8 = *((&((&llvm_cbe_this1->field1))->field5));
#line 860 "UecUeContextData.hpp"
  llvm_cbe_call9 = _ZN11DummyStreamlsIjEERS_T_(llvm_cbe_call, llvm_cbe_tmp8);
#line 860 "UecUeContextData.hpp"
  llvm_cbe_call10 = _ZN11DummyStreamlsIPKcEERS_T_(llvm_cbe_call9, ((&_OC_str80.array[((signed int )0u)])));
#line 861 "UecUeContextData.hpp"
  return;
}


#line 0 "LLVM INTERNAL"
unsigned int _ZNSt8ios_base4setfESt13_Ios_FmtflagsS0_(struct l_class_OC_std_KD__KD_ios_base *llvm_cbe_this, unsigned int llvm_cbe___fmtfl, unsigned int llvm_cbe___mask) {
  unsigned int llvm_cbe_retval;    /* Address-exposed local */
  struct l_class_OC_std_KD__KD_ios_base *llvm_cbe_this_2e_addr;    /* Address-exposed local */
  unsigned int llvm_cbe___fmtfl_2e_addr;    /* Address-exposed local */
  unsigned int llvm_cbe___mask_2e_addr;    /* Address-exposed local */
  unsigned int llvm_cbe___old;    /* Address-exposed local */
  struct l_class_OC_std_KD__KD_ios_base *llvm_cbe_this1;
  unsigned int llvm_cbe_tmp2;
  unsigned int llvm_cbe_tmp4;
  unsigned int llvm_cbe_call;
  unsigned int *llvm_cbe_call5;
  unsigned int llvm_cbe_tmp6;
  unsigned int llvm_cbe_tmp8;
  unsigned int llvm_cbe_tmp9;
  unsigned int llvm_cbe_call10;
  unsigned int *llvm_cbe_call11;
  unsigned int llvm_cbe_tmp12;
  unsigned int llvm_cbe_tmp13;
  unsigned int llvm_cbe_tmp__250;

#line 0 "LLVM INTERNAL"
  *(&llvm_cbe_this_2e_addr) = llvm_cbe_this;
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe___fmtfl_2e_addr) = llvm_cbe___fmtfl;
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe___mask_2e_addr) = llvm_cbe___mask;
#line 0 "LLVM INTERNAL"
  llvm_cbe_this1 = *(&llvm_cbe_this_2e_addr);
#line 600 "ios_base.h"
  llvm_cbe_tmp2 = *((&llvm_cbe_this1->field3));
#line 600 "ios_base.h"
  *(&llvm_cbe___old) = llvm_cbe_tmp2;
#line 601 "ios_base.h"
  llvm_cbe_tmp4 = *(&llvm_cbe___mask_2e_addr);
#line 601 "ios_base.h"
  llvm_cbe_call = _ZStcoSt13_Ios_Fmtflags(llvm_cbe_tmp4);
#line 601 "ios_base.h"
  llvm_cbe_call5 = _ZStaNRSt13_Ios_FmtflagsS_(((&llvm_cbe_this1->field3)), llvm_cbe_call);
#line 601 "ios_base.h"
  llvm_cbe_tmp6 = *llvm_cbe_call5;
#line 602 "ios_base.h"
  llvm_cbe_tmp8 = *(&llvm_cbe___fmtfl_2e_addr);
#line 602 "ios_base.h"
  llvm_cbe_tmp9 = *(&llvm_cbe___mask_2e_addr);
#line 602 "ios_base.h"
  llvm_cbe_call10 = _ZStanSt13_Ios_FmtflagsS_(llvm_cbe_tmp8, llvm_cbe_tmp9);
#line 602 "ios_base.h"
  llvm_cbe_call11 = _ZStoRRSt13_Ios_FmtflagsS_(((&llvm_cbe_this1->field3)), llvm_cbe_call10);
#line 602 "ios_base.h"
  llvm_cbe_tmp12 = *llvm_cbe_call11;
#line 603 "ios_base.h"
  llvm_cbe_tmp13 = *(&llvm_cbe___old);
#line 603 "ios_base.h"
  *(&llvm_cbe_retval) = llvm_cbe_tmp13;
#line 604 "ios_base.h"
  llvm_cbe_tmp__250 = *(&llvm_cbe_retval);
#line 604 "ios_base.h"
  return llvm_cbe_tmp__250;
}


#line 0 "LLVM INTERNAL"
unsigned int *_ZStaNRSt13_Ios_FmtflagsS_(unsigned int *llvm_cbe___a, unsigned int llvm_cbe___b) {
  unsigned int *llvm_cbe_retval;    /* Address-exposed local */
  unsigned int *llvm_cbe___a_2e_addr;    /* Address-exposed local */
  unsigned int llvm_cbe___b_2e_addr;    /* Address-exposed local */
  unsigned int *llvm_cbe_tmp;
  unsigned int *llvm_cbe_tmp1;
  unsigned int llvm_cbe_tmp2;
  unsigned int llvm_cbe_tmp3;
  unsigned int llvm_cbe_call;
  unsigned int *llvm_cbe_tmp__251;

#line 0 "LLVM INTERNAL"
  *(&llvm_cbe___a_2e_addr) = llvm_cbe___a;
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe___b_2e_addr) = llvm_cbe___b;
#line 100 "ios_base.h"
  llvm_cbe_tmp = *(&llvm_cbe___a_2e_addr);
#line 100 "ios_base.h"
  llvm_cbe_tmp1 = *(&llvm_cbe___a_2e_addr);
#line 100 "ios_base.h"
  llvm_cbe_tmp2 = *llvm_cbe_tmp1;
#line 100 "ios_base.h"
  llvm_cbe_tmp3 = *(&llvm_cbe___b_2e_addr);
#line 100 "ios_base.h"
  llvm_cbe_call = _ZStanSt13_Ios_FmtflagsS_(llvm_cbe_tmp2, llvm_cbe_tmp3);
#line 100 "ios_base.h"
  *llvm_cbe_tmp = llvm_cbe_call;
#line 100 "ios_base.h"
  *(&llvm_cbe_retval) = llvm_cbe_tmp;
#line 100 "ios_base.h"
  llvm_cbe_tmp__251 = *(&llvm_cbe_retval);
#line 100 "ios_base.h"
  return llvm_cbe_tmp__251;
}


#line 0 "LLVM INTERNAL"
unsigned int _ZStcoSt13_Ios_Fmtflags(unsigned int llvm_cbe___a) {
  unsigned int llvm_cbe_retval;    /* Address-exposed local */
  unsigned int llvm_cbe___a_2e_addr;    /* Address-exposed local */
  unsigned int llvm_cbe_tmp;
  unsigned int llvm_cbe_tmp__252;

#line 0 "LLVM INTERNAL"
  *(&llvm_cbe___a_2e_addr) = llvm_cbe___a;
#line 108 "ios_base.h"
  llvm_cbe_tmp = *(&llvm_cbe___a_2e_addr);
#line 108 "ios_base.h"
  *(&llvm_cbe_retval) = (llvm_cbe_tmp ^ 4294967295u);
#line 108 "ios_base.h"
  llvm_cbe_tmp__252 = *(&llvm_cbe_retval);
#line 108 "ios_base.h"
  return llvm_cbe_tmp__252;
}


#line 0 "LLVM INTERNAL"
unsigned int *_ZStoRRSt13_Ios_FmtflagsS_(unsigned int *llvm_cbe___a, unsigned int llvm_cbe___b) {
  unsigned int *llvm_cbe_retval;    /* Address-exposed local */
  unsigned int *llvm_cbe___a_2e_addr;    /* Address-exposed local */
  unsigned int llvm_cbe___b_2e_addr;    /* Address-exposed local */
  unsigned int *llvm_cbe_tmp;
  unsigned int *llvm_cbe_tmp1;
  unsigned int llvm_cbe_tmp2;
  unsigned int llvm_cbe_tmp3;
  unsigned int llvm_cbe_call;
  unsigned int *llvm_cbe_tmp__253;

#line 0 "LLVM INTERNAL"
  *(&llvm_cbe___a_2e_addr) = llvm_cbe___a;
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe___b_2e_addr) = llvm_cbe___b;
#line 96 "ios_base.h"
  llvm_cbe_tmp = *(&llvm_cbe___a_2e_addr);
#line 96 "ios_base.h"
  llvm_cbe_tmp1 = *(&llvm_cbe___a_2e_addr);
#line 96 "ios_base.h"
  llvm_cbe_tmp2 = *llvm_cbe_tmp1;
#line 96 "ios_base.h"
  llvm_cbe_tmp3 = *(&llvm_cbe___b_2e_addr);
#line 96 "ios_base.h"
  llvm_cbe_call = _ZStorSt13_Ios_FmtflagsS_(llvm_cbe_tmp2, llvm_cbe_tmp3);
#line 96 "ios_base.h"
  *llvm_cbe_tmp = llvm_cbe_call;
#line 96 "ios_base.h"
  *(&llvm_cbe_retval) = llvm_cbe_tmp;
#line 96 "ios_base.h"
  llvm_cbe_tmp__253 = *(&llvm_cbe_retval);
#line 96 "ios_base.h"
  return llvm_cbe_tmp__253;
}


#line 0 "LLVM INTERNAL"
unsigned int _ZStanSt13_Ios_FmtflagsS_(unsigned int llvm_cbe___a, unsigned int llvm_cbe___b) {
  unsigned int llvm_cbe_retval;    /* Address-exposed local */
  unsigned int llvm_cbe___a_2e_addr;    /* Address-exposed local */
  unsigned int llvm_cbe___b_2e_addr;    /* Address-exposed local */
  unsigned int llvm_cbe_tmp;
  unsigned int llvm_cbe_tmp1;
  unsigned int llvm_cbe_tmp__254;

#line 0 "LLVM INTERNAL"
  *(&llvm_cbe___a_2e_addr) = llvm_cbe___a;
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe___b_2e_addr) = llvm_cbe___b;
#line 84 "ios_base.h"
  llvm_cbe_tmp = *(&llvm_cbe___a_2e_addr);
#line 84 "ios_base.h"
  llvm_cbe_tmp1 = *(&llvm_cbe___b_2e_addr);
#line 84 "ios_base.h"
  *(&llvm_cbe_retval) = (llvm_cbe_tmp & llvm_cbe_tmp1);
#line 84 "ios_base.h"
  llvm_cbe_tmp__254 = *(&llvm_cbe_retval);
#line 84 "ios_base.h"
  return llvm_cbe_tmp__254;
}


#line 0 "LLVM INTERNAL"
unsigned int _ZStorSt13_Ios_FmtflagsS_(unsigned int llvm_cbe___a, unsigned int llvm_cbe___b) {
  unsigned int llvm_cbe_retval;    /* Address-exposed local */
  unsigned int llvm_cbe___a_2e_addr;    /* Address-exposed local */
  unsigned int llvm_cbe___b_2e_addr;    /* Address-exposed local */
  unsigned int llvm_cbe_tmp;
  unsigned int llvm_cbe_tmp1;
  unsigned int llvm_cbe_tmp__255;

#line 0 "LLVM INTERNAL"
  *(&llvm_cbe___a_2e_addr) = llvm_cbe___a;
#line 0 "LLVM INTERNAL"
  *(&llvm_cbe___b_2e_addr) = llvm_cbe___b;
#line 88 "ios_base.h"
  llvm_cbe_tmp = *(&llvm_cbe___a_2e_addr);
#line 88 "ios_base.h"
  llvm_cbe_tmp1 = *(&llvm_cbe___b_2e_addr);
#line 88 "ios_base.h"
  *(&llvm_cbe_retval) = (llvm_cbe_tmp | llvm_cbe_tmp1);
#line 88 "ios_base.h"
  llvm_cbe_tmp__255 = *(&llvm_cbe_retval);
#line 88 "ios_base.h"
  return llvm_cbe_tmp__255;
}


#line 0 "LLVM INTERNAL"
static void _GLOBAL__I_a(void) {
#line 0 "LLVM INTERNAL"
  __cxx_global_var_init();
#line 0 "LLVM INTERNAL"
  __cxx_global_var_init1();
#line 0 "LLVM INTERNAL"
  return;
}


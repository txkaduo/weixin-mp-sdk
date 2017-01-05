module WeiXin.PublicPlatform.Pay.BankCode where

import ClassyPrelude


data BankCode = ICBC_DEBIT
              | ICBC_CREDIT
              | ABC_DEBIT
              | ABC_CREDIT
              | PSBC_DEBIT
              | PSBC_CREDIT
              | CCB_DEBIT
              | CCB_CREDIT
              | CMB_DEBIT
              | CMB_CREDIT
              | BOC_DEBIT
              | BOC_CREDIT
              | COMM_DEBIT
              | SPDB_DEBIT
              | SPDB_CREDIT
              | GDB_DEBIT
              | GDB_CREDIT
              | CMBC_DEBIT
              | CMBC_CREDIT
              | PAB_DEBIT
              | PAB_CREDIT
              | CEB_DEBIT
              | CEB_CREDIT
              | CIB_DEBIT
              | CIB_CREDIT
              | CITIC_DEBIT
              | CITIC_CREDIT
              | BOSH_DEBIT
              | BOSH_CREDIT
              | CRB_DEBIT
              | HZB_DEBIT
              | HZB_CREDIT
              | BSB_DEBIT
              | BSB_CREDIT
              | CQB_DEBIT
              | SDEB_DEBIT
              | SZRCB_DEBIT
              | HRBB_DEBIT
              | BOCD_DEBIT
              | GDNYB_DEBIT
              | GDNYB_CREDIT
              | GZCB_DEBIT
              | GZCB_CREDIT
              | JSB_DEBIT
              | JSB_CREDIT
              | NBCB_DEBIT
              | NBCB_CREDIT
              | NJCB_DEBIT
              | JZB_DEBIT
              | KRCB_DEBIT
              | LJB_DEBIT
              | LNNX_DEBIT
              | LZB_DEBIT
              | WRCB_DEBIT
              | ZYB_DEBIT
              | ZJRCUB_DEBIT
              | WZB_DEBIT
              | XAB_DEBIT
              | JXNXB_DEBIT
              | NCB_DEBIT
              | NYCCB_DEBIT
              | NMGNX_DEBIT
              | SXXH_DEBIT
              | SRCB_CREDIT
              | SJB_DEBIT
              | SDRCU_DEBIT
              | SRCB_DEBIT
              | SCNX_DEBIT
              | QLB_DEBIT
              | QDCCB_DEBIT
              | PZHCCB_DEBIT
              | ZJTLCB_DEBIT
              | TJBHB_DEBIT
              | WEB_DEBIT
              | YNRCCB_DEBIT
              | WFB_DEBIT
              | WHRC_DEBIT
              | ORDOSB_DEBIT
              | XJRCCB_DEBIT
              | ORDOSB_CREDIT
              | CSRCB_DEBIT
              | JSNX_DEBIT
              | GRCB_CREDIT
              | GLB_DEBIT
              | GDRCU_DEBIT
              | GDHX_DEBIT
              | FJNX_DEBIT
              | DYCCB_DEBIT
              | DRCB_DEBIT
              | CZCB_DEBIT
              | CZB_DEBIT
              | CZB_CREDIT
              | GRCB_DEBIT
              | CSCB_DEBIT
              | CQRCB_DEBIT
              | CBHB_DEBIT
              | BOIMCB_DEBIT
              | BOD_DEBIT
              | BOD_CREDIT
              | BOB_DEBIT
              | BNC_DEBIT
              | BJRCB_DEBIT
              | AE_CREDIT
              | GYCB_CREDIT
              | JSHB_DEBIT
              | JRCB_DEBIT
              | JNRCB_DEBIT
              | JLNX_DEBIT
              | JLB_DEBIT
              | JJCCB_DEBIT
              | HXB_DEBIT
              | HXB_CREDIT
              | HUNNX_DEBIT
              | HSB_DEBIT
              | HSBC_DEBIT
              | HRXJB_DEBIT
              | HNNX_DEBIT
              | HKBEA_DEBIT
              | HEBNX_DEBIT
              | HBNX_DEBIT
              | HBNX_CREDIT
              | GYCB_DEBIT
              | GSNX_DEBIT
              | JCB_CREDIT
              | MASTERCARD_CREDIT
              | VISA_CREDIT

              --  远行出现的代码, 不在文档内
              | CFT
              deriving (Show, Read, Eq, Ord, Enum, Bounded)


parseBankCode :: (Element c ~ Char, MonoFoldable c) => c -> Maybe BankCode
parseBankCode = readMay

renderBankCodeSC :: BankCode -> Text
renderBankCodeSC ICBC_DEBIT        = "工商银行(借记卡)"
renderBankCodeSC ICBC_CREDIT       = "工商银行(信用卡)"
renderBankCodeSC ABC_DEBIT         = "农业银行(借记卡)"
renderBankCodeSC ABC_CREDIT        = "农业银行(信用卡)"
renderBankCodeSC PSBC_DEBIT        = "邮政储蓄银行(借记卡)"
renderBankCodeSC PSBC_CREDIT       = "邮政储蓄银行(信用卡)"
renderBankCodeSC CCB_DEBIT         = "建设银行(借记卡)"
renderBankCodeSC CCB_CREDIT        = "建设银行(信用卡)"
renderBankCodeSC CMB_DEBIT         = "招商银行(借记卡)"
renderBankCodeSC CMB_CREDIT        = "招商银行(信用卡)"
renderBankCodeSC BOC_DEBIT         = "中国银行(借记卡)"
renderBankCodeSC BOC_CREDIT        = "中国银行(信用卡)"
renderBankCodeSC COMM_DEBIT        = "交通银行(借记卡)"
renderBankCodeSC SPDB_DEBIT        = "浦发银行(借记卡)"
renderBankCodeSC SPDB_CREDIT       = "浦发银行(信用卡)"
renderBankCodeSC GDB_DEBIT         = "广发银行(借记卡)"
renderBankCodeSC GDB_CREDIT        = "广发银行(信用卡)"
renderBankCodeSC CMBC_DEBIT        = "民生银行(借记卡)"
renderBankCodeSC CMBC_CREDIT       = "民生银行(信用卡)"
renderBankCodeSC PAB_DEBIT         = "平安银行(借记卡)"
renderBankCodeSC PAB_CREDIT        = "平安银行(信用卡)"
renderBankCodeSC CEB_DEBIT         = "光大银行(借记卡)"
renderBankCodeSC CEB_CREDIT        = "光大银行(信用卡)"
renderBankCodeSC CIB_DEBIT         = "兴业银行(借记卡)"
renderBankCodeSC CIB_CREDIT        = "兴业银行(信用卡)"
renderBankCodeSC CITIC_DEBIT       = "中信银行(借记卡)"
renderBankCodeSC CITIC_CREDIT      = "中信银行(信用卡)"
renderBankCodeSC BOSH_DEBIT        = "上海银行(借记卡)"
renderBankCodeSC BOSH_CREDIT       = "上海银行(信用卡)"
renderBankCodeSC CRB_DEBIT         = "华润银行(借记卡)"
renderBankCodeSC HZB_DEBIT         = "杭州银行(借记卡)"
renderBankCodeSC HZB_CREDIT        = "杭州银行(信用卡)"
renderBankCodeSC BSB_DEBIT         = "包商银行(借记卡)"
renderBankCodeSC BSB_CREDIT        = "包商银行(信用卡)"
renderBankCodeSC CQB_DEBIT         = "重庆银行(借记卡)"
renderBankCodeSC SDEB_DEBIT        = "顺德农商行(借记卡)"
renderBankCodeSC SZRCB_DEBIT       = "深圳农商银行(借记卡)"
renderBankCodeSC HRBB_DEBIT        = "哈尔滨银行(借记卡)"
renderBankCodeSC BOCD_DEBIT        = "成都银行(借记卡)"
renderBankCodeSC GDNYB_DEBIT       = "南粤银行(借记卡)"
renderBankCodeSC GDNYB_CREDIT      = "南粤银行(信用卡)"
renderBankCodeSC GZCB_DEBIT        = "广州银行(借记卡)"
renderBankCodeSC GZCB_CREDIT       = "广州银行(信用卡)"
renderBankCodeSC JSB_DEBIT         = "江苏银行(借记卡)"
renderBankCodeSC JSB_CREDIT        = "江苏银行(信用卡)"
renderBankCodeSC NBCB_DEBIT        = "宁波银行(借记卡)"
renderBankCodeSC NBCB_CREDIT       = "宁波银行(信用卡)"
renderBankCodeSC NJCB_DEBIT        = "南京银行(借记卡)"
renderBankCodeSC JZB_DEBIT         = "晋中银行(借记卡)"
renderBankCodeSC KRCB_DEBIT        = "昆山农商(借记卡)"
renderBankCodeSC LJB_DEBIT         = "龙江银行(借记卡)"
renderBankCodeSC LNNX_DEBIT        = "辽宁农信(借记卡)"
renderBankCodeSC LZB_DEBIT         = "兰州银行(借记卡)"
renderBankCodeSC WRCB_DEBIT        = "无锡农商(借记卡)"
renderBankCodeSC ZYB_DEBIT         = "中原银行(借记卡)"
renderBankCodeSC ZJRCUB_DEBIT      = "浙江农信(借记卡)"
renderBankCodeSC WZB_DEBIT         = "温州银行(借记卡)"
renderBankCodeSC XAB_DEBIT         = "西安银行(借记卡)"
renderBankCodeSC JXNXB_DEBIT       = "江西农信(借记卡)"
renderBankCodeSC NCB_DEBIT         = "宁波通商银行(借记卡)"
renderBankCodeSC NYCCB_DEBIT       = "南阳村镇银行(借记卡)"
renderBankCodeSC NMGNX_DEBIT       = "内蒙古农信(借记卡)"
renderBankCodeSC SXXH_DEBIT        = "陕西信合(借记卡)"
renderBankCodeSC SRCB_CREDIT       = "上海农商银行(信用卡)"
renderBankCodeSC SJB_DEBIT         = "盛京银行(借记卡)"
renderBankCodeSC SDRCU_DEBIT       = "山东农信(借记卡)"
renderBankCodeSC SRCB_DEBIT        = "上海农商银行(借记卡)"
renderBankCodeSC SCNX_DEBIT        = "四川农信(借记卡)"
renderBankCodeSC QLB_DEBIT         = "齐鲁银行(借记卡)"
renderBankCodeSC QDCCB_DEBIT       = "青岛银行(借记卡)"
renderBankCodeSC PZHCCB_DEBIT      = "攀枝花银行(借记卡)"
renderBankCodeSC ZJTLCB_DEBIT      = "浙江泰隆银行(借记卡)"
renderBankCodeSC TJBHB_DEBIT       = "天津滨海农商行(借记卡)"
renderBankCodeSC WEB_DEBIT         = "微众银行(借记卡)"
renderBankCodeSC YNRCCB_DEBIT      = "云南农信(借记卡)"
renderBankCodeSC WFB_DEBIT         = "潍坊银行(借记卡)"
renderBankCodeSC WHRC_DEBIT        = "武汉农商行(借记卡)"
renderBankCodeSC ORDOSB_DEBIT      = "鄂尔多斯银行(借记卡)"
renderBankCodeSC XJRCCB_DEBIT      = "新疆农信银行(借记卡)"
renderBankCodeSC ORDOSB_CREDIT     = "鄂尔多斯银行(信用卡)"
renderBankCodeSC CSRCB_DEBIT       = "常熟农商银行(借记卡)"
renderBankCodeSC JSNX_DEBIT        = "江苏农商行(借记卡)"
renderBankCodeSC GRCB_CREDIT       = "广州农商银行(信用卡)"
renderBankCodeSC GLB_DEBIT         = "桂林银行(借记卡)"
renderBankCodeSC GDRCU_DEBIT       = "广东农信银行(借记卡)"
renderBankCodeSC GDHX_DEBIT        = "广东华兴银行(借记卡)"
renderBankCodeSC FJNX_DEBIT        = "福建农信银行(借记卡)"
renderBankCodeSC DYCCB_DEBIT       = "德阳银行(借记卡)"
renderBankCodeSC DRCB_DEBIT        = "东莞农商行(借记卡)"
renderBankCodeSC CZCB_DEBIT        = "稠州银行(借记卡)"
renderBankCodeSC CZB_DEBIT         = "浙商银行(借记卡)"
renderBankCodeSC CZB_CREDIT        = "浙商银行(信用卡)"
renderBankCodeSC GRCB_DEBIT        = "广州农商银行(借记卡)"
renderBankCodeSC CSCB_DEBIT        = "长沙银行(借记卡)"
renderBankCodeSC CQRCB_DEBIT       = "重庆农商银行(借记卡)"
renderBankCodeSC CBHB_DEBIT        = "渤海银行(借记卡)"
renderBankCodeSC BOIMCB_DEBIT      = "内蒙古银行(借记卡)"
renderBankCodeSC BOD_DEBIT         = "东莞银行(借记卡)"
renderBankCodeSC BOD_CREDIT        = "东莞银行(信用卡)"
renderBankCodeSC BOB_DEBIT         = "北京银行(借记卡)"
renderBankCodeSC BNC_DEBIT         = "江西银行(借记卡)"
renderBankCodeSC BJRCB_DEBIT       = "北京农商行(借记卡)"
renderBankCodeSC AE_CREDIT         = "AE(信用卡)"
renderBankCodeSC GYCB_CREDIT       = "贵阳银行(信用卡)"
renderBankCodeSC JSHB_DEBIT        = "晋商银行(借记卡)"
renderBankCodeSC JRCB_DEBIT        = "江阴农商行(借记卡)"
renderBankCodeSC JNRCB_DEBIT       = "江南农商(借记卡)"
renderBankCodeSC JLNX_DEBIT        = "吉林农信(借记卡)"
renderBankCodeSC JLB_DEBIT         = "吉林银行(借记卡)"
renderBankCodeSC JJCCB_DEBIT       = "九江银行(借记卡)"
renderBankCodeSC HXB_DEBIT         = "华夏银行(借记卡)"
renderBankCodeSC HXB_CREDIT        = "华夏银行(信用卡)"
renderBankCodeSC HUNNX_DEBIT       = "湖南农信(借记卡)"
renderBankCodeSC HSB_DEBIT         = "徽商银行(借记卡)"
renderBankCodeSC HSBC_DEBIT        = "恒生银行(借记卡)"
renderBankCodeSC HRXJB_DEBIT       = "华融湘江银行(借记卡)"
renderBankCodeSC HNNX_DEBIT        = "河南农信(借记卡)"
renderBankCodeSC HKBEA_DEBIT       = "东亚银行(借记卡)"
renderBankCodeSC HEBNX_DEBIT       = "河北农信(借记卡)"
renderBankCodeSC HBNX_DEBIT        = "湖北农信(借记卡)"
renderBankCodeSC HBNX_CREDIT       = "湖北农信(信用卡)"
renderBankCodeSC GYCB_DEBIT        = "贵阳银行(借记卡)"
renderBankCodeSC GSNX_DEBIT        = "甘肃农信(借记卡)"
renderBankCodeSC JCB_CREDIT        = "JCB(信用卡)"
renderBankCodeSC MASTERCARD_CREDIT = "MASTERCARD(信用卡)"
renderBankCodeSC VISA_CREDIT       = "VISA(信用卡)"
renderBankCodeSC CFT               = "财付通"

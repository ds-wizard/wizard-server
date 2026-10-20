module Shared.Constant.Mcp.KnowledgeModelEventExamples where

import Shared.Database.Migration.Development.KnowledgeModel.Data.Event.KnowledgeModelEvents
import Shared.Model.KnowledgeModel.Event.KnowledgeModelEvent

knowledgeModelEventExamples :: [KnowledgeModelEvent]
knowledgeModelEventExamples =
  [ a_km1
  , e_km1
  , a_km1_ch1
  , e_km1_ch1
  , d_km1_ch1
  , a_km1_ch1_q2
  , e_km1_ch1_q2
  , d_km1_ch1_q1
  , a_km1_ch1_q2_aNo1
  , e_km1_ch1_q2_aYes1
  , d_km1_ch1_q2_aYes1
  , a_km1_ch3_q11_cho1
  , e_km1_ch3_q11_cho1
  , d_km1_ch3_q11_cho1
  , a_km1_ch1_q2_eAlbert
  , e_km1_ch1_q2_eAlbert
  , d_km1_ch1_q2_eNikola
  , a_km1_ch1_q2_rCh2
  , e_km1_ch1_q2_rCh2
  , d_km1_ch1_q2_rCh2
  , a_km1_tds
  , e_km1_tds
  , d_km1_tds
  , a_km1_ir
  , e_km1_ir
  , d_km1_ir
  , a_km1_mtrF
  , e_km1_mtrF
  , d_km1_mtrF
  , a_km1_phs1
  , e_km1_phs1
  , d_km1_phs1
  , a_km1_rc1
  , e_km1_rc1
  , d_km1_rc1
  , a_km1_rc1_rp1
  , e_km1_rc1_rp1
  , d_km1_rc1_rp1
  , m_km1_ch1_q1__to_ch2
  , m_km1_ch1_q2_aYes__to_ch2_q3
  , m_km1_ch3_q11_cho1__to_ch3_q12
  , m_km1_ch1_q2_eAlbert__to_ch2_q3
  , m_km1_ch1_q2_r1__to_ch2_q3
  ]

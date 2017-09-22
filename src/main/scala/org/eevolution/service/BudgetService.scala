/**
  * Copyright (C) 2003-2017, e-Evolution Consultants S.A. , http://www.e-evolution.com
  * This program is free software: you can redistribute it and/or modify
  * it under the terms of the GNU General Public License as published by
  * the Free Software Foundation, either version 2 of the License, or
  * (at your option) any later version.
  * This program is distributed in the hope that it will be useful,
  * but WITHOUT ANY WARRANTY; without even the implied warranty of
  * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  * GNU General Public License for more details.
  * You should have received a copy of the GNU General Public License
  * along with this program.  If not, see <http://www.gnu.org/licenses/>.
  * Email: victor.perez@e-evolution.com, http://www.e-evolution.com , http://github.com/e-Evolution
  * Created by victor.perez@e-evolution.com , www.e-evolution.com
  */

package org.eevolution.service

import org.adempiere.model.ImportValidator
import org.adempiere.process.ImportProcess
import org.compiere.model.{MAccount, MJournalLine, X_I_Budget}
import org.compiere.util.Msg

/**
  * Budget service
  * Created by e-Evolution on 14/07/17.
  */
object BudgetService {

  /**
    * Import Budget Validator
    *
    * @param importProcess
    * @param importModel
    * @param importTarget
    * @param typeEvent
    */
  def importBudget(importProcess: ImportProcess, importModel: Object, importTarget: Object, typeEvent: Int): Unit = {
    val importBudget = importModel.asInstanceOf[X_I_Budget]
    if (ImportValidator.TIMING_BEFORE_VALIDATE == typeEvent) {
      val aliasOption = Option(importBudget.get_ValueAsString("Alias"))
      if (aliasOption.isDefined && !aliasOption.get.isEmpty) {
        val accountOption: Option[MAccount] = ValidCombinationService.getValidCombiantion(aliasOption.get)(importBudget.getCtx, importBudget.get_TrxName())
        if (accountOption.isDefined) {
          val account = accountOption.get
          importBudget.set_CustomColumn("BudgetValidCombination_ID", account.getC_ValidCombination_ID)
          importBudget.setAD_Org_ID(account.getAD_Org_ID)
          importBudget.setC_SubAcct_ID(account.getC_SubAcct_ID)
          importBudget.setM_Product_ID(account.getM_Product_ID)
          importBudget.setC_BPartner_ID(account.getC_BPartner_ID)
          importBudget.setAD_OrgTrx_ID(account.getAD_OrgTrx_ID)
          importBudget.setC_LocFrom_ID(account.getC_LocFrom_ID)
          importBudget.setC_LocTo_ID(account.getC_LocTo_ID)
          importBudget.setC_SalesRegion_ID(account.getC_SalesRegion_ID)
          importBudget.setC_Project_ID(account.getC_Project_ID)
          importBudget.setC_Campaign_ID(account.getC_Campaign_ID)
          importBudget.setC_Activity_ID(account.getC_Activity_ID)
          importBudget.setUser1_ID(account.getUser1_ID)
          importBudget.setUser2_ID(account.getUser2_ID)
          importBudget.setUser3_ID(account.getUser3_ID)
          importBudget.setUser4_ID(account.getUser4_ID)
          importBudget.setUserElement1_ID(account.getUserElement1_ID)
          importBudget.setUserElement2_ID(account.getUserElement2_ID)
          importBudget.saveEx
        }
        else {
          importBudget.setI_ErrorMsg(Msg.parseTranslation(importBudget.getCtx, s"ERR= @Alias@ @NotFound@ ${aliasOption.get},"))
          importBudget.setI_IsImported(false)
          importBudget.setProcessed(false)
        }
      }
    }
    if (ImportValidator.TIMING_AFTER_IMPORT == typeEvent) {
      val optionJournalLine = Option(importTarget.asInstanceOf[MJournalLine])
      if (optionJournalLine.isDefined) {
        val journalLine = optionJournalLine.get
        val budgetCodeOptionId = Option(importBudget.get_Value("BudgetValidCombination_ID").asInstanceOf[Integer])
        if (budgetCodeOptionId.isDefined && budgetCodeOptionId.get > 0) {
          journalLine.set_ValueOfColumn("BudgetValidCombination_ID", budgetCodeOptionId.get)
          journalLine.saveEx
        }
        importBudget.setC_ValidCombination_ID(-1)
        importBudget.saveEx
      }
    }
  }
}

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
import org.compiere.model._
import org.compiere.util.DB


/**
  * Journal Service
  * Created by e-Evolution on 11/06/17.
  */
object JournalService extends BudgetMandatory {


  /**
    * Model Validator after new
    *
    * @param po
    * @return
    */
  def afterNew(po: PO): String = po match {
    // validate budget dimension mandatory
    case _ if I_GL_JournalLine.Table_ID == po.get_Table_ID() => {
      checkBudgetValidCombination(po)
    }
    case _ => withoutErrors
  }

  /**
    * Check Budget Valid Combination
    *
    * @param po
    * @return
    */
  def checkBudgetValidCombination(po: PO): String = {
    val journalLine = po.asInstanceOf[MJournalLine]
    val optioElementValue = Option(DB.getSQLValueString(po.get_TrxName, "SELECT Value FROM C_ElementValue WHERE C_ElementValue_ID = ?", journalLine.getAccount_ID))
    if (optioElementValue.isDefined && (BudgetToBeExercised == optioElementValue.get || BudgetExecute == optioElementValue.get)) {
      if (journalLine.getAlias_ValidCombination_ID > 0) {
        journalLine.set_ValueOfColumn("BudgetValidCombination_ID", journalLine.getAlias_ValidCombination_ID)
        journalLine.saveEx
      }
    }
    else if (journalLine.get_ValueAsInt("BudgetValidCombination_ID") <= 0) {
      val alias = ValidCombinationService.getAlias(
        journalLine.getAD_Org_ID,
        journalLine.getAD_OrgTrx_ID,
        journalLine.getC_Activity_ID,
        journalLine.getC_Campaign_ID,
        journalLine.getC_Project_ID,
        journalLine.getUserElement1_ID,
        journalLine.getUserElement2_ID,
        journalLine.getUser1_ID,
        journalLine.getC_SalesRegion_ID,
        journalLine.getUser2_ID,
        journalLine.getUser3_ID,
        journalLine.getUser4_ID)(po.get_TrxName())
      val account = ValidCombinationService.getValidCombiantion(alias)(po.getCtx, po.get_TrxName())
      if (account.isDefined) {
        journalLine.set_ValueOfColumn("BudgetValidCombination_ID", account.get.getC_ValidCombination_ID)
        journalLine.saveEx
      }
    }
    withoutErrors
  }

  /**
    * Import Journal
    *
    * @param importProcess
    * @param importModel
    * @param importTarget
    * @param typeEvent
    */
  def importJournal(importProcess: ImportProcess, importModel: Object, importTarget: Object, typeEvent: Int): Unit = {
    if (ImportValidator.TIMING_AFTER_IMPORT == typeEvent) {
      val importJournal = importModel.asInstanceOf[X_I_GLJournal]
      val aliasOption = Option(importJournal.get_ValueAsString("Alias"))
      if (aliasOption.isDefined && !aliasOption.get.isEmpty) {
        val optionJournalLine = Option(importTarget.asInstanceOf[MJournalLine])
        if (optionJournalLine.isDefined) {
          val journalLine = optionJournalLine.get
          if (importJournal.getAccountValue != null) {
            val accountSchemaId = importJournal.getC_AcctSchema_ID
            val clientId = importJournal.getAD_Client_ID
            val elementValueValue = importJournal.getAccountValue
            val sql = new StringBuilder("SELECT ev.C_ElementValue_ID FROM C_ElementValue ev ")
            sql.append(" INNER JOIN C_Element e ON (e.C_Element_ID=ev.C_Element_ID)")
            sql.append(" INNER JOIN C_AcctSchema_Element ase ON (e.C_Element_ID=ase.C_Element_ID AND ase.ElementType='AC')")
            sql.append(s" WHERE ase.C_AcctSchema_ID = $accountSchemaId AND ev.AD_Client_ID=$clientId AND ev.Value ='$elementValueValue'")
            val elementValueId = DB.getSQLValueEx(importJournal.get_TrxName, sql.toString)
            if (elementValueId > 0) {
              val account = MAccount.get(importJournal.getCtx, importJournal.getC_ValidCombination_ID)
              journalLine.setAD_Org_ID(account.getAD_Org_ID)
              journalLine.setC_SubAcct_ID(account.getC_SubAcct_ID)
              journalLine.setM_Product_ID(account.getM_Product_ID)
              journalLine.setC_BPartner_ID(account.getC_BPartner_ID)
              journalLine.setAD_OrgTrx_ID(account.getAD_OrgTrx_ID)
              journalLine.setC_LocFrom_ID(account.getC_LocFrom_ID)
              journalLine.setC_LocTo_ID(account.getC_LocTo_ID)
              journalLine.setC_SalesRegion_ID(account.getC_SalesRegion_ID)
              journalLine.setC_Project_ID(account.getC_Project_ID)
              journalLine.setC_Campaign_ID(account.getC_Campaign_ID)
              journalLine.setC_Activity_ID(account.getC_Activity_ID)
              journalLine.setUser1_ID(account.getUser1_ID)
              journalLine.setUser2_ID(account.getUser2_ID)
              journalLine.setUser3_ID(account.getUser3_ID)
              journalLine.setUser4_ID(account.getUser4_ID)
              journalLine.setUserElement1_ID(account.getUserElement1_ID)
              journalLine.setUserElement2_ID(account.getUserElement2_ID)
              journalLine.setAccount_ID(elementValueId)
              if (importJournal.getC_ValidCombination_ID > 0)
                journalLine.set_ValueOfColumn("BudgetValidCombination_ID", importJournal.getC_ValidCombination_ID)

              val invoicePosition = importJournal.getDescription.indexOf(", ")
              if (invoicePosition > 0) {
                val invoiceDocumentNo = importJournal.getDescription.substring(invoicePosition + 2).trim
                val searchInvoiceSQL = "SELECT MAX(i.C_Invoice_ID) FROM C_Invoice i WHERE i.DocumentNo = ? AND C_BPartner_ID=?"
                val invoiceId = DB.getSQLValueEx(importJournal.get_TrxName, searchInvoiceSQL, invoiceDocumentNo , importJournal.getC_BPartner_ID.asInstanceOf[Integer])
                if (invoiceId > 0)
                  journalLine.set_ValueOfColumn("C_Invoice_ID", invoiceId)
              }
              journalLine.saveEx
              importJournal.setAccount_ID(elementValueId)
            }
            else {
              journalLine.deleteEx(true)
              importJournal.setGL_JournalBatch_ID(-1)
              importJournal.setGL_Journal_ID(-1)
              importJournal.setGL_JournalLine_ID(-1)
              importJournal.setI_ErrorMsg("ERR=Invalid Account,")
              importJournal.setI_IsImported(false)
              importJournal.setProcessed(false)
            }
            importJournal.setC_ValidCombination_ID(-1)
            importJournal.saveEx
          }
        }
      }
    }
  }
}

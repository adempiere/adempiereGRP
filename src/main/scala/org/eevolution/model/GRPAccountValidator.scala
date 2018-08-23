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

package org.eevolution.model

import java.util

import org.adempiere.model.ImportValidator
import org.adempiere.process.ImportProcess
import org.compiere.acct.Fact
import org.compiere.model._
import org.compiere.util.{CLogger, Env}
import org.eevolution.service._

/**
  * Created by e-Evolution on 22/04/17.
  */
class GRPAccountValidator extends ModelValidator with FactsValidator with ImportValidator {

  val logger: CLogger = CLogger.getCLogger(getClass)
  val clientId: Int = -1
  val withoutErrors = ""

  /**
    * GRP Budget Validation initialize method
    *
    * @param engine
    * @param client
    */
  def initialize(engine: ModelValidationEngine, client: MClient): Unit = {
    engine.addModelChange(I_C_ValidCombination.Table_Name, this)
    engine.addModelChange(I_C_BankStatementLine.Table_Name, this)
    engine.addModelChange(I_C_PaySelection.Table_Name, this)
    engine.addModelChange(I_GL_JournalLine.Table_Name, this)
    engine.addModelChange(I_C_Invoice.Table_Name, this)
    engine.addModelChange(I_C_InvoiceLine.Table_Name, this)
    engine.addModelChange(I_C_OrderLine.Table_Name, this)
    engine.addModelChange(I_M_RequisitionLine.Table_Name, this)

    engine.addFactsValidate(I_M_Requisition.Table_Name, this)
    engine.addFactsValidate(I_C_Order.Table_Name, this)
    engine.addFactsValidate(I_C_Invoice.Table_Name, this)
    engine.addFactsValidate(I_C_AllocationHdr.Table_Name, this)
    engine.addFactsValidate(I_C_BankStatement.Table_Name, this)
    engine.addFactsValidate(I_GL_Journal.Table_Name, this)

    engine.addDocValidate(I_M_Requisition.Table_Name, this)
    engine.addDocValidate(I_C_Order.Table_Name, this)
    engine.addDocValidate(I_C_Invoice.Table_Name, this)
    engine.addDocValidate(I_C_AllocationHdr.Table_Name, this)
    engine.addDocValidate(I_C_BankStatement.Table_Name, this)
    engine.addDocValidate(I_C_Payment.Table_Name, this)
    engine.addDocValidate(I_GL_Journal.Table_Name, this)

    engine.addImportValidate(I_I_GLJournal.Table_Name, this)
    engine.addImportValidate(I_I_Invoice.Table_Name, this)
    engine.addImportValidate(I_I_Budget.Table_Name, this)
  }

  /**
    * GRP Budget Validation Login method
    *
    * @param orgId
    * @param roleId
    * @param userId
    * @return
    */
  def login(orgId: Int, roleId: Int, userId: Int): String = {
    return withoutErrors
  }

  /**
    * GRP Budget Validation get client id method
    *
    * @return
    */
  def getAD_Client_ID: Int = {
    val clientId = Env.getAD_Client_ID(Env.getCtx)
    clientId
  }

  /**
    * GRP Budget Validation Model Change method
    *
    * @param po
    * @param typeEvent
    * @return
    */
  def modelChange(po: PO, typeEvent: Int): String = po match {
    // Update Valid Combination Alias
    case _ if I_C_ValidCombination.Table_ID == po.get_Table_ID
      && ModelValidator.TYPE_AFTER_NEW == typeEvent => ValidCombinationService.afterNew(po)
    case _ if I_M_RequisitionLine.Table_ID == po.get_Table_ID()
      && ModelValidator.TYPE_AFTER_NEW == typeEvent => RequisitionService.afterNewRequisitionLine(po.asInstanceOf[MRequisitionLine])
    case _ if I_M_RequisitionLine.Table_ID == po.get_Table_ID()
      && ModelValidator.TYPE_BEFORE_CHANGE == typeEvent => RequisitionService.beforeChangeRequisitionLine(po.asInstanceOf[MRequisitionLine])
    case _ if I_C_OrderLine.Table_ID == po.get_Table_ID()
      && ModelValidator.TYPE_AFTER_NEW == typeEvent => OrderService.afterNewOrderLine(po.asInstanceOf[MOrderLine])
    case _ if I_C_InvoiceLine.Table_ID == po.get_Table_ID()
      && ModelValidator.TYPE_AFTER_NEW == typeEvent => InvoiceService.afterNewInvoiceLine(po.asInstanceOf[MInvoiceLine])
    case _ if I_C_InvoiceLine.Table_ID == po.get_Table_ID()
      && ModelValidator.TYPE_BEFORE_CHANGE == typeEvent => InvoiceService.beforeChangeInvoiceLine(po.asInstanceOf[MInvoiceLine])
    case _ if I_C_BankStatementLine.Table_ID == po.get_Table_ID
      && ModelValidator.CHANGETYPE_CHANGE == typeEvent
      && po.is_ValueChanged(I_C_BankStatementLine.COLUMNNAME_C_Invoice_ID) => BankStatementService.updateStatementLine(po)
    case _ if I_C_PaySelection.Table_ID == po.get_Table_ID
      && ModelValidator.TYPE_BEFORE_CHANGE == typeEvent => PaySelectionService.closed(po.asInstanceOf[MPaySelection])
      withoutErrors
    case _ if I_GL_JournalLine.Table_ID == po.get_Table_ID
      && ModelValidator.TYPE_AFTER_NEW == typeEvent => JournalService.afterNew(po)
    case _ => withoutErrors
  }

  /**
    * GRP Budget Validation Document Validate method
    *
    * @param po
    * @param timing
    * @return
    */
  def docValidate(po: PO, timing: Int): String = po match {
    // Validate Budget Available before compelte a document
    case _ if ModelValidator.TIMING_BEFORE_COMPLETE == timing => FactAccountService.checkBudgetAvailable(po)
    // Validate reverse Payment
    case _ if I_C_Payment.Table_ID == po.get_Table_ID
      && ModelValidator.TIMING_BEFORE_REVERSECORRECT == timing => PaymentService.validateReverse(po.asInstanceOf[MPayment])
    // Validate reverse invoice
    case _ if ModelValidator.TIMING_BEFORE_REVERSECORRECT == timing => InvoiceService.validateReverse(po.asInstanceOf[MInvoice])
    // Syncronize Budget Combination Bank Statement with source Bank Statement
    case _ if I_C_BankStatement.Table_ID == po.get_Table_ID()
      && ModelValidator.TIMING_AFTER_REVERSECORRECT == timing => BankStatementService.reverse(po)
      withoutErrors
    // Validate that the Journal Line the mandatory budget mandatory dimensions
    /*case _ if I_GL_Journal.Table_ID == po.get_Table_ID()
      && ModelValidator.TIMING_BEFORE_PREPARE == timing  => {
        val journal = po.asInstanceOf[MJournal]
        journal.getLines(true).foreach( journalLine => ValidCombinationService.checkMandatoryDimension(journalLine))
      }
      withoutErrors
    */
    case _ => withoutErrors
  }

  /**
    * GRP Budget Validation fact Post Account
    *
    * @param accountSchema
    * @param accountingFacts
    * @param po
    * @return
    */
  def factsValidate(accountSchema: MAcctSchema, accountingFacts: util.List[Fact], po: PO): String = {
    // Fill Budget Dimension based on Budget Valid Combination
    FactAccountService.updateDimension(accountSchema, accountingFacts, po)
    // Create Reserve
    if (I_M_Requisition.Table_ID == po.get_Table_ID && accountingFacts != null && accountingFacts.size() > 0)
      FactAccountService.generateBudgetAccountReserved(po.asInstanceOf[MRequisition] , accountSchema , accountingFacts)
    // Create Commitment
    if (I_C_Order.Table_ID == po.get_Table_ID && accountingFacts != null && accountingFacts.size() > 0)
      FactAccountService.generateBudgetAccountCommitment(po.asInstanceOf[MOrder] , accountSchema , accountingFacts)
    // Move the account tax facts to expense account
    if (I_C_Invoice.Table_ID == po.get_Table_ID && accountingFacts != null && accountingFacts.size() > 0)
      FactAccountService.changeTaxAccount(accountSchema, accountingFacts, po.asInstanceOf[Invoice])
    // Generate the budget account Exercised and Paid based on Allocation
    if (I_C_AllocationHdr.Table_ID == po.get_Table_ID && accountingFacts != null && accountingFacts.size() > 0) {
      FactAccountService.generateBudgetAccountExercised(po.asInstanceOf[MAllocationHdr], accountSchema.get_ID())
      FactAccountService.generateBudgetAccountPaid(po.asInstanceOf[MAllocationHdr], accountSchema.get_ID())
    }
    withoutErrors
  }

  /**
    * Change of account before of set account dimension  based on valid combination.
    *
    * @param importProcess
    * @param importModel
    * @param importTarget
    * @param typeEvent
    */
  def validate(importProcess: ImportProcess, importModel: Object, importTarget: Object, typeEvent: Int): Unit = {
    // Apply the import journal logic for Budget functionality
    if (importModel.isInstanceOf[X_I_GLJournal])
      JournalService.importJournal(importProcess, importModel, importTarget, typeEvent)
    // Apply the import invoice logic  for Budget Functionality
    if (importModel.isInstanceOf[X_I_Invoice])
      InvoiceService.importInvoice(importProcess, importModel, importTarget, typeEvent)
    // Apply the import budget logic for Budget Functionality
    if (importModel.isInstanceOf[X_I_Budget])
      BudgetService.importBudget(importProcess, importModel, importTarget, typeEvent)
  }
}
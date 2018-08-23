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
import org.compiere.process.DocAction

/**
  * Invoice Service
  * Created by e-Evolution on 11/06/17.sbt copie
  */
object InvoiceService {

  /**
    * Validate After New Invoice Line
    *
    * @param invoiceLine
    * @return
    */
  def afterNewInvoiceLine(invoiceLine: MInvoiceLine): String = {
    val orderLineOption = Option(invoiceLine.getC_OrderLine().asInstanceOf[MOrderLine])
    orderLineOption.foreach(orderLine => {
        val accountId = ValidCombinationService.getBudgetValidCombination(orderLine)
        if (accountId > 0) {
          ValidCombinationService.setBudgetValidCombination(invoiceLine, accountId)
          ValidCombinationService.fillDimension(accountId, invoiceLine)(invoiceLine.getCtx, invoiceLine.get_TrxName())
        }
    })

    val accountId = ValidCombinationService.getBudgetValidCombination(invoiceLine)
    if (accountId > 0) ValidCombinationService.fillDimension(accountId, invoiceLine)(invoiceLine.getCtx, invoiceLine.get_TrxName())

    return withoutErrors
  }

  /**
    * Validate Before Change Invoice Line
    *
    * @param invoiceLine
    * @return
    */
  def beforeChangeInvoiceLine(invoiceLine: MInvoiceLine): String = {
    /*val orderLineOption = Option(invoiceLine.getC_OrderLine().asInstanceOf[MOrderLine])
    orderLineOption.foreach(orderLine => {
      // todo remove legacy code
      val accountId1 = orderLine.get_ValueAsInt("C_ValidCombination_ID")
      if (accountId1 > 0) {
        invoiceLine.set_ValueOfColumn("C_ValidCombination_ID", accountId1)
        ValidCombinationService.fillDimension(accountId1, invoiceLine)(invoiceLine.getCtx, invoiceLine.get_TrxName())
      }
      val accountId2 = orderLine.get_ValueAsInt("BudgetValidCombination_ID")
      if (accountId2 > 0) {
        invoiceLine.set_ValueOfColumn("BudgetValidCombination_ID", accountId2)
        ValidCombinationService.fillDimension(accountId2, invoiceLine)(invoiceLine.getCtx, invoiceLine.get_TrxName())
      }
    })*/
    return withoutErrors
  }

  /**
    * Validate Import Invoice
    *
    * @param importProcess
    * @param importModel
    * @param importTarget
    * @param typeEvent
    */
  def importInvoice(importProcess: ImportProcess, importModel: Object, importTarget: Object, typeEvent: Int): Unit = {
    if (ImportValidator.TIMING_AFTER_IMPORT == typeEvent) {
      val importInvoice = importModel.asInstanceOf[X_I_Invoice]
      implicit val context = importInvoice.getCtx
      implicit val trxName = importInvoice.get_TrxName()
      val aliasOption = Option(importInvoice.get_ValueAsString("Alias"))
      if (aliasOption.isDefined) {
        val optionInvoiceLine = Option(importTarget.asInstanceOf[MInvoiceLine])
        if (optionInvoiceLine.isDefined) {
          val invoiceLine = optionInvoiceLine.get
          val optionAccount: Option[MAccount] = ValidCombinationService.getValidCombiantion(aliasOption.get)
          if (optionAccount.isDefined) {
            val account = optionAccount.get
            ValidCombinationService.fillDimension(account.get_ID(), invoiceLine)(importProcess.getCtx, importProcess.get_TrxName())
          }
        }
        else {
          optionInvoiceLine.get.deleteEx(true)
          importInvoice.setC_InvoiceLine_ID(-1)
          importInvoice.setC_Invoice_ID(-1)
          importInvoice.setI_ErrorMsg("ERR=Invalid Account,")
          importInvoice.setI_IsImported(false)
          importInvoice.setProcessed(false)
        }
      }
    }
  }

  /**
    * Validation Reverse
    *
    * @param invoice
    * @return
    */
  def validateReverse(invoice: MInvoice): String = {
    //Validate invoice not reverse if is paid
    if (invoice.isPaid) {
      invoice.setDocAction(DocAction.ACTION_Close)
      return "@C_Invoice_ID@ @IsPaid@ @WorflowNotValid@"
    }
    //Validate invoice mot reverse if exist in pay selection
    val whereClause = new StringBuilder
    whereClause.append(I_C_PaySelectionLine.COLUMNNAME_C_Invoice_ID).append("=?")
    val paySelectionLine: MPaySelectionLine = new Query(invoice.getCtx, I_C_PaySelectionLine.Table_Name, whereClause.toString(), invoice.get_TrxName())
      .setClient_ID()
      .setParameters(invoice.getC_Invoice_ID.asInstanceOf[Integer])
      .first()
    if (paySelectionLine != null && paySelectionLine.get_ID() > 0) {
      invoice.setDocAction(DocAction.ACTION_Close)
      return s"@C_Invoice_ID@ @To@ @C_PaySelection_ID@ ${paySelectionLine.getParent.getDocumentInfo} @WorflowNotValid@"
    }
    withoutErrors
  }
}

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

import java.math.BigDecimal
import java.sql.Timestamp
import java.util

import org.compiere.acct.{Fact, FactLine}
import org.compiere.model.{I_Fact_Acct, _}
import org.compiere.util.DB

import scala.collection.JavaConversions._


/**
  * Fact Account Service
  * Created by e-Evolution on 08/04/17.
  */
object FactAccountService {

  /**
    * Update dimension for for a document
    *
    * @param accountSchema
    * @param accountingFacts
    * @param po
    * @return
    */
  def updateDimension(accountSchema: AccountSchema, accountingFacts: util.List[Fact], po: PO): String = {
    // get list fact account lines
    accountingFacts.foreach(accountingFact => {
      val tableId = accountingFact.getDocument.get_Table_ID()
      accountingFact.getLines.foreach(factLine => {
        val validCombinationId = getBudgetCombinationId(po, factLine.getLine_ID)
        if (validCombinationId > 0) {
          val account = new MAccount(po.getCtx, validCombinationId, po.get_TrxName())
          if (account != null && account.get_ID > 0) {
            factLine.setAD_OrgTrx_ID(account.getAD_OrgTrx_ID)
            factLine.setC_Project_ID(account.getC_Project_ID)
            factLine.setC_Campaign_ID(account.getC_Campaign_ID)
            factLine.setC_Activity_ID(account.getC_Activity_ID)
            factLine.setC_SalesRegion_ID(account.getC_SalesRegion_ID)
            factLine.setUser1_ID(account.getUser1_ID)
            factLine.setUser2_ID(account.getUser2_ID)
            factLine.setUser3_ID(account.getUser3_ID)
            factLine.setUser4_ID(account.getUser4_ID)
            factLine.setUserElement1_ID(account.getUserElement1_ID)
            factLine.setUserElement2_ID(account.getUserElement2_ID)
            factLine.saveEx
          }
        }
      })
    })
    withoutErrors
  }

  /**
    * Get Budget Combination Id
    *
    * @param document
    * @param documentLineId
    * @return
    */
  def getBudgetCombinationId(document: PO, documentLineId: Int): Int = {
    if (I_C_Invoice.Table_ID == document.get_Table_ID)
      return DB.getSQLValue(document.get_TrxName, s"SELECT C_ValidCombination_ID FROM C_InvoiceLine il WHERE il.C_Invoice_ID=${document.get_ID} AND il.C_InvoiceLine_ID=$documentLineId")
    if (I_GL_Journal.Table_ID == document.get_Table_ID)
      return DB.getSQLValue(document.get_TrxName, s"SELECT BudgetValidCombination_ID FROM GL_JournalLine jl WHERE jl.GL_Journal_ID=${document.get_ID} AND jl.GL_JournalLine_ID=$documentLineId")
    if (I_C_BankStatement.Table_ID == document.get_Table_ID)
      return DB.getSQLValue(document.get_TrxName, s"SELECT C_ValidCombination_ID FROM C_BankStatementLine bsl WHERE bsl.C_BankStatement_ID=${document.get_ID} AND bsl.C_BankStatementLine_ID=$documentLineId")
    return -1
  }

  /**
    * Apply Budget Paid
    *
    * @param accountSchema
    * @param accountingFacts
    * @param po
    */
  def applyBudgetPaid(accountSchema: AccountSchema, accountingFacts: util.List[Fact], po: PO) {
    if (po.isInstanceOf[MAllocationHdr]) {
      val allocation: MAllocationHdr = po.asInstanceOf[MAllocationHdr]
      val optionAllocationLines = Option(allocation.getLines(true))
      if (optionAllocationLines.isDefined) {
        optionAllocationLines.get.foreach(allocationLine => {
          if (allocationLine.getC_Invoice_ID > 0) {
            generateBudgetPaid(allocationLine)
          }
        })
      }
    }

    def generateBudgetPaid(allocationLine: MAllocationLine): Unit = {
      val where = new StringBuilder()
      where.append()
    }
  }


  /**
    * Move of account fact from tax to expense.
    *
    * @param accountSchema
    * @param accountingFacts
    * @param invoice
    * @return
    */
  def changeTaxAccount(accountSchema: AccountSchema, accountingFacts: util.List[Fact], invoice: Invoice): String = {
    invoice.getLines(true).foreach(invoiceLine => {
      accountingFacts.foreach(accoutingFact => {
        accoutingFact.getLines
          .filter(accoutingFactLine => accoutingFactLine != null && accoutingFactLine.getLine_ID == invoiceLine.get_ID)
          .foreach(accoutingFactLine => {
            val optionTax = Option(invoiceLine.getC_Tax.asInstanceOf[Tax])
            if (optionTax.isDefined && optionTax.get.getParent_Tax_ID > 0) {
              val optionChildTaxes = Option(optionTax.get.getChildTaxes(true))
              if (optionChildTaxes.isDefined) {
                optionChildTaxes.get.foreach(taxChild => {
                  if (taxChild.getRate.signum() > 0)
                    createFacline(invoiceLine, accoutingFact, accoutingFactLine, taxChild)
                })
              }
            }
            else {
              if (optionTax.get.getRate.signum() > 0)
                createFacline(invoiceLine, accoutingFact, accoutingFactLine, optionTax.get)
            }
          })
      })
    })

    /**
      * Create Fact Line
      *
      * @param invoiceLine
      * @param accoutingFact
      * @param accoutingFactLine
      * @param tax
      */
    def createFacline(invoiceLine: InvoiceLine, accoutingFact: Fact, accoutingFactLine: FactLine, tax: MTax): Unit = {
      val taxAccountCredit: TaxAccount = getAccoutTax(accountSchema, tax.getC_Tax_ID)
      if (taxAccountCredit == null)
        return
      val taxAccount = MAccount.get(invoice.getCtx, taxAccountCredit.getT_Credit_Acct)
      val taxAmount = if (invoiceLine.getC_Tax_ID == tax.get_ID()) invoiceLine.getTaxAmt else invoiceLine.getLineNetAmt.multiply(tax.getRate).divide(new BigDecimal(100), BigDecimal.ROUND_HALF_UP)
      val expenseFactLine = accoutingFact.createLine(accoutingFactLine.getDocLine, accoutingFactLine.getAccount, invoice.getC_Currency_ID, taxAmount, BigDecimal.ZERO)
      val taxFactLine = accoutingFact.createLine(accoutingFactLine.getDocLine, taxAccount, invoice.getC_Currency_ID, BigDecimal.ZERO, taxAmount)
      expenseFactLine.setDocumentInfo(accoutingFact.getDocument, accoutingFactLine.getDocLine)
      expenseFactLine.setLine_ID(accoutingFactLine.getLine_ID)
      expenseFactLine.setAD_OrgTrx_ID(accoutingFactLine.getAD_OrgTrx_ID)
      expenseFactLine.setC_Activity_ID(accoutingFactLine.getC_Activity_ID)
      expenseFactLine.setC_Campaign_ID(accoutingFactLine.getC_Campaign_ID)
      expenseFactLine.setC_Project_ID(accoutingFactLine.getC_Project_ID)
      expenseFactLine.setC_SalesRegion_ID(accoutingFactLine.getC_SalesRegion_ID)
      expenseFactLine.setUser1_ID(accoutingFactLine.getUser1_ID)
      expenseFactLine.setUser2_ID(accoutingFactLine.getUser2_ID)
      expenseFactLine.setUser3_ID(accoutingFactLine.getUser3_ID)
      expenseFactLine.setUser4_ID(accoutingFactLine.getUser4_ID)
      expenseFactLine.setUserElement1_ID(accoutingFactLine.getUserElement1_ID)
      expenseFactLine.setUserElement2_ID(accoutingFactLine.getUserElement2_ID)
      expenseFactLine.save
      taxFactLine.setDocumentInfo(accoutingFact.getDocument, accoutingFactLine.getDocLine)
      taxFactLine.save()
    }

    withoutErrors
  }

  /**
    * Generate Budget Account Exercised
    *
    * @param allocation
    * @param accountSchemaId
    */
  def generateBudgetAccountExercised(allocation: Allocation, accountSchemaId: Int): Unit = {
    val allocationLines = allocation.getLines(true)
    if (allocationLines != null && allocationLines.size > 0) {
      allocationLines.filter(allocationLine => allocationLine.getC_Invoice_ID > 0).foreach(allocationLine => {
        val invoice = allocationLine.getC_Invoice.asInstanceOf[Invoice]
        // Create budget based on invoice
        val accountInvoiceFacts: List[MFactAcct] = getAccountFact(invoice, accountSchemaId, X_Fact_Acct.POSTINGTYPE_Budget, BudgetAccrual)
        accountInvoiceFacts.foreach(accountFact => {
          //Debit Exercised
          val optionExercisedAccount = getAccount(invoice, accountSchemaId, BudgetExercised)
          if (optionExercisedAccount.isDefined) {
            val paymentAmount = allocationLine.getAmount.negate()
            val budgetAmount = accountFact.getAmtSourceDr.divide(invoice.getGrandTotal(), 12, BigDecimal.ROUND_HALF_UP).multiply(paymentAmount).setScale(2, BigDecimal.ROUND_HALF_UP)
            val periodId = MPeriod.getC_Period_ID(allocation.getCtx, allocation.getDateAcct, allocation.getAD_Org_ID)
            val debitExercisedFact = new MFactAcct(invoice.getCtx, 0, invoice.get_TrxName())
            PO.copyValues(accountFact, debitExercisedFact)
            debitExercisedFact.setDateAcct(allocation.getDateAcct)
            debitExercisedFact.setC_Period_ID(periodId)
            debitExercisedFact.setAD_Org_ID(invoice.getAD_Org_ID)
            debitExercisedFact.setAccount_ID(optionExercisedAccount.get.getC_ElementValue_ID)
            debitExercisedFact.setAD_Table_ID(allocation.get_Table_ID())
            debitExercisedFact.setRecord_ID(allocation.getC_AllocationHdr_ID)
            debitExercisedFact.setAmtSourceDr(budgetAmount)
            debitExercisedFact.setAmtSourceCr(BigDecimal.ZERO)
            debitExercisedFact.setAmtAcctDr(budgetAmount)
            debitExercisedFact.setAmtAcctCr(BigDecimal.ZERO)
            debitExercisedFact.saveEx
            //Credit Accrual
            val creditAccrualFact = new MFactAcct(invoice.getCtx, 0, invoice.get_TrxName())
            PO.copyValues(accountFact, creditAccrualFact)
            creditAccrualFact.setDateAcct(allocation.getDateAcct)
            creditAccrualFact.setC_Period_ID(periodId)
            creditAccrualFact.setAD_Org_ID(invoice.getAD_Org_ID)
            creditAccrualFact.setAD_Table_ID(allocation.get_Table_ID())
            creditAccrualFact.setRecord_ID(allocation.getC_AllocationHdr_ID)
            creditAccrualFact.setAmtSourceDr(BigDecimal.ZERO)
            creditAccrualFact.setAmtSourceCr(budgetAmount)
            creditAccrualFact.setAmtAcctDr(BigDecimal.ZERO)
            creditAccrualFact.setAmtAcctCr(budgetAmount)
            creditAccrualFact.saveEx
          }
        })

        // Based on Jorunal
        val accountJournalFacts: List[MFactAcct] = getAccountFactBasedOnInvoice(invoice, accountSchemaId, X_Fact_Acct.POSTINGTYPE_Budget, BudgetAccrual)
        accountJournalFacts.foreach(accountFact => {
          //Debit Exercised
          val optionExercisedAccount = getAccount(invoice, accountSchemaId, BudgetExercised)
          if (optionExercisedAccount.isDefined) {
            val paymentAmount = allocationLine.getAmount.negate()
            val budgetAmount = accountFact.getAmtSourceDr.divide(invoice.getGrandTotal(), 12, BigDecimal.ROUND_HALF_UP).multiply(paymentAmount).setScale(2, BigDecimal.ROUND_HALF_UP)
            val periodId = MPeriod.getC_Period_ID(allocation.getCtx, allocation.getDateAcct, allocation.getAD_Org_ID)
            val debitExercisedFact = new MFactAcct(invoice.getCtx, 0, invoice.get_TrxName())
            PO.copyValues(accountFact, debitExercisedFact)
            debitExercisedFact.setDateAcct(allocation.getDateAcct)
            debitExercisedFact.setC_Period_ID(periodId)
            debitExercisedFact.setAD_Org_ID(invoice.getAD_Org_ID)
            debitExercisedFact.setAccount_ID(optionExercisedAccount.get.getC_ElementValue_ID)
            debitExercisedFact.setAD_Table_ID(allocation.get_Table_ID())
            debitExercisedFact.setRecord_ID(allocation.getC_AllocationHdr_ID)
            debitExercisedFact.setAmtSourceDr(budgetAmount)
            debitExercisedFact.setAmtSourceCr(BigDecimal.ZERO)
            debitExercisedFact.setAmtAcctDr(budgetAmount)
            debitExercisedFact.setAmtAcctCr(BigDecimal.ZERO)
            debitExercisedFact.saveEx
            //Credit Accrual
            val creditAccrualFact = new MFactAcct(invoice.getCtx, 0, invoice.get_TrxName())
            PO.copyValues(accountFact, creditAccrualFact)
            creditAccrualFact.setDateAcct(allocation.getDateAcct)
            creditAccrualFact.setC_Period_ID(periodId)
            creditAccrualFact.setAD_Org_ID(invoice.getAD_Org_ID)
            creditAccrualFact.setAD_Table_ID(allocation.get_Table_ID())
            creditAccrualFact.setRecord_ID(allocation.getC_AllocationHdr_ID)
            creditAccrualFact.setAmtSourceDr(BigDecimal.ZERO)
            creditAccrualFact.setAmtSourceCr(budgetAmount)
            creditAccrualFact.setAmtAcctDr(BigDecimal.ZERO)
            creditAccrualFact.setAmtAcctCr(budgetAmount)
            creditAccrualFact.saveEx
          }
        })
      })
    }
  }

  /**
    * Generate Budget Account Exercised
    *
    * @param allocation
    * @param accountSchemaId
    */
  def generateBudgetAccountPaid(allocation: Allocation, accountSchemaId: Int): Unit = {
    val allocationLines = allocation.getLines(true)
    if (allocationLines != null && allocationLines.size > 0) {
      allocationLines.filter(allocationLine => allocationLine.getC_Invoice_ID > 0).foreach(allocationLine => {
        val invoice = allocationLine.getC_Invoice.asInstanceOf[Invoice]
        // Create budget based on invoice
        val accountInvoiceFacts: List[MFactAcct] = getAccountFact(invoice, accountSchemaId, X_Fact_Acct.POSTINGTYPE_Budget, BudgetAccrual)
        accountInvoiceFacts.foreach(accountFact => {
          //Debit Exercised
          val optionPaidAccount = getAccount(invoice, accountSchemaId, BudgetPaid)
          if (optionPaidAccount.isDefined) {
            val paymentAmount = allocationLine.getAmount.negate()
            val budgetAmount = accountFact.getAmtSourceDr.divide(invoice.getGrandTotal(), 12, BigDecimal.ROUND_HALF_UP).multiply(paymentAmount).setScale(2, BigDecimal.ROUND_HALF_UP)
            val periodId = MPeriod.getC_Period_ID(allocation.getCtx, allocation.getDateAcct, allocation.getAD_Org_ID)
            val debitPaidFact = new MFactAcct(invoice.getCtx, 0, invoice.get_TrxName())
            PO.copyValues(accountFact, debitPaidFact)
            debitPaidFact.setDateAcct(allocation.getDateAcct)
            debitPaidFact.setC_Period_ID(periodId)
            debitPaidFact.setAD_Org_ID(invoice.getAD_Org_ID)
            debitPaidFact.setAccount_ID(optionPaidAccount.get.getC_ElementValue_ID)
            debitPaidFact.setAD_Table_ID(allocation.get_Table_ID())
            debitPaidFact.setRecord_ID(allocation.getC_AllocationHdr_ID)
            debitPaidFact.setAmtSourceDr(budgetAmount)
            debitPaidFact.setAmtSourceCr(BigDecimal.ZERO)
            debitPaidFact.setAmtAcctDr(budgetAmount)
            debitPaidFact.setAmtAcctCr(BigDecimal.ZERO)
            debitPaidFact.saveEx

            //Credit Exercised
            val optionExercisedAccount = getAccount(invoice, accountSchemaId, BudgetExercised)
            val creditExercisedFact = new MFactAcct(invoice.getCtx, 0, invoice.get_TrxName())
            PO.copyValues(accountFact, creditExercisedFact)
            creditExercisedFact.setDateAcct(allocation.getDateAcct)
            creditExercisedFact.setC_Period_ID(periodId)
            creditExercisedFact.setAD_Org_ID(invoice.getAD_Org_ID)
            creditExercisedFact.setAccount_ID(optionExercisedAccount.get.getC_ElementValue_ID)
            creditExercisedFact.setAD_Table_ID(allocation.get_Table_ID())
            creditExercisedFact.setRecord_ID(allocation.getC_AllocationHdr_ID)
            creditExercisedFact.setAmtSourceDr(BigDecimal.ZERO)
            creditExercisedFact.setAmtSourceCr(budgetAmount)
            creditExercisedFact.setAmtAcctDr(BigDecimal.ZERO)
            creditExercisedFact.setAmtAcctCr(budgetAmount)
            creditExercisedFact.saveEx
          }
        })

        // Based on Jorunal
        val accountJournalFacts: List[MFactAcct] = getAccountFactBasedOnInvoice(invoice, accountSchemaId, X_Fact_Acct.POSTINGTYPE_Budget, BudgetAccrual)
        accountJournalFacts.foreach(accountFact => {
          //Debit Exercised
          val optionPaidAccount = getAccount(invoice, accountSchemaId, BudgetPaid)
          if (optionPaidAccount.isDefined) {
            val paymentAmount = allocationLine.getAmount.negate()
            val budgetAmount = accountFact.getAmtSourceDr.divide(invoice.getGrandTotal(), 12, BigDecimal.ROUND_HALF_UP).multiply(paymentAmount).setScale(2, BigDecimal.ROUND_HALF_UP)
            val periodId = MPeriod.getC_Period_ID(allocation.getCtx, allocation.getDateAcct, allocation.getAD_Org_ID)
            val debitPaidFact = new MFactAcct(invoice.getCtx, 0, invoice.get_TrxName())
            PO.copyValues(accountFact, debitPaidFact)
            debitPaidFact.setDateAcct(allocation.getDateAcct)
            debitPaidFact.setC_Period_ID(periodId)
            debitPaidFact.setAD_Org_ID(invoice.getAD_Org_ID)
            debitPaidFact.setAccount_ID(optionPaidAccount.get.getC_ElementValue_ID)
            debitPaidFact.setAD_Table_ID(allocation.get_Table_ID())
            debitPaidFact.setRecord_ID(allocation.getC_AllocationHdr_ID)
            debitPaidFact.setAmtSourceDr(budgetAmount)
            debitPaidFact.setAmtSourceCr(BigDecimal.ZERO)
            debitPaidFact.setAmtAcctDr(budgetAmount)
            debitPaidFact.setAmtAcctCr(BigDecimal.ZERO)
            debitPaidFact.saveEx
            //Credit Accrual
            val optionExercisedAccount = getAccount(invoice, accountSchemaId, BudgetExercised)
            val creditAExercisedFact = new MFactAcct(invoice.getCtx, 0, invoice.get_TrxName())
            PO.copyValues(accountFact, creditAExercisedFact)
            creditAExercisedFact.setDateAcct(allocation.getDateAcct)
            creditAExercisedFact.setC_Period_ID(periodId)
            creditAExercisedFact.setAD_Org_ID(invoice.getAD_Org_ID)
            creditAExercisedFact.setAD_Table_ID(allocation.get_Table_ID())
            creditAExercisedFact.setRecord_ID(allocation.getC_AllocationHdr_ID)
            creditAExercisedFact.setAccount_ID(optionExercisedAccount.get.getC_ElementValue_ID)
            creditAExercisedFact.setAmtSourceDr(BigDecimal.ZERO)
            creditAExercisedFact.setAmtSourceCr(budgetAmount)
            creditAExercisedFact.setAmtAcctDr(BigDecimal.ZERO)
            creditAExercisedFact.setAmtAcctCr(budgetAmount)
            creditAExercisedFact.saveEx
          }
        })
      })
    }
  }

  /**
    * Get Account
    *
    * @param po
    * @param accountSchemaId
    * @param accountValue
    * @return
    */
  def getAccount(po: PO, accountSchemaId: Int, accountValue: String): Option[MElementValue] = {
    val where = new StringBuilder()
    where.append("EXISTS (SELECT 1 FROM C_ElementValue ev , C_AcctSchema_Element ae WHERE ev.C_ElementValue_ID=C_ElementValue.C_ElementValue_ID AND ")
    where.append("ev.C_Element_ID=ae.C_Element_ID AND ae.ElementType='AC' AND ae.C_AcctSchema_ID=? AND ev.IsActive='Y' AND ev.Value=?)")
    val account: MElementValue = new Query(po.getCtx, I_C_ElementValue.Table_Name, where.toString(), po.get_TrxName())
      .setClient_ID()
      .setParameters(accountSchemaId.asInstanceOf[Integer], accountValue)
      .first.asInstanceOf[MElementValue]
    Option(account)
  }

  /**
    * Get Account Fact
    *
    * @param po
    * @param accountSchemaId
    * @param postingType
    * @param accountValue
    * @return
    */
  def getAccountFact(po: PO, accountSchemaId: Int, postingType: String, accountValue: String): List[MFactAcct] = {
    val where = new StringBuilder()
    where.append(I_Fact_Acct.COLUMNNAME_AD_Table_ID).append("=? AND ")
    where.append(I_Fact_Acct.COLUMNNAME_Record_ID).append("=? AND ")
    where.append(I_Fact_Acct.COLUMNNAME_PostingType).append("=? AND ")
    where.append("EXISTS (SELECT 1 FROM C_ElementValue ev , C_AcctSchema_Element ae WHERE ev.C_ElementValue_ID=Fact_Acct.Account_ID AND ")
    where.append("ev.C_Element_ID=ae.C_Element_ID AND ae.ElementType='AC' AND ae.C_AcctSchema_ID=? AND ev.IsActive='Y' AND ev.Value=?)")
    val factsAccount = new Query(po.getCtx, I_Fact_Acct.Table_Name, where.toString(), po.get_TrxName())
      .setClient_ID()
      .setParameters(
        po.get_Table_ID.asInstanceOf[Integer],
        po.get_ID.asInstanceOf[Integer],
        postingType,
        accountSchemaId.asInstanceOf[Integer],
        accountValue)
      .list.toList
    factsAccount
  }

  /**
    * get Account Fact for an Invoice
    *
    * @param invoice
    * @param accountSchemaId
    * @param postingType
    * @param accountValue
    * @return
    */
  def getAccountFactBasedOnInvoice(invoice: Invoice, accountSchemaId: Int, postingType: String, accountValue: String): List[MFactAcct] = {
    val where = new StringBuilder()
    where.append(I_Fact_Acct.COLUMNNAME_AD_Table_ID).append("=? AND ")
    where.append(I_Fact_Acct.COLUMNNAME_PostingType).append("=? AND ")
    where.append("EXISTS (SELECT 1 FROM GL_Journal j INNER JOIN GL_JournalLine jl ON (jl.GL_Journal_ID = j.GL_Journal_ID) WHERE j.GL_Journal_ID = Fact_Acct.Record_ID AND jl.GL_JournalLine_ID=Fact_Acct.Line_ID AND jl.C_Invoice_ID = ? ) AND ")
    where.append("EXISTS (SELECT 1 FROM C_ElementValue ev , C_AcctSchema_Element ae WHERE ev.C_ElementValue_ID=Fact_Acct.Account_ID AND ")
    where.append("ev.C_Element_ID=ae.C_Element_ID AND ae.ElementType='AC' AND ae.C_AcctSchema_ID=? AND ev.IsActive='Y' AND ev.Value=?)")
    val factAccount = new Query(invoice.getCtx, I_Fact_Acct.Table_Name, where.toString(), invoice.get_TrxName())
      .setClient_ID()
      .setParameters(
        I_GL_Journal.Table_ID.asInstanceOf[Integer],
        postingType,
        invoice.get_ID.asInstanceOf[Integer],
        accountSchemaId.asInstanceOf[Integer],
        accountValue)
      .list.toList
    factAccount
  }

  /**
    * Get Account Tax based on tax id
    *
    * @param accountSchema
    * @param taxId
    * @return
    */
  def getAccoutTax(accountSchema: AccountSchema, taxId: Int): TaxAccount = {
    val whereClause = new StringBuilder
    whereClause.append(I_C_Tax_Acct.COLUMNNAME_C_AcctSchema_ID).append("=? AND ")
    whereClause.append(I_C_Tax_Acct.COLUMNNAME_C_Tax_ID).append("=?")
    val taxAccount: TaxAccount = new Query(accountSchema.getCtx, I_C_Tax_Acct.Table_Name, whereClause.toString(), accountSchema.get_TrxName())
      .setClient_ID()
      .setParameters(accountSchema.getC_AcctSchema_ID.asInstanceOf[Integer], taxId.asInstanceOf[Integer])
      .first()
    taxAccount
  }

  /**
    * Get Balance By Alias
    *
    * @param accountSchemaId
    * @param alias
    * @param postingType
    * @param accountValue
    * @param dateFrom
    * @param dateTo
    * @param ctx
    * @param trxName
    * @return
    */
  def getBalanceByAlias(accountSchemaId: Int, alias: String, postingType: String, accountValue: String, dateFrom: Timestamp, dateTo: Timestamp)(implicit ctx: java.util.Properties, trxName: String): BigDecimal = {
    val accountOption: Option[Account] = ValidCombinationService.getValidCombiantion(alias)(ctx, trxName)
    if (accountOption.isDefined) {
      val account = accountOption.get
      val where = new StringBuilder()
      where.append(I_Fact_Acct.COLUMNNAME_PostingType).append("=? AND ")
      where.append(I_Fact_Acct.COLUMNNAME_AD_Org_ID).append("=? AND ")
      where.append(I_Fact_Acct.COLUMNNAME_AD_OrgTrx_ID).append("=? AND ")
      where.append(I_Fact_Acct.COLUMNNAME_C_Activity_ID).append("=? AND ")
      where.append(I_Fact_Acct.COLUMNNAME_C_Campaign_ID).append("=? AND ")
      where.append(I_Fact_Acct.COLUMNNAME_C_Project_ID).append("=? AND ")
      where.append(I_Fact_Acct.COLUMNNAME_UserElement1_ID).append("=? AND ")
      where.append(I_Fact_Acct.COLUMNNAME_UserElement2_ID).append("=? AND ")
      where.append(I_Fact_Acct.COLUMNNAME_User1_ID).append("=? AND ")
      where.append(I_Fact_Acct.COLUMNNAME_C_SalesRegion_ID).append("=? AND ")
      where.append(I_Fact_Acct.COLUMNNAME_User2_ID).append("=? AND ")
      where.append(I_Fact_Acct.COLUMNNAME_User3_ID).append("=? AND ")
      where.append(I_Fact_Acct.COLUMNNAME_User4_ID).append("=? AND ")
      where.append("trunc(")
      where.append(I_Fact_Acct.COLUMNNAME_DateAcct)
      where.append(")").append(" BETWEEN ").append("? AND ? AND ")
      where.append(" EXISTS (SELECT 1 FROM C_ElementValue ev , C_AcctSchema_Element ae WHERE ev.C_ElementValue_ID=Fact_Acct.Account_ID AND ")
      where.append("ev.C_Element_ID=ae.C_Element_ID AND ae.ElementType='AC' AND ae.C_AcctSchema_ID=? AND ev.IsActive='Y' AND ev.Value=?)")
      val balance = new Query(ctx, I_Fact_Acct.Table_Name, where.toString(), trxName)
        .setClient_ID()
        .setParameters(
          postingType,
          account.getAD_Org_ID.asInstanceOf[Integer],
          account.getAD_OrgTrx_ID.asInstanceOf[Integer],
          account.getC_Activity_ID.asInstanceOf[Integer],
          account.getC_Campaign_ID.asInstanceOf[Integer],
          account.getC_Project_ID.asInstanceOf[Integer],
          account.getUserElement1_ID.asInstanceOf[Integer],
          account.getUserElement2_ID.asInstanceOf[Integer],
          account.getUser1_ID.asInstanceOf[Integer],
          account.getC_SalesRegion_ID.asInstanceOf[Integer],
          account.getUser2_ID.asInstanceOf[Integer],
          account.getUser3_ID.asInstanceOf[Integer],
          account.getUser4_ID.asInstanceOf[Integer],
          dateFrom.asInstanceOf[Timestamp],
          dateTo.asInstanceOf[Timestamp],
          accountSchemaId.asInstanceOf[Integer],
          accountValue)
        .setClient_ID()
        .sum("acctbalance(Fact_Acct.Account_ID,AmtAcctDr , AmtAcctCr)")
      return balance

    }
    BigDecimal.ZERO
  }

  /**
    * Validate Available Budget
    *
    * @param document
    */
  def checkBudgetAvailable(document: PO): String = {
    var budgetErrorMessagesList = List[(String)]()
    val accountDateOption: Option[Timestamp] = Option(document.get_Value(I_Fact_Acct.COLUMNNAME_DateAcct).asInstanceOf[Timestamp])
    accountDateOption.foreach(accountDate => {
      val accountSchemas = MAcctSchema.getClientAcctSchema(document.getCtx, document.getAD_Client_ID)
      val periodOption = Option(MPeriod.get(document.getCtx, accountDateOption.get))
      var budgetAmountList = List[(String, BigDecimal)]()
      accountSchemas.foreach(accountSchema => {
        val sql = "SELECT ev.C_ElementValue_ID FROM C_ElementValue ev , C_AcctSchema_Element ae " +
          " WHERE ev.C_Element_ID=ae.C_Element_ID AND ae.ElementType='AC' AND ae.C_AcctSchema_ID=? " +
          " AND ev.IsActive='Y' AND ev.Value=?"
        val accountId = DB.getSQLValue(document.get_TrxName, sql, accountSchema.getC_AcctSchema_ID, BudgetToBeExercised)
        // Invoice
        if (document.get_TableName == I_C_Invoice.Table_Name) {
          val invoice = document.asInstanceOf[MInvoice]
          //Ignore Budget Valdiate  for Invoice reverse
          if (!invoice.isReversal) {
            invoice.getLines.toList.filter(invoiceLine => invoiceLine != null).foreach(invoiceLine => {
              if (invoiceLine.get_ColumnIndex(I_C_ValidCombination.COLUMNNAME_C_ValidCombination_ID) > 0) {
                val budgetKeyId = invoiceLine.get_ValueAsInt(I_C_ValidCombination.COLUMNNAME_C_ValidCombination_ID)
                if (budgetKeyId > 0) {
                  val budgetKeyOption = Option(new MAccount(document.getCtx, budgetKeyId, document.get_TrxName()))
                  val budgetKey = budgetKeyOption.get
                  //Validate only Budget To Be Exercised
                  if (budgetKey.getAccount_ID == accountId) {
                    val budgetValidationErrorList = validateBudgetKey(budgetKeyOption, invoice.getDocumentInfo, invoiceLine.getLine, invoiceLine.getLineTotalAmt)
                    if (budgetValidationErrorList.isEmpty) {

                      budgetAmountList = budgetAmountList ++ List(budgetKey.getAlias -> invoiceLine.getLineTotalAmt)
                    }
                    else
                      budgetErrorMessagesList = budgetErrorMessagesList ++ budgetValidationErrorList
                  }
                }
              }
            })
          }
        }
        // Journal
        if (I_GL_Journal.Table_Name == document.get_TableName) {
          val journal = document.asInstanceOf[MJournal];
          val journalLines = journal.getLines(true)
          journalLines.toList.filter(journalLine => journalLine != null).foreach(journalLine => {
            /*if ((X_GL_Journal.POSTINGTYPE_Budget == journal.getPostingType && journalLine.get_ColumnIndex("BudgetValidCombination_ID") > 0 && journalLine.getAccount_ID == accountId)
            ||   X_GL_Journal.POSTINGTYPE_Actual == journal.getPostingType && journalLine.get_ColumnIndex("BudgetValidCombination_ID") > 0)*/
            //Validate Actual to Be Exercised
            if (X_GL_Journal.POSTINGTYPE_Actual == journal.getPostingType && journalLine.get_ColumnIndex("BudgetValidCombination_ID") > 0) {
              val budgetKeyId = journalLine.get_ValueAsInt("BudgetValidCombination_ID")
              if (budgetKeyId > 0 && journalLine.getAmtAcctDr.signum() > 0) {
                val budgetKeyOption = Option(new MAccount(document.getCtx, budgetKeyId, document.get_TrxName()))
                val budgetKey = budgetKeyOption.get
                //Validate only Budget To Be Exercised
                //if (budgetKey.getAccount_ID == accountId) {
                val budgetValidationErrorList = validateBudgetKey(budgetKeyOption, journal.getDocumentInfo, journalLine.getLine, journalLine.getAmtAcctDr)
                if (budgetValidationErrorList.isEmpty) {
                  // Only validate for budget expense
                  budgetAmountList = budgetAmountList ++ List(budgetKey.getAlias -> journalLine.getAmtAcctDr)
                }
                else
                  budgetErrorMessagesList = budgetErrorMessagesList ++ budgetValidationErrorList
                //}
              }
            }
            //Validate Budget To Be Exercised
            if (X_GL_Journal.POSTINGTYPE_Budget == journal.getPostingType && journalLine.get_ColumnIndex("BudgetValidCombination_ID") > 0 && journalLine.getAccount_ID == accountId) {
              val budgetKeyId = journalLine.get_ValueAsInt("BudgetValidCombination_ID")
              if (budgetKeyId > 0 && journalLine.getAmtAcctCr.signum() > 0) {
                val budgetKeyOption = Option(new MAccount(document.getCtx, budgetKeyId, document.get_TrxName()))
                val budgetKey = budgetKeyOption.get
                //Validate only Budget To Be Exercised
                if (budgetKey.getAccount_ID == accountId) {
                  val budgetValidationErrorList = validateBudgetKey(budgetKeyOption, journal.getDocumentInfo, journalLine.getLine, journalLine.getAmtAcctCr)
                  if (budgetValidationErrorList.isEmpty) {
                    // Only validate for budget expense
                    budgetAmountList = budgetAmountList ++ List(budgetKey.getAlias -> journalLine.getAmtAcctCr)
                  }
                  else
                    budgetErrorMessagesList = budgetErrorMessagesList ++ budgetValidationErrorList
                }
              }
            }
          })
        }
        // Bank Statement
        if (document.get_TableName == I_C_BankStatement.Table_Name) {
          val bankStatement = document.asInstanceOf[MBankStatement]
          bankStatement.getLines(true).toList.filter(bankStatementLine => bankStatementLine != null).foreach(bankStatementLine => {
            if (bankStatementLine.get_ColumnIndex(I_C_ValidCombination.COLUMNNAME_C_ValidCombination_ID) > 0) {
              val budgetKeyId = bankStatementLine.get_ValueAsInt(I_C_ValidCombination.COLUMNNAME_C_ValidCombination_ID)
              if (budgetKeyId > 0) {
                val budgetKeyOption = Option(new MAccount(document.getCtx, budgetKeyId, document.get_TrxName()))
                val budgetKey = budgetKeyOption.get
                //Validate only Budget To Be Exercised
                if (budgetKey.getAccount_ID == accountId) {
                  val budgetValidationErrorList = validateBudgetKey(budgetKeyOption, bankStatement.getDocumentInfo, bankStatementLine.getLine, bankStatementLine.getChargeAmt.negate())
                  if (budgetValidationErrorList.isEmpty) {

                    budgetAmountList = budgetAmountList ++ List(budgetKey.getAlias -> bankStatementLine.getChargeAmt.negate())
                  }
                  else
                    budgetErrorMessagesList = budgetErrorMessagesList ++ budgetValidationErrorList
                }
              }
            }
          })
        }
        periodOption.foreach(period => {
          val budgetAmountByAlias = budgetAmountList.groupBy(budgetTuple => budgetTuple).flatMap(_._2)
          budgetAmountByAlias.foreach(budgetLine => {
            val balance = getBalanceByAlias(
              accountSchema.get_ID,
              budgetLine._1,
              X_Fact_Acct.POSTINGTYPE_Budget,
              BudgetToBeExercised, period.getStartDate,
              period.getEndDate)(document.getCtx,
              document.get_TrxName())

            val budgetKey = budgetLine._1
            val budgetAmount = budgetLine._2
            /*if (balance.signum() < 0)
            {
              val budgetErrorMessage = s"Presupuesto con disponibilidad negativa  ${period.getName} del ${period.getStartDate} " +
                s"hasta ${period.getEndDate} de la clave $budgetKey por ${budgetAmount} con disponibilidad ${balance}"
              budgetErrorMessagesList = budgetErrorMessagesList ++ List(budgetErrorMessage)
            }*/
            //else {
            if (balance.subtract(budgetAmount).signum() < 0) {
              val budgetErrorMessage = s"Insuficiencia presupuestaria para el periodo ${period.getName} del ${period.getStartDate} " +
                s"hasta ${period.getEndDate} de la clave $budgetKey por ${budgetAmount} con disponibilidad ${balance}"
              budgetErrorMessagesList = budgetErrorMessagesList ++ List(budgetErrorMessage)
              //}
            }
          })
        })

        return budgetErrorMessagesList.mkString("\n")
      })
    })
    return ""
  }

  /**
    * Validate Budget Key
    *
    * @param budgetKeyOption
    * @param documentInfo
    * @param line
    * @param amount
    * @return
    */
  def validateBudgetKey(budgetKeyOption: Option[MAccount], documentInfo: String, line: Int, amount: BigDecimal): List[(String)] = {
    var budgetErrorMessagesList = List[(String)]()
    if (budgetKeyOption.isDefined) {
      val budgetKey = budgetKeyOption.get
      if (budgetKey.getAlias == null) {
        val budgetErrorMessage = s"No existe clave presupuestal para clave ${budgetKey.toString} en la linea $line del documento $documentInfo  con monto : $amount"
        budgetErrorMessagesList = budgetErrorMessagesList ++ List(budgetErrorMessage)
      }
      if (budgetKey.getAccount.getValue != BudgetToBeExercised) {
        val budgetErrorMessage = s"No existe clave presupuestal para la cuenta ${budgetKey.getAccount.getName} en la linea $line del documento $documentInfo con monto : $amount"
        budgetErrorMessagesList = budgetErrorMessagesList ++ List(budgetErrorMessage)
      }
    }
    else {
      val budgetErrorMessage = s"No existe clave presupuestal en la linea $line del documento $documentInfo con el monto : $amount"
      budgetErrorMessagesList = budgetErrorMessagesList ++ List(budgetErrorMessage)
    }
    budgetErrorMessagesList
  }
}
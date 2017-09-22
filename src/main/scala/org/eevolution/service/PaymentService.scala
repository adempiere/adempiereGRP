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

import org.compiere.model._
import org.compiere.process.DocAction

/**
  * Payment service
  */
object PaymentService {

  /**
    * Validate Reverse
    * @param payment
    * @return
    */
  def validateReverse(payment: MPayment): String =
  {
    val whereClausePaySelection = new StringBuilder
    whereClausePaySelection.append(I_C_AllocationLine.COLUMNNAME_C_Invoice_ID).append("=?")
    val allocationLine : MAllocationLine = new Query(payment.getCtx, I_C_AllocationLine.Table_Name, whereClausePaySelection.toString() , payment.get_TrxName())
      .setClient_ID()
      .setParameters(payment.get_ID.asInstanceOf[Integer])
      .first()

    if (allocationLine != null && allocationLine.get_ID > 0 ) {
      payment.setDocAction(DocAction.ACTION_Close)
      return s"@C_Payment_ID@ @IsAllocated@ ${allocationLine.getParent.getDocumentInfo}"
    }

    val whereClauseBankStatement =  new StringBuilder
    whereClauseBankStatement.append(I_C_BankStatementLine.COLUMNNAME_C_Payment_ID).append("=?")
    val bankStatementLine : MBankStatementLine = new Query(payment.getCtx , I_C_BankStatementLine.Table_Name , whereClauseBankStatement.toString(), payment.get_TrxName)
      .setClient_ID
      .setParameters(payment.getC_Payment_ID.asInstanceOf[Integer])
      .first()
    if (bankStatementLine != null && bankStatementLine.get_ID > 0 )
      return s"@C_Payment_ID@ @IsReconciled@ @C_BankStatement_ID@ ${bankStatementLine.getParent.getDocumentInfo}"
    withoutErrors
  }
}

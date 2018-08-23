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

package org.eevolution

import org.compiere.model._

/**
  * Service package
  * Created by e-Evolution on 22/04/17.
  */
package object service {

  type Tax = MTax
  type TaxAccount = X_C_Tax_Acct
  type Requisition = MRequisition
  type RequisitionLine = MRequisitionLine
  type Order = MOrder
  type OrderLine = MOrderLine
  type Invoice = MInvoice
  type Journal = MJournal
  type Allocation = MAllocationHdr
  type InvoiceLine = MInvoiceLine
  type AccountingFact = MFactAcct
  type AccountSchema = MAcctSchema
  type Account = MAccount
  type PaySelection = MPaySelection
  type ValidCombinaton = X_C_ValidCombination

  val BudgetToBeExercised = "822"
  val BudgetApproval = "798008220"
  val BudgetReserved = "798008240"
  val BudgetCommittee = "824"
  val BudgetAccrual = "825"
  val BudgetExercised = "826"
  val BudgetPaid = "827"
  val BudgetExecute = "812"
  val withoutErrors = ""
}

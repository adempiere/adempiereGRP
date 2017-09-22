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

import org.compiere.model._
import org.compiere.util.{CLogger, Env}

import scala.collection.immutable.HashMap

/**
 * Created by eEvolution author Victor Perez <victor.perez@e-evolution.com> on 23/07/15.
 */
class GRPBudgetValidator extends ModelValidator {
  val logger: CLogger = CLogger.getCLogger (getClass)
  val clientId: Int = -1
  val noError = ""

  def initialize (engine: ModelValidationEngine, client: MClient): Unit = {
    engine.addDocValidate(I_M_Requisition.Table_Name, this)
    engine.addDocValidate(I_C_Order.Table_Name, this)
    engine.addDocValidate(I_C_Invoice.Table_Name, this)
    engine.addDocValidate(I_GL_Journal.Table_Name, this)
    engine.addDocValidate(I_C_BankStatement.Table_Name , this)
  }


  def login (orgId: Int, roleId: Int, userId: Int): String = {
      noError
  }
  def getAD_Client_ID: Int = {
    val clientId = Env.getAD_Client_ID(Env.getCtx)
    clientId
  }

  def modelChange (po: PO, typeEvent: Int): String = {
    noError
  }

  def docValidate(po: PO, timing: Int): String = po match {
    case _ if (po.isInstanceOf[MRequisition]  && ModelValidator.TIMING_BEFORE_COMPLETE == timing) => {
     val requisition = po.asInstanceOf[MRequisition]
     val budgetElements = new HashMap[Int , BigDecimal]
      requisition
        .getLines
        .filter(requisitionLine => Option(requisitionLine.get_ValueAsString(I_C_ValidCombination.COLUMNNAME_C_ValidCombination_ID)).isDefined)
        .foreach(requisitionLine => {
          val budgetKey = requisitionLine.get_ValueAsInt(I_C_ValidCombination.COLUMNNAME_C_ValidCombination_ID)
          val budgetAmount = requisitionLine.getLineNetAmt
          budgetElements.updated(budgetKey, budgetElements.get(budgetKey).get + budgetAmount)
        })
      noError
    }
    case _ => noError
  }
}
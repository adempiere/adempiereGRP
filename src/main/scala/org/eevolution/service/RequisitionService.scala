/**
  * Copyright (C) 2003-2018, e-Evolution Consultants S.A. , http://www.e-evolution.com
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

import org.compiere.model.{MOrderLine, MRequisitionLine}

object RequisitionService {

  /**
    * Validate After New Requisition Line
    *
    * @param requisitionLine
    * @return
    */
  def afterNewRequisitionLine(requisitionLine: MRequisitionLine): String = {
    if (ValidCombinationService.existsBudgetValidCombination(requisitionLine)) {
      val accountId =  ValidCombinationService.getBudgetValidCombination(requisitionLine);
      if (accountId > 0) ValidCombinationService.fillDimension(accountId, requisitionLine)(requisitionLine.getCtx, requisitionLine.get_TrxName())
    }
    return withoutErrors
  }

  def beforeChangeRequisitionLine(requisitionLine: MRequisitionLine) : String = {
    val orderLineOption = Option(requisitionLine.getC_OrderLine().asInstanceOf[MOrderLine])
    orderLineOption.filter(orderLine => orderLine.get_ID() > 0).foreach(orderLine => {
      if (ValidCombinationService.existsBudgetValidCombination(requisitionLine)) {
        val accountId = ValidCombinationService.getBudgetValidCombination(requisitionLine);
        if (accountId > 0) {
          if (ValidCombinationService.existsBudgetValidCombination(orderLine)) {
            ValidCombinationService.setBudgetValidCombination(orderLine, accountId)
            ValidCombinationService.fillDimension(accountId, orderLine)(requisitionLine.getCtx, requisitionLine.get_TrxName())
          }
        }
      }
    })
    return withoutErrors
  }

}

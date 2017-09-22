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

/**
  * Bank Statement service
  * Created by e-Evolution on 07/06/17.
  */
object BankStatementService {

  /**
    * Update Statement Line
    * @param po
    * @return
    */
  def updateStatementLine(po: PO): String = {
    po.set_ValueOfColumn(I_C_BankStatementLine.COLUMNNAME_C_BPartner_ID, -1)
    po.saveEx(po.get_TrxName())
    withoutErrors
  }

  /**
    * Reserve Bank Statement
    * @param instane
    */
  def reverse(instane: PO): Unit = {
  }
}

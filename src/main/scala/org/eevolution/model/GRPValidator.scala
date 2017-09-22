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
import org.compiere.util.{CLogger, DB, Env, Ini}
import org.eevolution.service.{FactAccountService, ValidCombinationService}

/**
 * Created by eEvolution author Victor Perez <victor.perez@e-evolution.com> on 23/07/15.
 */
class GRPValidator extends ModelValidator{
  val logger: CLogger = CLogger.getCLogger (getClass)
  val clientId: Int = -1
  val noError = ""

  def initialize (engine: ModelValidationEngine, client: MClient): Unit = {
    engine.addModelChange(I_C_Invoice.Table_Name, this)
  }


  def login (orgId: Int, roleId: Int, userId: Int): String = {
      return noError
  }
  def getAD_Client_ID: Int = {
    val clientId = Env.getAD_Client_ID(Env.getCtx)
    clientId
  }

  def modelChange (po: PO, typeEvent: Int): String = po match {
    case _ => noError
  }

  def docValidate(po: PO, timing: Int): String = po match {
    case _ => noError
  }
}
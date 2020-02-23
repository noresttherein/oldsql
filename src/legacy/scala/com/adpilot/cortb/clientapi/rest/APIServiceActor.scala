package com.adpilot.cortb.clientapi.rest

import com.adpilot.cortb.clientapi.prototype.repository.APIRepository


class APIServiceActor(val repository :APIRepository) extends SprayServiceActor with APIService

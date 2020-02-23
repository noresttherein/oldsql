package com.hcore.clientapi.rest

import com.hcore.clientapi.repository.APIRepository


class APIServiceActor(val repository :APIRepository) extends SprayServiceActor with APIService

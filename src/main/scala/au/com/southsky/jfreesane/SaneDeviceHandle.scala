// Copyright 2012 Google Inc. All Rights Reserved.
package au.com.southsky.jfreesane

import com.google.common.base.Strings

/**
  * Represents a SANE device handle. This class is used solely to implement JFreeSane, therefore it
  * is not visible to others.
  *
  * Constructs a new {@code SaneDeviceHandle}. This will typically be done in response to a call to
  * {@link SaneDevice#open}.
  *
  * @param status
  * the status code returned by the SANE daemon in response to the { @code open} request
  * @param handle
  * the handle assigned to the device by the SANE daemon
  * @param resource
  * the name of the resource for authentication purposes, or { @code null} if authorization
  * is not required to use the resource
  * @author sjr@google.com (James Ring)
  */
class SaneDeviceHandle private[jfreesane](val status: SaneWord, val handle: SaneWord, val resource: String) {
  /**
    * Returns the status that was provided by SANE when creating the device handle in response to an
    * {@code open} request.
    *
    * @see SaneDevice#open
    */
  def getStatus: SaneWord = status

  /**
    * Returns the handle that was assigned to the device by SANE in response to the {@code open}
    * request.
    *
    * @see SaneDevice#open
    */
  def getHandle: SaneWord = handle

  /**
    * Returns the name of the resource for authorization purposes. If authorization is not required,
    * this returns {@code null}.
    */
  def getResource: String = resource

  /**
    * Returns {@code true} if authorization is required to use the device represented by this handle.
    */
  def isAuthorizationRequired: Boolean = !Strings.isNullOrEmpty(resource)
}
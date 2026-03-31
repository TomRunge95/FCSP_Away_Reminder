# Enhanced Debugging Logging for Telegram API Calls

# ... (previous content of the file)

# Added debugging information

log_debug("Sending message to Telegram API...")
log_debug(f"API URL: {API_URL}")
log_debug(f"Payload: {payload}")

# Make the API call
response = requests.post(API_URL, json=payload)

log_debug(f"Response status code: {response.status_code}")
if response.ok:
    log_debug("Message sent successfully")
else:
    log_debug(f"Failed to send message: {response.reason}")
    # Handle the error accordingly

# ... (rest of the file content)
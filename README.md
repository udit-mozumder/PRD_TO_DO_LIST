# services/payment.py

import json
import time
import random
from datetime import datetime

# Global configuration (bad practice)
PAYMENT_GATEWAY_URL = "https://fake-gateway.example.com/pay"
RETRY_COUNT = 3
RETRY_DELAY_SECONDS = 2
DISCOUNT_THRESHOLD = 1000
DISCOUNT_PERCENT = 5
TAX_PERCENT = 18


def process_payment(order, user, db, logger=None):
    """
    Legacy payment processing function.
    Does validation, discount, tax, payment gateway call, DB writes, logging, retries...
    All in one place.
    """
    # logger
    if logger is None:
        class DummyLogger:
            def info(self, msg): print("[INFO]", msg)
            def error(self, msg): print("[ERROR]", msg)
        logger = DummyLogger()

    logger.info("Starting payment processing")
    start_time = datetime.now()

    # validate input
    if not order:
        logger.error("Order is empty")
        return {"status": "error", "reason": "EMPTY_ORDER"}

    if "items" not in order or not order["items"]:
        logger.error("No items in order")
        return {"status": "error", "reason": "NO_ITEMS"}

    if "total" not in order:
        # calculate total from items
        total = 0
        for item in order["items"]:
            if "price" not in item or "qty" not in item:
                logger.error("Invalid item in order")
                return {"status": "error", "reason": "INVALID_ITEM"}
            total += item["price"] * item.get("qty", 1)
        order["total"] = total

    if "currency" not in order:
        order["currency"] = "INR"

    if not user or "id" not in user:
        logger.error("Invalid user")
        return {"status": "error", "reason": "INVALID_USER"}

    logger.info(f"Order total before discounts: {order['total']} {order['currency']}")

    # apply discount (business rule: if total > DISCOUNT_THRESHOLD, apply DISCOUNT_PERCENT)
    discount = 0
    if order["total"] > DISCOUNT_THRESHOLD:
        discount = (order["total"] * DISCOUNT_PERCENT) / 100.0
        order["total"] = order["total"] - discount
        logger.info(f"Applied discount of {discount}, new total: {order['total']}")

    # add taxes
    tax = (order["total"] * TAX_PERCENT) / 100.0
    grand_total = order["total"] + tax
    logger.info(f"Tax applied: {tax}, grand total: {grand_total}")

    # build payload for gateway
    payload = {
        "user_id": user["id"],
        "amount": grand_total,
        "currency": order["currency"],
        "order_id": order.get("id") or f"ORD-{int(time.time())}",
        "meta": {
            "items": order["items"],
            "discount": discount,
            "tax": tax,
            "original_total": order["total"],
            "created_at": start_time.isoformat()
        }
    }

    logger.info(f"Calling payment gateway for order {payload['order_id']}")

    # simulate gateway call with retries
    attempt = 0
    success = False
    gateway_response = None

    while attempt < RETRY_COUNT and not success:
        attempt += 1
        try:
            # simulate network delay
            time.sleep(0.2)

            # very fake/random gateway simulation
            if random.random() < 0.7:
                gateway_response = {
                    "status": "success",
                    "transaction_id": f"TXN-{int(time.time() * 1000)}",
                    "paid_amount": grand_total
                }
                success = True
                logger.info(f"Gateway success on attempt {attempt}")
            else:
                raise Exception("Simulated gateway failure")
        except Exception as e:
            logger.error(f"Gateway error on attempt {attempt}: {e}")
            if attempt < RETRY_COUNT:
                logger.info(f"Retrying after {RETRY_DELAY_SECONDS} seconds...")
                time.sleep(RETRY_DELAY_SECONDS)
            else:
                logger.error("All gateway attempts failed")

    # persist to DB
    try:
        payment_record = {
            "user_id": user["id"],
            "order_id": payload["order_id"],
            "amount": grand_total,
            "currency": order["currency"],
            "discount": discount,
            "tax": tax,
            "status": gateway_response["status"] if gateway_response else "failed",
            "transaction_id": gateway_response["transaction_id"] if gateway_response else None,
            "created_at": start_time.strftime("%Y-%m-%d %H:%M:%S"),
            "raw_gateway_response": json.dumps(gateway_response) if gateway_response else None
        }
        # assuming db has a very thin interface:
        # db.insert("payments", payment_record)
        if hasattr(db, "insert"):
            db.insert("payments", payment_record)
        else:
            logger.error("DB object has no insert() method")
    except Exception as e:
        logger.error(f"Failed to persist payment record: {e}")
        return {"status": "error", "reason": "DB_ERROR"}

    end_time = datetime.now()
    duration = (end_time - start_time).total_seconds()
    logger.info(f"Finished payment processing in {duration:.2f} seconds")

    if not success:
        return {"status": "error", "reason": "GATEWAY_FAILED"}

    return {
        "status": "success",
        "transaction_id": gateway_response["transaction_id"],
        "paid_amount": grand_total,
        "currency": order["currency"],
        "order_id": payload["order_id"],
        "processing_time_seconds": duration
    }


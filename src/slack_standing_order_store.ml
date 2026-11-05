(** Stable standing-order persistence facade. Children depend on Base, never on
    this facade, so lifecycle transactions and the clock remain shared. *)

include Slack_standing_order_store_base
include Slack_standing_order_card_store
include Slack_standing_order_replace_store
include Slack_standing_order_fire_store

Verboice::Application.routes.draw do
  resources :channels do
    resources :queued_calls
    member do
      get :call
    end
  end

  root :to => "applications#index"

  devise_for :accounts

  resources :applications
  resources :call_logs do
    get :progress, :on => :member
  end

  match "api/call" => "api#call", :as => :api_call
  match "api/call/:id/state" => "api#call_state"
  post  "api/channels" => "api_channels#create"
  delete "api/channels/:name" => "api_channels#destroy"
end

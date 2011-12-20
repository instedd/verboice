Verboice::Application.routes.draw do
  resources :channels do
    resources :queued_calls
    member do
      get :call
    end
  end

  match '/' => 'home#index',  :as => 'home'

  devise_for :accounts

  resources :applications
  resources :call_logs do
    get :progress, :on => :member
  end

  match "api/call" => "api#call", :as => :api_call
  match "api/call/:id/state" => "api#call_state"
  match 'api/call/:id/redirect' => 'api#call_redirect'
  post  "api/channels" => "api_channels#create"
  delete "api/channels/:name" => "api_channels#destroy"
  root :to => 'home#index'
  get 'terms_and_conditions', :to => redirect('/')

end
